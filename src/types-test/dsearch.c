/* Nsp
 * Copyright (C) 2004-2009 Bruno Pincon  Bruno.Pincon@iecn.u-nancy.fr
 *              Jean-Philippe Chancelier Enpc/Cermics jpc@cermics.enpc.fr 
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Graphic library
 * jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h> 

#include "nsp/matrix-in.h"
#include "nsp/smatrix-in.h"
#include "nsp/bmatrix-in.h"
#include "nsp/seval.h"
#include "nsp/gsort-p.h"
#include "nsp/imatrix.h"

#define CLOSED_LEFT 0
#define CLOSED_RIGHT 1

void nsp_bsearchc(const double x[], int m,const double val[], int n, 
		  int indx[], int occ[], int *info, int interval_flag);

void nsp_bsearchd(const double x[], int m,const double val[], int n,
		  int indx[], int occ[], int *info);

void nsp_bsearchc_for_strings(char **x, int m, char **val, int n, 
			      int indx[], int occ[], int *info, int interval_flag);

void nsp_bsearchd_for_strings(char **x, int m, char **val, int n,
			      int *indx, int *occ, int *info);

void nsp_bsearchc_for_IMat(const NspIMatrix *x, const NspIMatrix *val,
			   int indx[], int occ[], int *info, int interval_flag);

void nsp_bsearchd_for_IMat(const NspIMatrix *x, const NspIMatrix *val,
			   int indx[], int occ[], int *info);

/*
 * bsearch interface  
 * jpc / bp
 *
 *       [ind , occ, info] = bsearch(x, val, match= 'i'|'v', interval='[--)'|'(--]', ind_type='double'|'int')
 *        
 * X and val must be both real or strings vectors (says of length m for X and n for val ), 
 *
 * ind is a vector with the same shape as x 
 * occ is a vector with the same shape as val (but with n-1 components in the case match='i')
 * info is a scalar
 */

int int_bsearch(Stack stack, int rhs, int opt, int lhs)
{
  int info, m_occ, n_occ, m_x, n_x, mn_val, i;
  char *interval_type=NULL,*match_type=NULL, *ind_type=NULL;
  int interval_flag = CLOSED_LEFT, *ind=NULL; 
  char match_flag='i', type_flag='d', type_ind='d';
  Boolean assume_sorted = FALSE, in_order = TRUE;
  nsp_option opts[] ={{ "interval",string,NULLOBJ,-1},
		      { "match",string,NULLOBJ,-1},
		      { "assume_sorted",s_bool,NULLOBJ,-1},
		      { "ind_type",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspMatrix *x=NULLMAT, *val=NULLMAT, *occ=NULLMAT;
  NspSMatrix *xstr=NULLSMAT, *valstr=NULLSMAT;
  NspIMatrix *xi=NULLIMAT, *vali=NULLIMAT;
  NspObject *Ind=NULLOBJ;

  CheckStdRhs(2,2);
  CheckLhs(0,3);

  if ( IsMatObj(stack,1) ) 
    {
      if ( (x=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      if ( (val=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
      CheckVector(NspFname(stack),2,val);
      type_flag = 'd'; m_x = x->m; n_x = x->n;
      m_occ = val->m;  n_occ = val->n; mn_val = val->mn;
    }
  else if ( IsSMatObj(stack,1) ) 
    {
      if ( (xstr=GetSMat(stack,1)) == NULLSMAT ) return RET_BUG;
      if ( (valstr=GetSMat(stack,2)) == NULLSMAT ) return RET_BUG;
      CheckVector(NspFname(stack),2,valstr);
      type_flag = 's'; m_x = xstr->m; n_x = xstr->n;
      m_occ = valstr->m; n_occ = valstr->n; mn_val = valstr->mn;
    }
  else if ( IsIMatObj(stack,1) ) 
    {
      if ( (xi=GetIMat(stack,1)) == NULLIMAT ) return RET_BUG;
      if ( (vali=GetIMat(stack,2)) == NULLIMAT ) return RET_BUG;
      CheckVector(NspFname(stack),2,vali);
      if ( xi->itype != vali->itype ) 
	{
	  Scierror("Error: arguments must have the same integer type in function %s\n",NspFname(stack));
	  return RET_BUG;
	}
      type_flag = 'i'; m_x = xi->m; n_x = xi->n;
      m_occ = vali->m;  n_occ = vali->n; mn_val = vali->mn;
    }
  else
    {
      Scierror("Error: first argument must be of type real, integer or string in function %s\n",NspFname(stack));
      return RET_BUG;
    }

  if ( get_optional_args(stack, rhs, opt, opts, &interval_type, &match_type, &assume_sorted, &ind_type) == FAIL) 
    return RET_BUG;

  if ( match_type != NULL )
    {
      if ( strcmp(match_type,"i") == 0 )
	match_flag = 'i';
      else if (strcmp(match_type,"v") == 0 )
	match_flag = 'v';
      else
	{
	  Scierror("Error: bad match value only 'i' or 'v' are allowed in function %s\n",NspFname(stack));
	  return RET_BUG;
	}
    }

  if ( interval_type != NULL )
    {
      if (match_flag == 'v' )
	Sciprintf("Warning: with match='v' the interval option is not used in function %s\n",NspFname(stack));
      if ( strcmp(interval_type,"[--)") == 0 )
	interval_flag = CLOSED_LEFT;
      else if ( strcmp(interval_type,"(--]") == 0 )
	interval_flag = CLOSED_RIGHT;
      else
	{
	  Scierror("Error: bad interval value only '[--)' or '(--]' are allowed in function %s\n",NspFname(stack));
	  return RET_BUG;
	}
    }

  if ( ind_type != NULL )
    {
      if ( strcmp(ind_type,"double") == 0 )
	type_ind = 'd';
      else if ( strcmp(ind_type,"int") == 0 )
	type_ind = 'i';
      else
	{
	  Scierror("Error: unsupported ind_type in function %s (should be 'double' or 'int')\n",NspFname(stack));
	  return RET_BUG;
	}
    }

  if ( (Ind = nsp_alloc_mat_or_imat(m_x, n_x, type_ind, &ind)) == NULLOBJ )
    return RET_BUG;
      
  if ( match_flag == 'i' )
    {
      m_occ = Max(m_occ -1, 1); 
      n_occ = Max(n_occ -1, 1);
    }

  if ( (occ=nsp_matrix_create(NVOID,'r',m_occ,n_occ)) == NULLMAT ) return RET_BUG;

  if ( ! assume_sorted )      /* Check that val is increasing */
    {
      if ( type_flag == 'd' )
	{
	  for ( i = 0 ; i < mn_val-1 && in_order ; i++ ) 
	    in_order = val->R[i] < val->R[i+1];
	}
      else if ( type_flag == 's' )
	{
	  for ( i = 0 ; i < mn_val-1 && in_order ; i++ ) 
	    if ( strcmp(valstr->S[i],valstr->S[i+1]) >= 0 )
	      in_order = FALSE;
	}
      else if ( type_flag == 'i' )
	{
#define IMAT_ORDER_TEST(name,type,arg)                     \
          for ( i = 0 ; i < mn_val-1 && in_order ; i++ )   \
	    in_order = vali->name[i] < vali->name[i+1];        \
          break;                    
          NSP_ITYPE_SWITCH(vali->itype,IMAT_ORDER_TEST,"");
#undef IMAT_ORDER_TEST
	}

      if ( ! in_order )
	{
	  Scierror("Error: second argument in function %s\n",NspFname(stack));
	  Scierror("\tis not strictly increasing \n");
	  goto err;
	}
    }

  if ( match_flag == 'i' ) 
    {
      if ( mn_val <= 1 ) 
	{ 
	  Scierror("%s: second argument should be of size > 1 when match='i' \n",NspFname(stack));
	  goto err;
	}
    }
  else
    {
      if ( mn_val < 1 ) 
	{ 
	  Scierror("%s: second argument should be of size >= 1 when match='v' \n",NspFname(stack));
	  goto err;
	}
    }

  if ( type_flag == 'd' )
    {
      if ( match_flag == 'i' ) 
	nsp_bsearchc(x->R, x->mn, val->R, val->mn-1, ind, occ->I, &info, interval_flag);
      else 
	nsp_bsearchd(x->R, x->mn, val->R, val->mn, ind, occ->I, &info);
    }

  else if ( type_flag == 's' )
    {
      if ( match_flag == 'i' ) 
	nsp_bsearchc_for_strings(xstr->S, xstr->mn, valstr->S, mn_val-1, ind, occ->I, &info, interval_flag);
      else 
	nsp_bsearchd_for_strings(xstr->S, xstr->mn, valstr->S, mn_val, ind, occ->I, &info);
    }

  else if ( type_flag == 'i' )
    {
      if ( match_flag == 'i' ) 
	nsp_bsearchc_for_IMat(xi, vali, ind, occ->I, &info, interval_flag);
      else 
	nsp_bsearchd_for_IMat(xi, vali, ind, occ->I, &info);
    }

  if ( type_ind == 'd' )
    Ind = (NspObject *) Mat2double((NspMatrix *) Ind);

  MoveObj(stack,1,Ind);
  if ( lhs >= 2)
    {
      occ->convert = 'i';
      occ = Mat2double(occ);
      MoveObj(stack,2,NSP_OBJECT(occ));
    }
  else 
    {
      nsp_matrix_destroy(occ);
    }
  if ( lhs == 3 ) 
    {
      if ( nsp_move_double(stack,3,(double) info) == FAIL) return RET_BUG;
    }
  return Max(lhs,1);

 err:
  nsp_matrix_destroy(occ);
  nsp_object_destroy(&Ind);
  return RET_BUG;
}


/*     PURPOSE 
 *        val[n+1] being an array (with strict increasing order and n >=1) 
 *        representing n intervals, this routine, by the mean of a 
 *        dichotomic search, computes : 
 *
 *           a/ for each x[i] its interval number stored in indx[i] : 
 *                  I(j) = [val[j-1], val[j] )  j=1,...,n-1 
 *                  I(n) = [val[n-1], val[n] ]
 *               indx[i] = 0 if  x[i] is not in [val(1),val(n+1)] 
 *
 *           b/ the number of points falling in the interval j : 
 *
 *              occ[j] = # { x[i] such that x[i] in I(j) } 
 *
 *     PARAMETERS 
 *        inputs : 
 *           m         int 
 *           x[m]      double float array 
 *           n         int 
 *           val[n+1]  double float array (val[0] < val[1] < ....) 
 *        outputs 
 *           indx[m] int array 
 *           occ[n]  int array 
 *           info    int (number of x[i] not in [val[0], val[n]]) 
 *
 *     AUTHOR 
 *        Bruno Pincon 
 *        C converted by J.Ph Chancelier (f2c + hand polished ). 
 */

void nsp_bsearchc(const double x[], int m,const double val[], int n, 
		  int indx[], int occ[], int *info, int interval_flag)
{
  int  i, j, j1, j2;
 
  for (j = 0; j < n; j++ ) occ[j]= 0;
  *info = 0;

  for (i = 0; i < m ; i++ ) 
    {
      if (val[0] <= x[i] && x[i] <= val[n]) 
	{
	  /* x[i] is in [val(0),val(n)]:  find j such that x[i] is in I(j) */
	  j1 = 0; j2 = n;
	  if (interval_flag == CLOSED_LEFT )
	    while(j2 - j1 > 1) 
	      {
		j = (j1 + j2) / 2;
		if (x[i] < val[j]) j2 = j; else j1 = j;
	      }
	  else   /* (interval_flag == CLOSED_RIGHT ) */
	    while(j2 - j1 > 1) 
	      {
		j = (j1 + j2) / 2;
		if (x[i] > val[j]) j1 = j; else j2 = j;
	      }
	  ++occ[j1]; indx[i] = j2;
	} 
      else   /* x[i] is not in [val(0), val(n)] */ 
	{
	  ++(*info);
	  indx[i] = 0;
	}
    }
}

/**
 * nsp_bsearchd:
 * @x: array of double.
 * @m: size of @x
 * @val: a strictly increasing array of double.
 * @n: size of @val
 * @indx: int array
 * @occ: int array
 * @info: int pointer.
 * 
 * @val being a strictly increasing array of size @n,
 * this routines by the mean of a binary search computes
 * a/ the number of occurences (@occ[j]) of each value @val[j] 
 *    in the array @x i.e @occ[j] = card{ x[i] such that x[i] == val[j] } 
 *
 * b/ the array @indx : if x[i] == val[j] then indx[i] = j+1
 *    (if x[i] is not in val then indx[i] = 0) 
 * the number of values of @x not matching an entry in @val are 
 * returned in @info.
 *
 * Author: Bruno Pincon.
 * 
 **/

void nsp_bsearchd(const double *x, int m,const double *val, int n,
		  int *indx, int *occ, int *info)
{
  int i, j, j1, j2;

  for (j = 0; j < n ; j++ ) occ[j]= 0;
  *info = 0;

  for (i = 0 ; i < m ; i++) 
    {
      if (val[0] <= x[i] && x[i] <= val[n-1]) 
	{
	  /* find j such that x[i] = val(j) by a dicho search */
	  j1 = 0;
	  j2 = n-1;
	  while(j2 - j1 > 1) 
	    {
	      j = (j1 + j2) / 2;
	      if (x[i] < val[j]) j2 = j; else j1 = j;
	    }
	  /*  here we know that val(j1) <= x[i] <= val(j2)  with j2 = j1 + 1
	   *  (in fact we have exactly  val(j1) <= x[i] < val(j2) if j2 < n) 
	   */
	  if (x[i] == val[j1]) 
	    {
	      ++occ[j1];
	      indx[i] = j1+1;
	    } 
	  else if (x[i] == val[j2]) /* (note: this case may happen only for j2=n-1) */
	    {
	      ++occ[j2];
	      indx[i] = j2+1;
	    } 
	  else  /* x[i] is not in {val(1), val(2),..., val(n)} */ 
	    {
	      ++(*info);
	      indx[i] = 0;
	    }
	} 
      else    /* x[i] is not in {val(1), val(2),..., val(n)} */ 
	{
	  ++(*info);
	  indx[i] = 0;
	}
    }
} 


void nsp_bsearchc_for_strings(char **x, int m, char **val, int n, 
			      int indx[], int occ[], int *info, int interval_flag)
{
  int  i, j, j1, j2;
 
  for (j = 0; j < n; j++ ) occ[j]= 0;
  *info = 0;

  for (i = 0; i < m ; i++ ) 
    {
      if ( strcmp(val[0],x[i]) <= 0 &&  strcmp(x[i],val[n]) <= 0 ) 
	{
	  /* x[i] is in [val(0),val(n)] :  find j such that x[i] is in I(j) */
	  j1 = 0; j2 = n;
	  if (interval_flag == CLOSED_LEFT )
	    while(j2 - j1 > 1) 
	      {
		j = (j1 + j2) / 2;
		if ( strcmp(x[i],val[j]) < 0 ) j2 = j; else j1 = j;
	      }
	  else   /* (interval_flag == CLOSED_RIGHT ) */
	    while(j2 - j1 > 1) 
	      {
		j = (j1 + j2) / 2;
		if ( strcmp(x[i],val[j]) > 0 ) j1 = j; else j2 = j;
	      }
	  ++occ[j1]; indx[i] = j2 ;
	} 
      else  /* x[i] is not in [val(0), val(n)] */ 
	{
	  ++(*info);
	  indx[i] = 0;
	}
    }
}


void nsp_bsearchd_for_strings(char **x, int m, char **val, int n,
			      int *indx, int *occ, int *info)
{
  int i, j, j1, j2;

  for (j = 0; j < n ; j++ ) occ[j]= 0;
  *info = 0;

  for (i = 0 ; i < m ; i++) 
    {
      if ( strcmp(val[0],x[i]) <= 0  &&  strcmp(x[i],val[n-1]) <= 0 ) 
	{
	  /* find j such that x[i] = val(j) by a dicho search */
	  j1 = 0;
	  j2 = n-1;
	  while(j2 - j1 > 1) 
	    {
	      j = (j1 + j2) / 2;
	      if ( strcmp(x[i],val[j]) < 0 ) j2 = j; else j1 = j;
	    }
	  /*  here we know that val(j1) <= x[i] <= val(j2)  with j2 = j1 + 1
	   *  (in fact we have exactly  val(j1) <= x[i] < val(j2) if j2 < n-1) 
	   */
	  if ( strcmp(x[i],val[j1]) == 0 ) 
	    {
	      ++occ[j1];
	      indx[i] = j1+1;
	    } 
	  else if ( j2 == n-1  &&  strcmp(x[i],val[j2]) == 0 )  /* this case may happen only for j2=n-1 */
	    {
	      ++occ[j2];
	      indx[i] = j2+1;
	    } 
	  else /* x[i] is not in {val(1), val(2),..., val(n)} */ 
	    {
	      ++(*info);
	      indx[i] = 0;
	    }
	} 
      else     /* x[i] is not in {val(1), val(2),..., val(n)} */ 
	{
	  ++(*info);
	  indx[i] = 0;
	}
    }
} 




/*     PURPOSE 
 *        val[0:n] being an array (with strict increasing order and n >=1) 
 *        representing n intervals, this routine, by the mean of a 
 *        binary search, computes : 
 *
 *           a/ for each x[i] its interval number stored in indx[i] : 
 *                  I(j) = [val[j-1], val[j] )  j=1,...,n-1 
 *                  I(n) = [val[n-1], val[n] ]
 *               indx[i] = 0 if  x[i] is not in [val(1),val(n+1)] 
 *
 *           b/ the number of points falling in the interval j : 
 *
 *              occ[j] = # { x[i] such that x[i] in I(j) } 
 *
 *     PARAMETERS 
 *        inputs : 
 *           x         IMat array 
 *           val       IMat array (val[0] < val[1] < ....< Val[n]) 
 *           x and val should be of same integer type
 *        outputs 
 *           indx[m] int array 
 *           occ[n]  int array 
 *           info    int (number of x[i] not in [val[0], val[n]]) 
 *
 *     AUTHOR 
 *        Bruno Pincon 
 */

void nsp_bsearchc_for_IMat(const NspIMatrix *x, const NspIMatrix *val,
		  int indx[], int occ[], int *info, int interval_flag)
{
  int  i, j, j1, j2;
  int m=x->mn, n=val->mn-1;
 
  for (j = 0; j < n; j++ ) occ[j]= 0;
  *info = 0;

#define IMAT_BSEARCHC(name,type,arg)                                    \
  for (i = 0; i < m ; i++ )						\
    {									\
       if (val->name[0] <= x->name[i] && x->name[i] <= val->name[n])    \
         {                                                              \
	   /* x[i] in [val(0),val(n)]: find j such that x[i] in I(j) */ \
	    j1 = 0; j2 = n;						\
	    if (interval_flag == CLOSED_LEFT )				\
	      while(j2 - j1 > 1)					\
		{							\
		  j = (j1 + j2) / 2;					\
		  if (x->name[i] < val->name[j]) j2 = j; else j1 = j;	\
		}							\
	    else   /* (interval_flag == CLOSED_RIGHT ) */		\
	      while(j2 - j1 > 1)					\
		{							\
		  j = (j1 + j2) / 2;					\
		  if (x->name[i] > val->name[j]) j1 = j; else j2 = j;	\
		}							\
	    ++occ[j1]; indx[i] = j2;					\
	  }								\
       else   /* x[i] is not in [val(0), val(n)] */			\
	  {								\
	    ++(*info); indx[i] = 0;					\
	  }								\
    }									\
  break;
  NSP_ITYPE_SWITCH(x->itype,IMAT_BSEARCHC,"");
#undef IMAT_BSEARCHC
}


void nsp_bsearchd_for_IMat(const NspIMatrix *x, const NspIMatrix *val,
		  int indx[], int occ[], int *info)
{
  int  i, j, j1, j2;
  int m=x->mn, n=val->mn;
 
  for (j = 0; j < n; j++ ) occ[j]= 0;
  *info = 0;

#define IMAT_BSEARCHD(name,type,arg)                                    \
  for (i = 0 ; i < m ; i++)                                             \
    {                                                                   \
      if (val->name[0] <= x->name[i] && x->name[i] <= val->name[n-1])   \
	{                                                               \
	  /* find j such that x[i] = val[j] by a binary search */       \
	  j1 = 0;                                                       \
	  j2 = n-1;                                                     \
	  while(j2 - j1 > 1)                                            \
	    {                                                           \
	      j = (j1 + j2) / 2;                                        \
	      if (x->name[i] < val->name[j]) j2 = j; else j1 = j;       \
	    }                                                           \
	  if (x->name[i] == val->name[j1])                              \
	    {                                                           \
	      ++occ[j1]; indx[i] = j1+1;                                \
	    }                                                           \
	  else if (x->name[i] == val->name[j2]) /* (note: this case may happen only for j2=n-1) */ \
	    {                                                           \
	      ++occ[j2]; indx[i] = j2+1;                                \
	    }                                                           \
	  else  /* x[i] is not in {val(1), val(2),..., val(n)} */       \
	    {                                                           \
	      ++(*info); indx[i] = 0;                                   \
	    }                                                           \
	}                                                               \
      else    /* x[i] is not in {val[0], val[1],..., val[n-1]} */       \
	{                                                               \
	  ++(*info); indx[i] = 0;                                       \
	}                                                               \
    }                                                                   \
  break;
  NSP_ITYPE_SWITCH(x->itype,IMAT_BSEARCHD,"");
#undef IMAT_BSEARCHD
} 
