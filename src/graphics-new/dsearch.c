/*------------------------------------------------------------------
 * Copyright 2004 
 *   Bruno Pincon  Bruno.Pincon@iecn.u-nancy.fr
 *   Jean-Philippe Chancelier Enpc/Cermics jpc@cermics.enpc.fr 
 *------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h> 

#include "nsp/matrix-in.h"
#include "nsp/smatrix-in.h"
#include "nsp/bmatrix-in.h"
#include "../interp/Eval.h"

#include "nsp/graphics/Graphics.h"
#include "nsp/gsort-p.h"

void nsp_dsearchc(const double x[], int m,const double val[], int n, 
		  int indx[], int occ[], int *info);

void nsp_dsearchd(const double x[], int m,const double val[], int n,
		  int indx[], int occ[], int *info);

void nsp_dsearchc_for_strings(char **x, int m, char **val, int n, 
			      int indx[], int occ[], int *info);

void nsp_dsearchd_for_strings(char **x, int m, char **val, int n,
			      int *indx, int *occ, int *info);


/*
 * dsearch interface  
 * jpc 
 *       [ind , occ, info] = dsearch(x, val, [ 'c' | 'd' ])
 *        
 * X and val must be real vectors (says of length m for X and n for val ), 
 * if ch is not present then ch = 'c'  (dsearch on "intervals")
 * ch must be 'd' or 'c'
 *
 * ind is a vector with the same shape as x 
 * occ is a vector with the same shape as val (but with n-1
 * components in the case ch='c')
 * info is a scalar
 */

int int_dsearch(Stack stack, int rhs, int opt, int lhs)
{
  char *type;
  int info, m_occ, n_occ, m_x, n_x, mn_val, i, flagstr;
  char c= 'c';
  NspMatrix *x=NULLMAT, *val=NULLMAT, *ind=NULLMAT, *occ=NULLMAT;
  NspSMatrix *xstr=NULLSMAT, *valstr=NULLSMAT;
  CheckRhs(2,3);
  CheckLhs(0,3);

  if ( IsMatObj(stack,1) ) 
    {
      if ( (x=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      if ( (val=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
      CheckVector(stack.fname,2,val);
      flagstr = 0; m_x = x->m; n_x = x->n;
      m_occ = val->m;  n_occ = val->n; mn_val = val->mn;
    }
  else if ( IsSMatObj(stack,1) ) 
    {
      if ( (xstr=GetSMat(stack,1)) == NULLSMAT ) return RET_BUG;
      if ( (valstr=GetSMat(stack,2)) == NULLSMAT ) return RET_BUG;
      CheckVector(stack.fname,2,valstr);
      flagstr = 1; m_x = xstr->m; n_x = xstr->n;
      m_occ = valstr->m; n_occ = valstr->n; mn_val = valstr->mn;
    }
  else
    {
      Scierror("Error: first argument must be of type real or string in function %s\n",stack.fname);
      return RET_BUG;
    }

  if ( rhs == 3 ) 
    {
      if ((type = GetString(stack,3)) == NULLSTRING ) return RET_BUG;
      if ( strcmp(type,"c") != 0 && strcmp(type,"d") != 0 ) 
	{
	  Scierror("Error: wrong third argument in function %s\n",stack.fname);
	  Scierror("\tonly 'c' or 'd' are allowed\n");
	  return RET_BUG;
	}
      c = type[0];
    }

  if ( (ind=nsp_matrix_create(NVOID,'r',m_x,n_x)) == NULLMAT ) return RET_BUG;
  if ( c == 'c' )
    {
      m_occ = Max(m_occ -1, 1); 
      n_occ = Max(n_occ -1, 1);
    }

  if ( (occ=nsp_matrix_create(NVOID,'r',m_occ,n_occ)) == NULLMAT ) return RET_BUG;

  /* Check that val is increasing */
  if ( flagstr == 0 )
    {
      for ( i = 0 ; i < mn_val-1 ; i++ ) 
	if ( ! ( val->R[i] < val->R[i+1]) )
	  {
	    Scierror("Error: second argument in function %s\n",stack.fname);
	    Scierror("\tis not scrictly increasing \n");
	    return RET_BUG;
	  }
    }
  else
    {
      for ( i = 0 ; i < mn_val-1 ; i++ ) 
	if ( strcmp(valstr->S[i],valstr->S[i+1]) >= 0 )
	  {
	    Scierror("Error: second argument in function %s\n",stack.fname);
	    Scierror("\tis not scrictly increasing \n");
	    return RET_BUG;
	  }
    }

  if ( c == 'c' ) 
    {
      if ( mn_val <= 1 ) 
	{ 
	  Scierror("%s: second argument should be of size > 1 for 'c' option \n",stack.fname);
	  return RET_BUG;
	}
    }
  else
    {
      if ( mn_val < 1 ) 
	{ 
	  Scierror("%s: second argument should be of size >= 1 for 'd' option \n",stack.fname);
	  return RET_BUG;
	}
    }

  if ( flagstr == 0 )
    {
      if ( c == 'c' ) 
	nsp_dsearchc(x->R,x->mn,val->R,val->mn-1,ind->I,occ->I,&info);
      else 
	nsp_dsearchd(x->R,x->mn,val->R,val->mn,ind->I,occ->I,&info);
    }
  else
    {
      if ( c == 'c' ) 
	nsp_dsearchc_for_strings(xstr->S,xstr->mn,valstr->S,mn_val-1,ind->I,occ->I,&info);
      else 
	nsp_dsearchd_for_strings(xstr->S,xstr->mn,valstr->S,mn_val,ind->I,occ->I,&info);
    }

  ind->convert = 'i'; /* occ is filed with integers */
  ind = Mat2double(ind);
  MoveObj(stack,1,NSP_OBJECT(ind));
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

void nsp_dsearchc(const double x[], int m,const double val[], int n, 
		  int indx[], int occ[], int *info)
{
  int  i, j, j1, j2;
 
  for (j = 0; j < n; j++ ) occ[j]= 0;
  *info = 0;

  for (i = 0; i < m ; i++ ) 
    {
      if (val[0] <= x[i] && x[i] <= val[n]) 
	{
	  /* x[i] is in [val(0),val(n)]:  find j such that x[i] is in I(j) */
	  j1 = 0;
	  j2 = n;
	  while(j2 - j1 > 1) 
	    {
	      j = (j1 + j2) / 2;
	      if (x[i] < val[j]) j2 = j; else j1 = j;
	    }
	  /* here we have : (i)  j2 = j1+1 
	   *                (ii) val[j1] <= x[i] < val[j2]  if j2 < n 
           *                  or val[j1] <= x[i] <= val[j2] if j2 = n
	   * so j2 is the good interval number in all cases 
	   */
	  ++occ[j1]; 
	  indx[i] = j2 ;
	} 
      else   /* x[i] is not in [val(0), val(n)] */ 
	{
	  ++(*info);
	  indx[i] = 0;
	}
    }
}

/**
 * nsp_dsearchd:
 * @x: array of double.
 * @m: size of @x
 * @val: a strictly increasing array of double.
 * @n: size of @val
 * @indx: int array
 * @occ: int array
 * @info: int pointer.
 * 
 * @val being a strictly increasing array of size @n,
 * this routines by the mean of a dichotomic search computes
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

void nsp_dsearchd(const double *x, int m,const double *val, int n,
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


void nsp_dsearchc_for_strings(char **x, int m, char **val, int n, 
			      int indx[], int occ[], int *info)
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
	  while(j2 - j1 > 1) 
	    {
	      j = (j1 + j2) / 2;
	      if ( strcmp(x[i],val[j]) < 0 ) j2 = j; else j1 = j;
	    }
	  /* here we have : (i)  j2 = j1+1 
	   *                (ii) val[j1] <= x[i] < val[j2]  if j2 < n 
           *                  or val[j1] <= x[i] <= val[j2] if j2 = n
	   * so j2 is the good interval number in all cases 
	   */
	  ++occ[j1]; 
	  indx[i] = j2 ;
	} 
      else  /* x[i] is not in [val(0), val(n)] */ 
	{
	  ++(*info);
	  indx[i] = 0;
	}
    }
}


void nsp_dsearchd_for_strings(char **x, int m, char **val, int n,
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



