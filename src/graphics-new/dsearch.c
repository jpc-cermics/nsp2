 /*------------------------------------------------------------------
  * Copyright ENPC 2004 
  *   Jean-Philippe Chancelier Enpc/Cermics
  *   jpc@cermics.enpc.fr 
  *   Bruno Pincon 
  *------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h> 

#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "../interp/Eval.h"

#include "nsp/graphics/Graphics.h"
#include "nsp/gsort-p.h"

void nsp_dsearchc(const double x[], int m,const double val[], int n, 
		  int indx[], int occ[], int *info);

void nsp_dsearchd(const double x[], int m,const double val[], int n,
		  int indx[], int occ[], int *info);

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
  int info, m_occ,n_occ,i;
  char c= 'c';
  NspMatrix *x, *val, *ind, *occ, *Minfo=NULL;
  CheckRhs(2,3);
  CheckLhs(0,3);
  if ((x =GetRealMat(stack,1))== NULLMAT ) return RET_BUG;
  CheckVector(stack.fname,1,x)
    if ((val =GetRealMat(stack,2))== NULLMAT ) return RET_BUG;
  CheckVector(stack.fname,2,val)
  if ( rhs == 3 ) 
    {
      if ((type = GetString(stack,3)) == (char*)0) return RET_BUG;
      if ( strcmp(type,"c") != 0 && strcmp(type,"d") != 0 ) 
	{
	  Scierror("Error: wrong second argument in function %s\n",stack.fname);
	  Scierror("\tonly 'c' or 'd' are allowed\n");
	  return RET_BUG;
	}
      c = type[0];
    }
  if (( ind = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT ) return RET_BUG;
  m_occ = val->m ; 
  n_occ = val->n ;
  if ( c == 'c' )
    {
      m_occ = Max(m_occ -1, 1); 
      n_occ = Max(n_occ -1, 1);
    }
  if (( occ = nsp_matrix_create(NVOID,'r',m_occ,n_occ)) == NULLMAT ) return RET_BUG;
  if ( lhs == 3 ) 
    {
      if (( Minfo = nsp_matrix_create(NVOID,'r',1,1 )) == NULLMAT ) return RET_BUG;
    }
  /* Check that val is increasing */
  for ( i = 0 ; i < val->mn -1 ; i++ ) 
    {
      if ( ! ( val->R[i] < val->R[i+1]) )
	{
	  Scierror("Error: second argument in function %s\n",stack.fname);
	  Scierror("\tis not scrictly increasing \n");
	  return RET_BUG;
	}
    }
  if ( c == 'c' ) 
    {
      if ( val->mn <= 1 ) 
	{ 
	  Scierror("%s: second argument should be of size > 1 for 'c' option \n",stack.fname);
	  return RET_BUG;
	}
    }
  else
    {
      if ( val->mn < 1 ) 
	{ 
	  Scierror("%s: second argument should be of size >= 1 for 'd' option \n",stack.fname);
	  return RET_BUG;
	}
    }
  if ( c == 'c' ) 
    nsp_dsearchc(x->R,x->mn,val->R,val->mn-1,(int *) ind->R,(int *) occ->R,&info);
  else 
    nsp_dsearchd(x->R,x->mn,val->R,val->mn,(int *) ind->R,(int *) occ->R,&info);
  if ( lhs >= 0) 
    {
      ind->convert = 'i'; /* occ is filed with integers */
      ind = Mat2double(ind);
      NthObj(rhs+1) = (NspObject *) ind;
      NSP_OBJECT(NthObj(rhs+1))->ret_pos = 1;
    }
  if ( lhs >= 2)
    {
      occ->convert = 'i';
      occ = Mat2double(occ);
      NthObj(rhs+2) = (NspObject *) occ;
      NSP_OBJECT(NthObj(rhs+2))->ret_pos = 2;
    }
  if ( lhs == 3 ) 
    {
      Minfo->R[0] = info;
      NthObj(rhs+3) = (NspObject *) Minfo;
      NSP_OBJECT(NthObj(rhs+3))->ret_pos = 3;
    }
  return Max(lhs,1);
}


/*     PURPOSE 
 *        val[n+1] being an array (with strict increasing order and n >=1) 
 *        representing n intervals, this routine, by the mean of a 
 *        dichotomic search, computes : 
 *
 *           a/ for each x[i] its interval number stored in indx[i] : 
 *                     indx[i] = j if  x[i] in (val[j-1], val[j] ] 
 *                             = 1 if  x[i] = val[0]
 *                             = 0 if  x[i] is not in [val(0),val(n)] 
 *
 *           b/ the number of points falling in the interval j : 
 *
 *              occ[j] = # { x[i] such that x[i] in ( val[j], val[j+1] } 
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
	  /* x[i] is in [val(0),val(n)] : 
	   * find j such that val[j-1] <= x[i] <= val[j] by a dicho search 
	   */
	  j1 = 0;
	  j2 = n;
	  while(j2 - j1 > 1) {
	    j = (j1 + j2) / 2;
	    if (x[i] <= val[j]) {
	      j2 = j;
	    } else {
	      j1 = j;
	    }
	  }
	  /* we have val[j1] < x[i] <= val[j2]  if j2 > 1  (j1=j2-1) 
	   * or val[j1] <= x[i] <= val[j2] if j2 = 1  (j1=j2-1) 
	   * so that j2 is the good interval number in all cases 
	   */
	  ++occ[j1]; 
	  indx[i] = j2 ;
	} else {
	  /* x[i] is not in [val(0), val(n)] */
	  ++(*info);
	  indx[i] = 0;
	}
    }
}

/*     PURPOSE 
 *        double val[n] being a strictly increasing array, this 
 *        routines by the mean of a dichotomic search computes : 
 *
 *        a/ the number of occurences (occ[j]) of each value val[j] 
 *           in the array x : 
 *
 *              occ[j] = #{ x[i] such that x[i] = val[j] } 
 *
 *        b/ the array indx :  if x[i] = val[j] then indx[i] = j+1
 *           (if x[i] is not in val then indx[i] = 0) 
 *
 *     PARAMETERS 
 *        inputs : 
 *           m         int 
 *           x[m]      double float array 
 *           n         int 
 *           val[n]    double float array (must be in a strict increasing order) 
 *        outputs : 
 *           occ[n]    int array 
 *           indx[m]   int array 
 *           info      int  (number of x[i] which are not in val(1..n)) 
 *
 *     AUTHOR 
 *        Bruno Pincon 
 */

void nsp_dsearchd(const double x[], int m,const double val[], int n,
		  int indx[], int occ[], int *info)
{
  int i, j, j1, j2;

  for (j = 0; j < n ; j++ ) occ[j]= 0;
  *info = 0;

  for (i = 0 ; i < m ; ++i) 
    {
      if (val[0] <= x[i] && x[i] < val[n-1]) {
	/* find j such that x[i] = val(j) by a dicho search */
	j1 = 0;
	j2 = n-1;
	while(j2 - j1 > 1) {
	  j = (j1 + j2) / 2;
	  if (x[i] < val[j]) {
	    j2 = j;
	  } else {
	    j1 = j;
	  }
	}
	/*  here we know that val(j1) <= x[i] <= val(j2)  with j2 = j1 + 1
	 *  (in fact we have exactly  val(j1) <= x[i] < val(j2) if j2 < n) 
	 */
	if (x[i] == val[j1]) {
	  ++occ[j1];
	  indx[i] = j1+1;
	} else if (x[i] == val[j2]) {
	  /* (note: this case may hap */
	  ++occ[j2];
	  indx[i] = j2+1;
	} else {
	  /* x[i] is not in {val(1), val(2),..., val(n)} */
	  ++(*info);
	  indx[i] = 0;
	}
      } else {
	/* x[i] is not in {val(1), val(2),..., val(n)} */
	++(*info);
	indx[i] = 0;
      }
    }
} 

/*
 *
 */





