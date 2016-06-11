/* dmprod.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__0 = 0;

int
nsp_calpack_dmprod (int *flag__, double *a, int *na, int *m, int *n,
		    double *v, int *nv)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1;

  /* Local variables */
  int i__, j;
  double t;
  int iv;

  /*!purpose 
   *    computes the product of the entries of a matrix according to flag 
   *!calling sequence 
   *    subroutine dmprod(flag,a,na,m,n,v,nv) 
   *    double precision a(na,n),v(*) 
   *    int na,n,m,nv 
   *    int flag 
   *!parameters 
   *    flag : indicates operation to perform 
   *           0 : returns in v(1) the product of all entries of a 
   *           1 : returns in v(j) the product of jth column of a 
   *           2 : returns in v(i) the product of ith row of a 
   *    a    : array containing the a matrix 
   *    na   : a matrix leading dimension 
   *    m    : a matrix row dimension 
   *    n    : a matrix column dimension 
   *    v    : array containing the result, may be confused with a row or 
   *           a column of the a matrix 
   *           if flag==0 size(v)>=1 
   *           if flag==1 size(v)>=n*nv 
   *           if flag==1 size(v)>=m*nv 
   *    nv   : increment between to consecutive entries ov v 
   *! 
   *    Copyright INRIA 
   * 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --v;

  /* Function Body */
  iv = 1;
  if (*flag__ == 0)
    {
      /*    product of all the entries 
       */
      t = 1.;
      /*        do 10 j=1,n 
       *           call dvmul(m,a(1,j),1,t,0) 
       *10      continue 
       */
      i__1 = *m * *n;
      nsp_calpack_dvmul (&i__1, &a[a_dim1 + 1], &c__1, &t, &c__0);
      v[1] = t;
    }
  else if (*flag__ == 1)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  t = 1.;
	  nsp_calpack_dvmul (m, &a[j * a_dim1 + 1], &c__1, &t, &c__0);
	  v[iv] = t;
	  iv += *nv;
	  /* L20: */
	}
    }
  else if (*flag__ == 2)
    {
      i__1 = *m;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  t = 1.;
	  nsp_calpack_dvmul (n, &a[i__ + a_dim1], m, &t, &c__0);
	  v[iv] = t;
	  iv += *nv;
	  /* L30: */
	}
    }
  return 0;
}				/* dmprod_ */
