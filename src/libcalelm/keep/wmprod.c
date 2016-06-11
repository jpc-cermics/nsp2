/* wmprod.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__0 = 0;

int
nsp_calpack_wmprod (int *flag__, double *ar, double *ai, int *na, int *m,
		    int *n, double *vr, double *vi, int *nv)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1;

  /* Local variables */
  int i__, j;
  double ti;
  int iv;
  double tr;

  /*!purpose 
   *    computes the product of the entries of a complex matrix according to flag 
   *!calling sequence 
   *    subroutine wmprod(flag,ar,ai,na,m,n,vr,vi,nv) 
   *    double precision ar(na,n),ai(na,n),vr(*),vi(*) 
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
   *    v    : array containing the result, 
   *           vr (resp vi) may be confused with a row or 
   *           a column of the ar (resp ai) matrix 
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
  ai_dim1 = *na;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *na;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
  --vr;
  --vi;

  /* Function Body */
  iv = 1;
  if (*flag__ == 0)
    {
      /*    product of all the entries 
       */
      tr = 1.;
      ti = 0.;
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  nsp_calpack_wvmul (m, &ar[j * ar_dim1 + 1], &ai[j * ai_dim1 + 1],
			     &c__1, &tr, &ti, &c__0);
	  /* L10: */
	}
      vr[1] = tr;
      vi[1] = ti;
    }
  else if (*flag__ == 1)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  tr = 1.;
	  ti = 0.;
	  nsp_calpack_wvmul (m, &ar[j * ar_dim1 + 1], &ai[j * ai_dim1 + 1],
			     &c__1, &tr, &ti, &c__0);
	  vr[iv] = tr;
	  vi[iv] = ti;
	  iv += *nv;
	  /* L20: */
	}
    }
  else if (*flag__ == 2)
    {
      i__1 = *m;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  tr = 1.;
	  ti = 0.;
	  nsp_calpack_wvmul (n, &ar[i__ + ar_dim1], &ai[i__ + ai_dim1], m,
			     &tr, &ti, &c__0);
	  vr[iv] = tr;
	  vi[iv] = ti;
	  iv += *nv;
	  /* L30: */
	}
    }
  return 0;
}				/* wmprod_ */
