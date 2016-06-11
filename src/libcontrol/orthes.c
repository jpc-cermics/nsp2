/* orthes.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
/* 
 *    given a real general matrix, this subroutine 
 *    reduces a submatrix situated in rows and columns 
 *    low through igh to upper hessenberg form by 
 *    orthogonal similarity transformations. 
 * 
 *     subroutine orthes(nm,n,low,igh,a,ort) 
 *    on input: 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 *       n is the order of the matrix; 
 *       low and igh are ints determined by the balancing 
 *         subroutine  balanc.  if  balanc  has not been used, 
 *         set low=1, igh=n; 
 *       a contains the input matrix. 
 *    on output: 
 *       a contains the hessenberg matrix.  information about 
 *         the orthogonal transformations used in the reduction 
 *         is stored in the remaining triangle under the 
 *         hessenberg matrix; 
 *       ort contains further information about the transformations. 
 *         only elements low through igh are used. 
 * 
 *    this subroutine is a translation of the algol procedure orthes, 
 *    num. math. 12, 349-368(1968) by martin and wilkinson. 
 *    handbook for auto. comp., vol.ii-linear algebra, 339-358(1971). 
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 */

int
nsp_ctrlpack_orthes (int *nm, int *n, int *low, int *igh, double *a,
		     double *ort)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2, i__3;
  double d__1;

  /* Builtin functions */
  double sqrt (double), d_sign (double *, double *);

  /* Local variables */
  double f, g, h__;
  int i__, j, m;
  double scale;
  int la, ii, jj, mp, kp1;

  /* Parameter adjustments */
  a_dim1 = *nm;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ort;

  /* Function Body */
  la = *igh - 1;
  kp1 = *low + 1;
  if (la < kp1)
    {
      goto L200;
    }
  /* 
   */
  i__1 = la;
  for (m = kp1; m <= i__1; ++m)
    {
      h__ = 0.;
      ort[m] = 0.;
      scale = 0.;
      /*    :::::::::: scale column (algol tol then not needed) :::::::::: 
       */
      i__2 = *igh;
      for (i__ = m; i__ <= i__2; ++i__)
	{
	  /* L90: */
	  scale += (d__1 = a[i__ + (m - 1) * a_dim1], Abs (d__1));
	}
      /* 
       */
      if (scale == 0.)
	{
	  goto L180;
	}
      mp = m + *igh;
      /*    :::::::::: for i=igh step -1 until m do -- :::::::::: 
       */
      i__2 = *igh;
      for (ii = m; ii <= i__2; ++ii)
	{
	  i__ = mp - ii;
	  ort[i__] = a[i__ + (m - 1) * a_dim1] / scale;
	  h__ += ort[i__] * ort[i__];
	  /* L100: */
	}
      /* 
       */
      d__1 = sqrt (h__);
      g = -d_sign (&d__1, &ort[m]);
      h__ -= ort[m] * g;
      ort[m] -= g;
      /*    :::::::::: form (i-(u*ut)/h) * a :::::::::: 
       */
      i__2 = *n;
      for (j = m; j <= i__2; ++j)
	{
	  f = 0.;
	  /*    :::::::::: for i=igh step -1 until m do -- :::::::::: 
	   */
	  i__3 = *igh;
	  for (ii = m; ii <= i__3; ++ii)
	    {
	      i__ = mp - ii;
	      f += ort[i__] * a[i__ + j * a_dim1];
	      /* L110: */
	    }
	  /* 
	   */
	  f /= h__;
	  /* 
	   */
	  i__3 = *igh;
	  for (i__ = m; i__ <= i__3; ++i__)
	    {
	      /* L120: */
	      a[i__ + j * a_dim1] -= f * ort[i__];
	    }
	  /* 
	   */
	  /* L130: */
	}
      /*    :::::::::: form (i-(u*ut)/h)*a*(i-(u*ut)/h) :::::::::: 
       */
      i__2 = *igh;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  f = 0.;
	  /*    :::::::::: for j=igh step -1 until m do -- :::::::::: 
	   */
	  i__3 = *igh;
	  for (jj = m; jj <= i__3; ++jj)
	    {
	      j = mp - jj;
	      f += ort[j] * a[i__ + j * a_dim1];
	      /* L140: */
	    }
	  /* 
	   */
	  f /= h__;
	  /* 
	   */
	  i__3 = *igh;
	  for (j = m; j <= i__3; ++j)
	    {
	      /* L150: */
	      a[i__ + j * a_dim1] -= f * ort[j];
	    }
	  /* 
	   */
	  /* L160: */
	}
      /* 
       */
      ort[m] = scale * ort[m];
      a[m + (m - 1) * a_dim1] = scale * g;
    L180:
      ;
    }
  /* 
   */
 L200:
  return 0;
  /*    :::::::::: last card of orthes :::::::::: 
   */
}				/* orthes_ */
