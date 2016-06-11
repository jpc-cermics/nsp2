/* ortran.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* 
 *    this subroutine accumulates the orthogonal similarity 
 *    transformations used in the reduction of a real general 
 *    matrix to upper hessenberg form by  orthes. 
 * 
 *     subroutine ortran(nm,n,low,igh,a,ort,z) 
 * 
 *    on input: 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 *       n is the order of the matrix; 
 *       low and igh are ints determined by the balancing 
 *         subroutine  balanc.  if  balanc  has not been used, 
 *         set low=1, igh=n; 
 *       a contains information about the orthogonal trans- 
 *         formations used in the reduction by  orthes 
 *         in its strict lower triangle; 
 *       ort contains further information about the trans- 
 *         formations used in the reduction by  orthes. 
 *         only elements low through igh are used. 
 * 
 *    on output: 
 *       z contains the transformation matrix produced in the 
 *         reduction by  orthes; 
 *       ort has been altered. 
 *
 *    this subroutine is a translation of the algol procedure ortrans, 
 *    num. math. 16, 181-204(1970) by peters and wilkinson. 
 *    handbook for auto. comp., vol.ii-linear algebra, 372-395(1971). 
 *
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 */

int
nsp_ctrlpack_ortran (int *nm, int *n, int *low, int *igh, double *a,
		     double *ort, double *z__)
{
  int a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;
  double g;
  int i__, j, kl, mm, mp, mp1;

  /* Parameter adjustments */
  z_dim1 = *nm;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  --ort;
  a_dim1 = *nm;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L60: */
	  z__[i__ + j * z_dim1] = 0.;
	}
      /* 
       */
      z__[i__ + i__ * z_dim1] = 1.;
      /* L80: */
    }
  /* 
   */
  kl = *igh - *low - 1;
  if (kl < 1)
    {
      goto L200;
    }
  /*    :::::::::: for mp=igh-1 step -1 until low+1 do -- :::::::::: 
   */
  i__1 = kl;
  for (mm = 1; mm <= i__1; ++mm)
    {
      mp = *igh - mm;
      if (a[mp + (mp - 1) * a_dim1] == 0.)
	{
	  goto L140;
	}
      mp1 = mp + 1;
      /* 
       */
      i__2 = *igh;
      for (i__ = mp1; i__ <= i__2; ++i__)
	{
	  /* L100: */
	  ort[i__] = a[i__ + (mp - 1) * a_dim1];
	}
      /* 
       */
      i__2 = *igh;
      for (j = mp; j <= i__2; ++j)
	{
	  g = 0.;
	  /* 
	   */
	  i__3 = *igh;
	  for (i__ = mp; i__ <= i__3; ++i__)
	    {
	      /* L110: */
	      g += ort[i__] * z__[i__ + j * z_dim1];
	    }
	  /*    :::::::::: divisor below is negative of h formed in orthes. 
	   *               double division avoids possible underflow :::::::::: 
	   */
	  g = g / ort[mp] / a[mp + (mp - 1) * a_dim1];
	  /* 
	   */
	  i__3 = *igh;
	  for (i__ = mp; i__ <= i__3; ++i__)
	    {
	      /* L120: */
	      z__[i__ + j * z_dim1] += g * ort[i__];
	    }
	  /* 
	   */
	  /* L130: */
	}
      /* 
       */
    L140:
      ;
    }
  /* 
   */
 L200:
  return 0;
  /*    :::::::::: last card of ortran :::::::::: 
   */
}				/* ortran_ */
