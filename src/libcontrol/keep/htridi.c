/* htridi.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* 
 *    this subroutine reduces a complex hermitian matrix 
 *    to a real symmetric tridiagonal matrix using 
 *    unitary similarity transformations. 
 *
 *    subroutine htridi(nm,n,ar,ai,d,e,e2,tau) 
 * 
 *    int n,nm 
 *    double precision ar(nm,n),ai(nm,n),d(n),e(n),e2(n),tau(2,n) 
 * 
 *    on input: 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 *       n is the order of the matrix; 
 *       ar and ai contain the real and imaginary parts, 
 *         respectively, of the complex hermitian input matrix. 
 *         only the lower triangle of the matrix need be supplied. 
 *    on output: 
 *       ar and ai contain information about the unitary trans- 
 *         formations used in the reduction in their full lower 
 *         triangles.  their strict upper triangles and the 
 *         diagonal of ar are unaltered; 
 *       d contains the diagonal elements of the the tridiagonal matrix; 
 *       e contains the subdiagonal elements of the tridiagonal 
 *         matrix in its last n-1 positions.  e(1) is set to zero; 
 *       e2 contains the squares of the corresponding elements of e. 
 *         e2 may coincide with e if the squares are not needed; 
 *       tau contains further information about the transformations. 
 * 
 *    arithmetic is real except for the use of the subroutines 
 * 
 *    this subroutine is a translation of a complex analogue of 
 *    the algol procedure tred1, num. math. 11, 181-195(1968) 
 *    by martin, reinsch, and wilkinson. 
 *    handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). 
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 */

int
nsp_ctrlpack_htridi (int *nm, int *n, double *ar, double *ai, double *d__,
		     double *e, double *e2, double *tau)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  double f, g, h__;
  int i__, j, k, l;
  double scale, fi, gi, hh;
  int ii;
  double si;
  int jp1;

  /* Parameter adjustments */
  tau -= 3;
  --e2;
  --e;
  --d__;
  ai_dim1 = *nm;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *nm;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;

  /* Function Body */
  tau[(*n << 1) + 1] = 1.;
  tau[(*n << 1) + 2] = 0.;
  /* 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L100: */
      d__[i__] = ar[i__ + i__ * ar_dim1];
    }
  /*    :::::::::: for i=n step -1 until 1 do -- :::::::::: 
   */
  i__1 = *n;
  for (ii = 1; ii <= i__1; ++ii)
    {
      i__ = *n + 1 - ii;
      l = i__ - 1;
      h__ = 0.;
      scale = 0.;
      if (l < 1)
	{
	  goto L130;
	}
      /*    :::::::::: scale row (algol tol then not needed) :::::::::: 
       */
      i__2 = l;
      for (k = 1; k <= i__2; ++k)
	{
	  /* L120: */
	  scale = scale + (d__1 = ar[i__ + k * ar_dim1], Abs (d__1)) + (d__2 =
									ai[i__
									   +
									   k *
									   ai_dim1],
									Abs
									(d__2));
	}
      /* 
       */
      if (scale != 0.)
	{
	  goto L140;
	}
      tau[(l << 1) + 1] = 1.;
      tau[(l << 1) + 2] = 0.;
    L130:
      e[i__] = 0.;
      e2[i__] = 0.;
      goto L290;
      /* 
       */
    L140:
      i__2 = l;
      for (k = 1; k <= i__2; ++k)
	{
	  ar[i__ + k * ar_dim1] /= scale;
	  ai[i__ + k * ai_dim1] /= scale;
	  h__ =
	    h__ + ar[i__ + k * ar_dim1] * ar[i__ + k * ar_dim1] + ai[i__ +
								     k *
								     ai_dim1]
	    * ai[i__ + k * ai_dim1];
	  /* L150: */
	}
      /* 
       */
      e2[i__] = scale * scale * h__;
      g = sqrt (h__);
      e[i__] = scale * g;
      f =
	sqrt (ar[i__ + l * ar_dim1] * ar[i__ + l * ar_dim1] +
	      ai[i__ + l * ai_dim1] * ai[i__ + l * ai_dim1]);
      /*    :::::::::: form next diagonal element of matrix t :::::::::: 
       */
      if (f == 0.)
	{
	  goto L160;
	}
      tau[(l << 1) + 1] =
	(ai[i__ + l * ai_dim1] * tau[(i__ << 1) + 2] -
	 ar[i__ + l * ar_dim1] * tau[(i__ << 1) + 1]) / f;
      si =
	(ar[i__ + l * ar_dim1] * tau[(i__ << 1) + 2] +
	 ai[i__ + l * ai_dim1] * tau[(i__ << 1) + 1]) / f;
      h__ += f * g;
      g = g / f + 1.;
      ar[i__ + l * ar_dim1] = g * ar[i__ + l * ar_dim1];
      ai[i__ + l * ai_dim1] = g * ai[i__ + l * ai_dim1];
      if (l == 1)
	{
	  goto L270;
	}
      goto L170;
    L160:
      tau[(l << 1) + 1] = -tau[(i__ << 1) + 1];
      si = tau[(i__ << 1) + 2];
      ar[i__ + l * ar_dim1] = g;
    L170:
      f = 0.;
      /* 
       */
      i__2 = l;
      for (j = 1; j <= i__2; ++j)
	{
	  g = 0.;
	  gi = 0.;
	  /*    :::::::::: form element of a*u :::::::::: 
	   */
	  i__3 = j;
	  for (k = 1; k <= i__3; ++k)
	    {
	      g =
		g + ar[j + k * ar_dim1] * ar[i__ + k * ar_dim1] + ai[j +
								     k *
								     ai_dim1]
		* ai[i__ + k * ai_dim1];
	      gi =
		gi - ar[j + k * ar_dim1] * ai[i__ + k * ai_dim1] + ai[j +
								      k *
								      ai_dim1]
		* ar[i__ + k * ar_dim1];
	      /* L180: */
	    }
	  /* 
	   */
	  jp1 = j + 1;
	  if (l < jp1)
	    {
	      goto L220;
	    }
	  /* 
	   */
	  i__3 = l;
	  for (k = jp1; k <= i__3; ++k)
	    {
	      g =
		g + ar[k + j * ar_dim1] * ar[i__ + k * ar_dim1] - ai[k +
								     j *
								     ai_dim1]
		* ai[i__ + k * ai_dim1];
	      gi =
		gi - ar[k + j * ar_dim1] * ai[i__ + k * ai_dim1] - ai[k +
								      j *
								      ai_dim1]
		* ar[i__ + k * ar_dim1];
	      /* L200: */
	    }
	  /*    :::::::::: form element of p :::::::::: 
	   */
	L220:
	  e[j] = g / h__;
	  tau[(j << 1) + 2] = gi / h__;
	  f =
	    f + e[j] * ar[i__ + j * ar_dim1] - tau[(j << 1) + 2] * ai[i__ +
								      j *
								      ai_dim1];
	  /* L240: */
	}
      /* 
       */
      hh = f / (h__ + h__);
      /*    :::::::::: form reduced a :::::::::: 
       */
      i__2 = l;
      for (j = 1; j <= i__2; ++j)
	{
	  f = ar[i__ + j * ar_dim1];
	  g = e[j] - hh * f;
	  e[j] = g;
	  fi = -ai[i__ + j * ai_dim1];
	  gi = tau[(j << 1) + 2] - hh * fi;
	  tau[(j << 1) + 2] = -gi;
	  /* 
	   */
	  i__3 = j;
	  for (k = 1; k <= i__3; ++k)
	    {
	      ar[j + k * ar_dim1] =
		ar[j + k * ar_dim1] - f * e[k] - g * ar[i__ + k * ar_dim1] +
		fi * tau[(k << 1) + 2] + gi * ai[i__ + k * ai_dim1];
	      ai[j + k * ai_dim1] =
		ai[j + k * ai_dim1] - f * tau[(k << 1) + 2] - g * ai[i__ +
								     k *
								     ai_dim1]
		- fi * e[k] - gi * ar[i__ + k * ar_dim1];
	      /* L260: */
	    }
	}
      /* 
       */
    L270:
      i__3 = l;
      for (k = 1; k <= i__3; ++k)
	{
	  ar[i__ + k * ar_dim1] = scale * ar[i__ + k * ar_dim1];
	  ai[i__ + k * ai_dim1] = scale * ai[i__ + k * ai_dim1];
	  /* L280: */
	}
      /* 
       */
      tau[(l << 1) + 2] = -si;
    L290:
      hh = d__[i__];
      d__[i__] = ar[i__ + i__ * ar_dim1];
      ar[i__ + i__ * ar_dim1] = hh;
      ai[i__ + i__ * ai_dim1] = scale * sqrt (h__);
      /* L300: */
    }
  /* 
   */
  return 0;
  /*    :::::::::: last card of htridi :::::::::: 
   */
}				/* htridi_ */
