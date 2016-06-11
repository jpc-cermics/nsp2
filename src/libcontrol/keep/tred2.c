/* tred2.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

int
nsp_ctrlpack_tred2 (int *nm, int *n, double *a, double *d__, double *e,
		    double *z__)
{
  /* System generated locals */
  int a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;
  double d__1;

  /* Builtin functions */
  double sqrt (double), d_sign (double *, double *);

  /* Local variables */
  double f, g, h__;
  int i__, j, k, l;
  double scale, hh;
  int ii, jp1;

  /* 
   * 
   *    this subroutine is a translation of the algol procedure tred2, 
   *    num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson. 
   *    handbook for auto. comp., vol.ii-linear algebra, 212-226(1971). 
   * 
   *    this subroutine reduces a real symmetric matrix to a 
   *    symmetric tridiagonal matrix using and accumulating 
   *    orthogonal similarity transformations. 
   * 
   *    on input 
   * 
   *       nm must be set to the row dimension of two-dimensional 
   *         array parameters as declared in the calling program 
   *         dimension statement. 
   * 
   *       n is the order of the matrix. 
   * 
   *       a contains the real symmetric input matrix.  only the 
   *         lower triangle of the matrix need be supplied. 
   * 
   *    on output 
   * 
   *       d contains the diagonal elements of the tridiagonal matrix. 
   * 
   *       e contains the subdiagonal elements of the tridiagonal 
   *         matrix in its last n-1 positions.  e(1) is set to zero. 
   * 
   *       z contains the orthogonal transformation matrix 
   *         produced in the reduction. 
   * 
   *       a and z may coincide.  if distinct, a is unaltered. 
   * 
   *    questions and comments should be directed to burton s. garbow, 
   *    mathematics and computer science div, argonne national laboratory 
   * 
   *    this version dated august 1983. 
   * 
   *    ------------------------------------------------------------------ 
   * 
   */
  /* Parameter adjustments */
  z_dim1 = *nm;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  --e;
  --d__;
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
      for (j = i__; j <= i__2; ++j)
	{
	  /* L80: */
	  z__[j + i__ * z_dim1] = a[j + i__ * a_dim1];
	}
      /* 
       */
      d__[i__] = a[*n + i__ * a_dim1];
      /* L100: */
    }
  /* 
   */
  if (*n == 1)
    {
      goto L510;
    }
  /*    .......... for i=n step -1 until 2 do -- .......... 
   */
  i__1 = *n;
  for (ii = 2; ii <= i__1; ++ii)
    {
      i__ = *n + 2 - ii;
      l = i__ - 1;
      h__ = 0.;
      scale = 0.;
      if (l < 2)
	{
	  goto L130;
	}
      /*    .......... scale row (algol tol then not needed) .......... 
       */
      i__2 = l;
      for (k = 1; k <= i__2; ++k)
	{
	  /* L120: */
	  scale += (d__1 = d__[k], Abs (d__1));
	}
      /* 
       */
      if (scale != 0.)
	{
	  goto L140;
	}
    L130:
      e[i__] = d__[l];
      /* 
       */
      i__2 = l;
      for (j = 1; j <= i__2; ++j)
	{
	  d__[j] = z__[l + j * z_dim1];
	  z__[i__ + j * z_dim1] = 0.;
	  z__[j + i__ * z_dim1] = 0.;
	  /* L135: */
	}
      /* 
       */
      goto L290;
      /* 
       */
    L140:
      i__2 = l;
      for (k = 1; k <= i__2; ++k)
	{
	  d__[k] /= scale;
	  h__ += d__[k] * d__[k];
	  /* L150: */
	}
      /* 
       */
      f = d__[l];
      d__1 = sqrt (h__);
      g = -d_sign (&d__1, &f);
      e[i__] = scale * g;
      h__ -= f * g;
      d__[l] = f - g;
      /*    .......... form a*u .......... 
       */
      i__2 = l;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L170: */
	  e[j] = 0.;
	}
      /* 
       */
      i__2 = l;
      for (j = 1; j <= i__2; ++j)
	{
	  f = d__[j];
	  z__[j + i__ * z_dim1] = f;
	  g = e[j] + z__[j + j * z_dim1] * f;
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
	      g += z__[k + j * z_dim1] * d__[k];
	      e[k] += z__[k + j * z_dim1] * f;
	      /* L200: */
	    }
	  /* 
	   */
	L220:
	  e[j] = g;
	  /* L240: */
	}
      /*    .......... form p .......... 
       */
      f = 0.;
      /* 
       */
      i__2 = l;
      for (j = 1; j <= i__2; ++j)
	{
	  e[j] /= h__;
	  f += e[j] * d__[j];
	  /* L245: */
	}
      /* 
       */
      hh = f / (h__ + h__);
      /*    .......... form q .......... 
       */
      i__2 = l;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L250: */
	  e[j] -= hh * d__[j];
	}
      /*    .......... form reduced a .......... 
       */
      i__2 = l;
      for (j = 1; j <= i__2; ++j)
	{
	  f = d__[j];
	  g = e[j];
	  /* 
	   */
	  i__3 = l;
	  for (k = j; k <= i__3; ++k)
	    {
	      /* L260: */
	      z__[k + j * z_dim1] =
		z__[k + j * z_dim1] - f * e[k] - g * d__[k];
	    }
	  /* 
	   */
	  d__[j] = z__[l + j * z_dim1];
	  z__[i__ + j * z_dim1] = 0.;
	  /* L280: */
	}
      /* 
       */
    L290:
      d__[i__] = h__;
      /* L300: */
    }
  /*    .......... accumulation of transformation matrices .......... 
   */
  i__1 = *n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      l = i__ - 1;
      z__[*n + l * z_dim1] = z__[l + l * z_dim1];
      z__[l + l * z_dim1] = 1.;
      h__ = d__[i__];
      if (h__ == 0.)
	{
	  goto L380;
	}
      /* 
       */
      i__2 = l;
      for (k = 1; k <= i__2; ++k)
	{
	  /* L330: */
	  d__[k] = z__[k + i__ * z_dim1] / h__;
	}
      /* 
       */
      i__2 = l;
      for (j = 1; j <= i__2; ++j)
	{
	  g = 0.;
	  /* 
	   */
	  i__3 = l;
	  for (k = 1; k <= i__3; ++k)
	    {
	      /* L340: */
	      g += z__[k + i__ * z_dim1] * z__[k + j * z_dim1];
	    }
	  /* 
	   */
	  i__3 = l;
	  for (k = 1; k <= i__3; ++k)
	    {
	      z__[k + j * z_dim1] -= g * d__[k];
	      /* L360: */
	    }
	}
      /* 
       */
    L380:
      i__3 = l;
      for (k = 1; k <= i__3; ++k)
	{
	  /* L400: */
	  z__[k + i__ * z_dim1] = 0.;
	}
      /* 
       */
      /* L500: */
    }
  /* 
   */
 L510:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      d__[i__] = z__[*n + i__ * z_dim1];
      z__[*n + i__ * z_dim1] = 0.;
      /* L520: */
    }
  /* 
   */
  z__[*n + *n * z_dim1] = 1.;
  e[1] = 0.;
  return 0;
}				/* tred2_ */
