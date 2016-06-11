/* imtql3.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* 
 *    this subroutine finds the eigenvalues and eigenvectors 
 *    of a symmetric tridiagonal matrix by the implicit ql method. 
 *    the eigenvectors of a full symmetric matrix can also 
 *    be found if  tred2  has been used to reduce this 
 *    full matrix to tridiagonal form. 
 * 
 *    subroutine imtql3(nm,n,d,e,z,ierr) 
 * 
 *    int i,j,k,l,m,n,ii,nm,mml,ierr 
 *    real*8 d(n),e(n),z(nm,n) 
 *    real*8 b,c,f,g,p,r,s,machep 
 * 
 *    on input: 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 *       n is the order of the matrix; 
 *       d contains the diagonal elements of the input matrix; 
 *       e contains the subdiagonal elements of the input matrix 
 *         in its last n-1 positions.  e(1) is arbitrary; 
 *       z contains the transformation matrix produced in the 
 *         reduction by  tred2, if performed.  if the eigenvectors 
 *         of the tridiagonal matrix are desired, z must contain 
 *         the identity matrix. 
 *       job specifies if eigenvectors are desired 
 *           job=1 eigenvectors are calculated 
 *           job=0 no eigenvectors 
 * 
 *     on output: 
 *       d contains the eigenvalues in ascending order.  if an 
 *         error exit is made, the eigenvalues are correct but 
 *         unordered for indices 1,2,...,ierr-1; 
 *       e has been destroyed; 
 *       z contains orthonormal eigenvectors of the symmetric 
 *         tridiagonal (or full) matrix.  if an error exit is made, 
 *         z contains the eigenvectors associated with the stored 
 *         eigenvalues; 
 *       ierr is set to 
 *         zero       for normal return, 
 *         j          if the j-th eigenvalue has not been 
 *                    determined after 30 iterations. 
 * 
 *    this subroutine is a translation of the algol procedure imtql3, 
 *    num. math. 12, 377-383(1968) by martin and wilkinson, 
 *    as modified in num. math. 15, 450(1970) by dubrulle. 
 *    handbook for auto. comp., vol.ii-linear algebra, 241-248(1971). 
 * 
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 */


int
nsp_ctrlpack_imtql3 (int *nm, int *n, double *d__, double *e, double *z__,
		     int *ierr, int *job)
{
  /* System generated locals */
  int z_dim1, z_offset, i__1, i__2, i__3;
  double d__1, d__2, d__3;

  /* Local variables */
  double b, c__, f, g;
  int i__, j, k, l, m;
  double p, r__, s;
  int ii;
  double machep;
  int mml;

  /* Parameter adjustments */
  z_dim1 = *nm;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  --e;
  --d__;

  /* Function Body */
  machep = nsp_dlamch ("p");
  /* 
   */
  *ierr = 0;
  if (*n == 1)
    {
      goto L1001;
    }
  /* 
   */
  i__1 = *n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      /* L100: */
      e[i__ - 1] = e[i__];
    }
  /* 
   */
  e[*n] = 0.;
  /* 
   */
  i__1 = *n;
  for (l = 1; l <= i__1; ++l)
    {
      j = 0;
      /*    :::::::::: look for small sub-diagonal element :::::::::: 
       */
    L105:
      i__2 = *n;
      for (m = l; m <= i__2; ++m)
	{
	  if (m == *n)
	    {
	      goto L120;
	    }
	  if ((d__1 =
	       e[m], Abs (d__1)) <= machep * ((d__2 =
					       d__[m], Abs (d__2)) + (d__3 =
								      d__[m +
									  1],
								      Abs
								      (d__3))))
	    {
	      goto L120;
	    }
	  /* L110: */
	}
      /* 
       */
    L120:
      p = d__[l];
      if (m == l)
	{
	  goto L240;
	}
      if (j == 30)
	{
	  goto L1000;
	}
      ++j;
      /*    :::::::::: form shift :::::::::: 
       */
      g = (d__[l + 1] - p) / (e[l] * 2.);
      r__ = sqrt (g * g + 1.);
      g = d__[m] - p + e[l] / (g + d_sign (&r__, &g));
      s = 1.;
      c__ = 1.;
      p = 0.;
      mml = m - l;
      /*    :::::::::: for i=m-1 step -1 until l do -- :::::::::: 
       */
      i__2 = mml;
      for (ii = 1; ii <= i__2; ++ii)
	{
	  i__ = m - ii;
	  f = s * e[i__];
	  b = c__ * e[i__];
	  if (Abs (f) < Abs (g))
	    {
	      goto L150;
	    }
	  c__ = g / f;
	  r__ = sqrt (c__ * c__ + 1.);
	  e[i__ + 1] = f * r__;
	  s = 1. / r__;
	  c__ *= s;
	  goto L160;
	L150:
	  s = f / g;
	  r__ = sqrt (s * s + 1.);
	  e[i__ + 1] = g * r__;
	  c__ = 1. / r__;
	  s *= c__;
	L160:
	  g = d__[i__ + 1] - p;
	  r__ = (d__[i__] - g) * s + c__ * 2. * b;
	  p = s * r__;
	  d__[i__ + 1] = g + p;
	  g = c__ * r__ - b;
	  /*    :::::::::: form vector :::::::::: 
	   */
	  if (*job == 0)
	    {
	      goto L200;
	    }
	  i__3 = *n;
	  for (k = 1; k <= i__3; ++k)
	    {
	      f = z__[k + (i__ + 1) * z_dim1];
	      z__[k + (i__ + 1) * z_dim1] =
		s * z__[k + i__ * z_dim1] + c__ * f;
	      z__[k + i__ * z_dim1] = c__ * z__[k + i__ * z_dim1] - s * f;
	      /* L180: */
	    }
	  /* 
	   */
	L200:
	  ;
	}
      /* 
       */
      d__[l] -= p;
      e[l] = g;
      e[m] = 0.;
      goto L105;
    L240:
      ;
    }
  /*    :::::::::: order eigenvalues and eigenvectors :::::::::: 
   */
  i__1 = *n;
  for (ii = 2; ii <= i__1; ++ii)
    {
      i__ = ii - 1;
      k = i__;
      p = d__[i__];
      /* 
       */
      i__2 = *n;
      for (j = ii; j <= i__2; ++j)
	{
	  if (d__[j] >= p)
	    {
	      goto L260;
	    }
	  k = j;
	  p = d__[j];
	L260:
	  ;
	}
      /* 
       */
      if (k == i__)
	{
	  goto L300;
	}
      d__[k] = d__[i__];
      d__[i__] = p;
      /* 
       */
      if (*job == 0)
	{
	  goto L300;
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  p = z__[j + i__ * z_dim1];
	  z__[j + i__ * z_dim1] = z__[j + k * z_dim1];
	  z__[j + k * z_dim1] = p;
	  /* L280: */
	}
      /* 
       */
    L300:
      ;
    }
  /* 
   */
  goto L1001;
  /*    :::::::::: set error -- no convergence to an 
   *               eigenvalue after 30 iterations :::::::::: 
   */
 L1000:
  *ierr = l;
 L1001:
  return 0;
  /*    :::::::::: last card of imtql3 :::::::::: 
   */
}				/* imtql3_ */
