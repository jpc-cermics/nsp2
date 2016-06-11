/* tql2.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static double c_b10 = 1.;

int
nsp_ctrlpack_tql2 (int *nm, int *n, double *d__, double *e, double *z__,
		   int *job, int *ierr)
{
  /* System generated locals */
  int z_dim1, z_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Builtin functions */
  double d_sign (double *, double *);

  /* Local variables */
  double c__, f, g, h__;
  int i__, j, k, l, m;
  double p, r__, s, c2, c3;
  int l1, l2;
  double s2;
  int ii;
  double dl1, el1;
  int mml;
  double tst1, tst2;

  /* 
   * 
   *    this subroutine is a translation of the algol procedure tql2, 
   *    num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and 
   *    wilkinson. 
   *    handbook for auto. comp., vol.ii-linear algebra, 227-240(1971). 
   * 
   *    this subroutine finds the eigenvalues and eigenvectors 
   *    of a symmetric tridiagonal matrix by the ql method. 
   *    the eigenvectors of a full symmetric matrix can also 
   *    be found if  tred2  has been used to reduce this 
   *    full matrix to tridiagonal form. 
   * 
   *!calling sequence 
   * 
   *     subroutine tql2(nm,n,d,e,z,job,ierr) 
   * 
   *    on input 
   * 
   *       nm must be set to the row dimension of two-dimensional 
   *         array parameters as declared in the calling program 
   *         dimension statement. 
   * 
   *       n is the order of the matrix. 
   * 
   *       d contains the diagonal elements of the input matrix. 
   * 
   *       e contains the subdiagonal elements of the input matrix 
   *         in its last n-1 positions.  e(1) is arbitrary. 
   * 
   *       z contains (for job=1) the transformation matrix produced 
   *         in the reduction by  tred2, if performed.  if the eigenvectors 
   *         of the tridiagonal matrix are desired, z must contain 
   *         the identity matrix. If job=0 z is not referenced 
   * 
   *     on output 
   * 
   *       d contains the eigenvalues in ascending order.  if an 
   *         error exit is made, the eigenvalues are correct but 
   *         unordered for indices 1,2,...,ierr-1. 
   * 
   *       e has been destroyed. 
   * 
   *       z contains orthonormal eigenvectors of the symmetric 
   *         tridiagonal (or full) matrix (for job=1).  if an error 
   *         exit is made,z contains the eigenvectors associated with the stored 
   *         eigenvalues. If job=0 z is not referenced 
   * 
   *       ierr is set to 
   *         zero       for normal return, 
   *         j          if the j-th eigenvalue has not been 
   *                    determined after 30*n iterations. 
   * 
   *    calls pythag for  dsqrt(a*a + b*b) . 
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

  /* Function Body */
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
  f = 0.;
  tst1 = 0.;
  e[*n] = 0.;
  /* 
   */
  i__1 = *n;
  for (l = 1; l <= i__1; ++l)
    {
      j = 0;
      h__ = (d__1 = d__[l], Abs (d__1)) + (d__2 = e[l], Abs (d__2));
      if (tst1 < h__)
	{
	  tst1 = h__;
	}
      /*    .......... look for small sub-diagonal element .......... 
       */
      i__2 = *n;
      for (m = l; m <= i__2; ++m)
	{
	  tst2 = tst1 + (d__1 = e[m], Abs (d__1));
	  if (tst2 == tst1)
	    {
	      goto L120;
	    }
	  /*    .......... e(n) is always zero, so there is no exit 
	   *               through the bottom of the loop .......... 
	   */
	  /* L110: */
	}
      /* 
       */
    L120:
      if (m == l)
	{
	  goto L220;
	}
    L130:
      if (j == *n * 30)
	{
	  goto L1000;
	}
      ++j;
      /*    .......... form shift .......... 
       */
      l1 = l + 1;
      l2 = l1 + 1;
      g = d__[l];
      p = (d__[l1] - g) / (e[l] * 2.);
      r__ = nsp_calpack_pythag (&p, &c_b10);
      d__[l] = e[l] / (p + d_sign (&r__, &p));
      d__[l1] = e[l] * (p + d_sign (&r__, &p));
      dl1 = d__[l1];
      h__ = g - d__[l];
      if (l2 > *n)
	{
	  goto L145;
	}
      /* 
       */
      i__2 = *n;
      for (i__ = l2; i__ <= i__2; ++i__)
	{
	  /* L140: */
	  d__[i__] -= h__;
	}
      /* 
       */
    L145:
      f += h__;
      /*    .......... ql transformation .......... 
       */
      p = d__[m];
      c__ = 1.;
      c2 = c__;
      el1 = e[l1];
      s = 0.;
      mml = m - l;
      /*    .......... for i=m-1 step -1 until l do -- .......... 
       */
      i__2 = mml;
      for (ii = 1; ii <= i__2; ++ii)
	{
	  c3 = c2;
	  c2 = c__;
	  s2 = s;
	  i__ = m - ii;
	  g = c__ * e[i__];
	  h__ = c__ * p;
	  r__ = nsp_calpack_pythag (&p, &e[i__]);
	  e[i__ + 1] = s * r__;
	  s = e[i__] / r__;
	  c__ = p / r__;
	  p = c__ * d__[i__] - s * g;
	  d__[i__ + 1] = h__ + s * (c__ * g + s * d__[i__]);
	  /*SS96        test on job added to inhibit z computation 
	   */
	  if (*job == 1)
	    {
	      /*    .......... form vector .......... 
	       */
	      i__3 = *n;
	      for (k = 1; k <= i__3; ++k)
		{
		  h__ = z__[k + (i__ + 1) * z_dim1];
		  z__[k + (i__ + 1) * z_dim1] =
		    s * z__[k + i__ * z_dim1] + c__ * h__;
		  z__[k + i__ * z_dim1] =
		    c__ * z__[k + i__ * z_dim1] - s * h__;
		  /* L180: */
		}
	    }
	  /* 
	   */
	  /* L200: */
	}
      /* 
       */
      p = -s * s2 * c3 * el1 * e[l] / dl1;
      e[l] = s * p;
      d__[l] = c__ * p;
      tst2 = tst1 + (d__1 = e[l], Abs (d__1));
      if (tst2 > tst1)
	{
	  goto L130;
	}
    L220:
      d__[l] += f;
      /* L240: */
    }
  /*    .......... order eigenvalues and eigenvectors .......... 
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
       *SS96    test on job added to inhibit z computation 
       */
      if (*job == 1)
	{
	  i__2 = *n;
	  for (j = 1; j <= i__2; ++j)
	    {
	      p = z__[j + i__ * z_dim1];
	      z__[j + i__ * z_dim1] = z__[j + k * z_dim1];
	      z__[j + k * z_dim1] = p;
	      /* L280: */
	    }
	}
      /* 
       */
    L300:
      ;
    }
  /* 
   */
  goto L1001;
  /*    .......... set error -- no convergence to an 
   *               eigenvalue after 30*n iterations .......... 
   */
 L1000:
  *ierr = l;
 L1001:
  return 0;
}				/* tql2_ */
