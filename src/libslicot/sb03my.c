/* SB03MY.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static int c_true = TRUE;
static int c__2 = 2;
static double c_b25 = 1.;
static double c_b29 = 0.;
static int c_false = FALSE;

/* Subroutine */ int
nsp_slicot_sb03my (char *trana, int *n, double *a, int *lda,
		   double *c__, int *ldc, double *scale, int *info,
		   long int trana_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, i__1, i__2, i__3, i__4;
  double d__1, d__2;

  /* Local variables */
  int ierr;
  double smin;
  int j, k, l;
  double x[4] /* was [2][2] */ ;
  int knext, lnext, k1, k2, l1, l2;
  double xnorm;
  int mink1n, mink2n, minl1n, minl2n;
  double a11, db;
  double scaloc;
  double bignum;
  int notrna, lupper;
  double smlnum, da11, vec[4] /* was [2][2] */ , dum[1], eps;

  /* 
   *    SLICOT RELEASE 5.0. 
   * 
   *    Copyright (c) 2002-2009 NICONET e.V. 
   * 
   *    This program is free software: you can redistribute it and/or 
   *    modify it under the terms of the GNU General Public License as 
   *    published by the Free Software Foundation, either version 2 of 
   *    the License, or (at your option) any later version. 
   * 
   *    This program is distributed in the hope that it will be useful, 
   *    but WITHOUT ANY WARRANTY; without even the implied warranty of 
   *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
   *    GNU General Public License for more details. 
   * 
   *    You should have received a copy of the GNU General Public License 
   *    along with this program.  If not, see 
   *    <http://www.gnu.org/licenses/>. 
   * 
   *    PURPOSE 
   * 
   *    To solve the real Lyapunov matrix equation 
   * 
   *           op(A)'*X + X*op(A) = scale*C 
   * 
   *    where op(A) = A or A' (A**T), A is upper quasi-triangular and C is 
   *    symmetric (C = C'). (A' denotes the transpose of the matrix A.) 
   *    A is N-by-N, the right hand side C and the solution X are N-by-N, 
   *    and scale is an output scale factor, set less than or equal to 1 
   *    to avoid overflow in X. The solution matrix X is overwritten 
   *    onto C. 
   * 
   *    A must be in Schur canonical form (as returned by LAPACK routines 
   *    DGEES or DHSEQR), that is, block upper triangular with 1-by-1 and 
   *    2-by-2 diagonal blocks; each 2-by-2 diagonal block has its 
   *    diagonal elements equal and its off-diagonal elements of opposite 
   *    sign. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    TRANA   CHARACTER*1 
   *            Specifies the form of op(A) to be used, as follows: 
   *            = 'N':  op(A) = A    (No transpose); 
   *            = 'T':  op(A) = A**T (Transpose); 
   *            = 'C':  op(A) = A**T (Conjugate transpose = Transpose). 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrices A, X, and C.  N >= 0. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
   *            The leading N-by-N part of this array must contain the 
   *            upper quasi-triangular matrix A, in Schur canonical form. 
   *            The part of A below the first sub-diagonal is not 
   *            referenced. 
   * 
   *    LDA     INT 
   *            The leading dimension of array A.  LDA >= MAX(1,N). 
   * 
   *    C       (input/output) DOUBLE PRECISION array, dimension (LDC,N) 
   *            On entry, the leading N-by-N part of this array must 
   *            contain the symmetric matrix C. 
   *            On exit, if INFO >= 0, the leading N-by-N part of this 
   *            array contains the symmetric solution matrix X. 
   * 
   *    LDC     INT 
   *            The leading dimension of array C.  LDC >= MAX(1,N). 
   * 
   *    SCALE   (output) DOUBLE PRECISION 
   *            The scale factor, scale, set less than or equal to 1 to 
   *            prevent the solution overflowing. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = 1:  if A and -A have common or very close eigenvalues; 
   *                  perturbed values were used to solve the equation 
   *                  (but the matrix A is unchanged). 
   * 
   *    METHOD 
   * 
   *    Bartels-Stewart algorithm is used. A set of equivalent linear 
   *    algebraic systems of equations of order at most four are formed 
   *    and solved using Gaussian elimination with complete pivoting. 
   * 
   *    REFERENCES 
   * 
   *    [1] Bartels, R.H. and Stewart, G.W.  T 
   *        Solution of the matrix equation A X + XB = C. 
   *        Comm. A.C.M., 15, pp. 820-826, 1972. 
   * 
   *    NUMERICAL ASPECTS 
   *                              3 
   *    The algorithm requires 0(N ) operations. 
   * 
   *    CONTRIBUTOR 
   * 
   *    Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, May 1997. 
   *    Supersedes Release 2.0 routine SB03AY by Control Systems Research 
   *    Group, Kingston Polytechnic, United Kingdom, October 1982. 
   *    Based on DTRLYP by P. Petkov, Tech. University of Sofia, September 
   *    1993. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, May 1999. 
   * 
   *    KEYWORDS 
   * 
   *    Continuous-time system, Lyapunov equation, matrix algebra, real 
   *    Schur form. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. 
   *    .. Scalar Arguments .. 
   *    .. 
   *    .. Array Arguments .. 
   *    .. 
   *    .. Local Scalars .. 
   *    .. 
   *    .. Local Arrays .. 
   *    .. 
   *    .. External Functions .. 
   *    .. 
   *    .. External Subroutines .. 
   *    .. 
   *    .. Intrinsic Functions .. 
   *    .. 
   *    .. Executable Statements .. 
   * 
   *    Decode and Test input parameters. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lupper = TRUE;
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -6;
    }
  /* 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("SB03MY", &i__1, 6L);
      return 0;
    }
  /* 
   */
  *scale = 1.;
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Set constants to control overflow. 
   * 
   */
  eps = C2F (dlamch) ("P", 1L);
  smlnum = C2F (dlamch) ("S", 1L);
  bignum = 1. / smlnum;
  C2F (dlabad) (&smlnum, &bignum);
  smlnum = smlnum * (double) (*n * *n) / eps;
  bignum = 1. / smlnum;
  /* 
   *Computing MAX 
   */
  d__1 = smlnum, d__2 =
    eps * C2F (dlanhs) ("Max", n, &a[a_offset], lda, dum, 3L);
  smin = Max (d__1, d__2);
  /* 
   */
  if (notrna)
    {
      /* 
       *       Solve    A'*X + X*A = scale*C. 
       * 
       *       The (K,L)th block of X is determined starting from 
       *       upper-left corner column by column by 
       * 
       *         A(K,K)'*X(K,L) + X(K,L)*A(L,L) = C(K,L) - R(K,L), 
       * 
       *       where 
       *                  K-1                    L-1 
       *         R(K,L) = SUM [A(I,K)'*X(I,L)] + SUM [X(K,J)*A(J,L)]. 
       *                  I=1                    J=1 
       * 
       *       Start column loop (index = L). 
       *       L1 (L2): column index of the first (last) row of X(K,L). 
       * 
       */
      lnext = 1;
      /* 
       */
      i__1 = *n;
      for (l = 1; l <= i__1; ++l)
	{
	  if (l < lnext)
	    {
	      goto L60;
	    }
	  l1 = l;
	  l2 = l;
	  if (l < *n)
	    {
	      if (a[l + 1 + l * a_dim1] != 0.)
		{
		  ++l2;
		}
	      lnext = l2 + 1;
	    }
	  /* 
	   *          Start row loop (index = K). 
	   *          K1 (K2): row index of the first (last) row of X(K,L). 
	   * 
	   */
	  knext = l;
	  /* 
	   */
	  i__2 = *n;
	  for (k = l; k <= i__2; ++k)
	    {
	      if (k < knext)
		{
		  goto L50;
		}
	      k1 = k;
	      k2 = k;
	      if (k < *n)
		{
		  if (a[k + 1 + k * a_dim1] != 0.)
		    {
		      ++k2;
		    }
		  knext = k2 + 1;
		}
	      /* 
	       */
	      if (l1 == l2 && k1 == k2)
		{
		  i__3 = k1 - 1;
		  i__4 = l1 - 1;
		  vec[0] =
		    c__[k1 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__3, &a[k1 * a_dim1 + 1], &c__1,
		      &c__[l1 * c_dim1 + 1], &c__1) + C2F (ddot) (&i__4,
								  &c__[k1 +
								       c_dim1],
								  ldc,
								  &a[l1 *
								     a_dim1 +
								     1],
								  &c__1));
		  scaloc = 1.;
		  /* 
		   */
		  a11 = a[k1 + k1 * a_dim1] + a[l1 + l1 * a_dim1];
		  da11 = Abs (a11);
		  if (da11 <= smin)
		    {
		      a11 = smin;
		      da11 = smin;
		      *info = 1;
		    }
		  db = Abs (vec[0]);
		  if (da11 < 1. && db > 1.)
		    {
		      if (db > bignum * da11)
			{
			  scaloc = 1. / db;
			}
		    }
		  x[0] = vec[0] * scaloc / a11;
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc, &c__[j * c_dim1 + 1],
				       &c__1);
			  /* L10: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		    }
		  /* 
		   */
		}
	      else if (l1 == l2 && k1 != k2)
		{
		  /* 
		   */
		  i__3 = k1 - 1;
		  i__4 = l1 - 1;
		  vec[0] =
		    c__[k1 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__3, &a[k1 * a_dim1 + 1], &c__1,
		      &c__[l1 * c_dim1 + 1], &c__1) + C2F (ddot) (&i__4,
								  &c__[k1 +
								       c_dim1],
								  ldc,
								  &a[l1 *
								     a_dim1 +
								     1],
								  &c__1));
		  /* 
		   */
		  i__3 = k1 - 1;
		  i__4 = l1 - 1;
		  vec[1] =
		    c__[k2 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__3, &a[k2 * a_dim1 + 1], &c__1,
		      &c__[l1 * c_dim1 + 1], &c__1) + C2F (ddot) (&i__4,
								  &c__[k2 +
								       c_dim1],
								  ldc,
								  &a[l1 *
								     a_dim1 +
								     1],
								  &c__1));
		  /* 
		   */
		  d__1 = -a[l1 + l1 * a_dim1];
		  C2F (dlaln2) (&c_true, &c__2, &c__1, &smin, &c_b25,
				&a[k1 + k1 * a_dim1], lda, &c_b25, &c_b25,
				vec, &c__2, &d__1, &c_b29, x, &c__2, &scaloc,
				&xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc, &c__[j * c_dim1 + 1],
				       &c__1);
			  /* L20: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l1 + k2 * c_dim1] = x[1];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 == k2)
		{
		  /* 
		   */
		  i__3 = k1 - 1;
		  i__4 = l1 - 1;
		  vec[0] =
		    c__[k1 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__3, &a[k1 * a_dim1 + 1], &c__1,
		      &c__[l1 * c_dim1 + 1], &c__1) + C2F (ddot) (&i__4,
								  &c__[k1 +
								       c_dim1],
								  ldc,
								  &a[l1 *
								     a_dim1 +
								     1],
								  &c__1));
		  /* 
		   */
		  i__3 = k1 - 1;
		  i__4 = l1 - 1;
		  vec[1] =
		    c__[k1 + l2 * c_dim1] -
		    (C2F (ddot)
		     (&i__3, &a[k1 * a_dim1 + 1], &c__1,
		      &c__[l2 * c_dim1 + 1], &c__1) + C2F (ddot) (&i__4,
								  &c__[k1 +
								       c_dim1],
								  ldc,
								  &a[l2 *
								     a_dim1 +
								     1],
								  &c__1));
		  /* 
		   */
		  d__1 = -a[k1 + k1 * a_dim1];
		  C2F (dlaln2) (&c_true, &c__2, &c__1, &smin, &c_b25,
				&a[l1 + l1 * a_dim1], lda, &c_b25, &c_b25,
				vec, &c__2, &d__1, &c_b29, x, &c__2, &scaloc,
				&xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc, &c__[j * c_dim1 + 1],
				       &c__1);
			  /* L30: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[1];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l2 + k1 * c_dim1] = x[1];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 != k2)
		{
		  /* 
		   */
		  i__3 = k1 - 1;
		  i__4 = l1 - 1;
		  vec[0] =
		    c__[k1 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__3, &a[k1 * a_dim1 + 1], &c__1,
		      &c__[l1 * c_dim1 + 1], &c__1) + C2F (ddot) (&i__4,
								  &c__[k1 +
								       c_dim1],
								  ldc,
								  &a[l1 *
								     a_dim1 +
								     1],
								  &c__1));
		  /* 
		   */
		  i__3 = k1 - 1;
		  i__4 = l1 - 1;
		  vec[2] =
		    c__[k1 + l2 * c_dim1] -
		    (C2F (ddot)
		     (&i__3, &a[k1 * a_dim1 + 1], &c__1,
		      &c__[l2 * c_dim1 + 1], &c__1) + C2F (ddot) (&i__4,
								  &c__[k1 +
								       c_dim1],
								  ldc,
								  &a[l2 *
								     a_dim1 +
								     1],
								  &c__1));
		  /* 
		   */
		  i__3 = k1 - 1;
		  i__4 = l1 - 1;
		  vec[1] =
		    c__[k2 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__3, &a[k2 * a_dim1 + 1], &c__1,
		      &c__[l1 * c_dim1 + 1], &c__1) + C2F (ddot) (&i__4,
								  &c__[k2 +
								       c_dim1],
								  ldc,
								  &a[l1 *
								     a_dim1 +
								     1],
								  &c__1));
		  /* 
		   */
		  i__3 = k1 - 1;
		  i__4 = l1 - 1;
		  vec[3] =
		    c__[k2 + l2 * c_dim1] -
		    (C2F (ddot)
		     (&i__3, &a[k2 * a_dim1 + 1], &c__1,
		      &c__[l2 * c_dim1 + 1], &c__1) + C2F (ddot) (&i__4,
								  &c__[k2 +
								       c_dim1],
								  ldc,
								  &a[l2 *
								     a_dim1 +
								     1],
								  &c__1));
		  /* 
		   */
		  if (k1 == l1)
		    {
		      nsp_slicot_sb03mw (&c_false, &lupper,
					 &a[k1 + k1 * a_dim1], lda, vec,
					 &c__2, &scaloc, x, &c__2, &xnorm,
					 &ierr);
		      if (lupper)
			{
			  x[1] = x[2];
			}
		      else
			{
			  x[2] = x[1];
			}
		    }
		  else
		    {
		      C2F (dlasy2) (&c_true, &c_false, &c__1, &c__2, &c__2,
				    &a[k1 + k1 * a_dim1], lda,
				    &a[l1 + l1 * a_dim1], lda, vec, &c__2,
				    &scaloc, x, &c__2, &xnorm, &ierr);
		    }
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc, &c__[j * c_dim1 + 1],
				       &c__1);
			  /* L40: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[2];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[k2 + l2 * c_dim1] = x[3];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		      c__[l2 + k1 * c_dim1] = x[2];
		      c__[l1 + k2 * c_dim1] = x[1];
		      c__[l2 + k2 * c_dim1] = x[3];
		    }
		}
	      /* 
	       */
	    L50:
	      ;
	    }
	  /* 
	   */
	L60:
	  ;
	}
      /* 
       */
    }
  else
    {
      /* 
       *       Solve    A*X + X*A' = scale*C. 
       * 
       *       The (K,L)th block of X is determined starting from 
       *       bottom-right corner column by column by 
       * 
       *           A(K,K)*X(K,L) + X(K,L)*A(L,L)' = C(K,L) - R(K,L), 
       * 
       *       where 
       *                     N                     N 
       *           R(K,L) = SUM [A(K,I)*X(I,L)] + SUM [X(K,J)*A(L,J)']. 
       *                   I=K+1                 J=L+1 
       * 
       *       Start column loop (index = L). 
       *       L1 (L2): column index of the first (last) row of X(K,L). 
       * 
       */
      lnext = *n;
      /* 
       */
      for (l = *n; l >= 1; --l)
	{
	  if (l > lnext)
	    {
	      goto L120;
	    }
	  l1 = l;
	  l2 = l;
	  if (l > 1)
	    {
	      if (a[l + (l - 1) * a_dim1] != 0.)
		{
		  --l1;
		}
	      lnext = l1 - 1;
	    }
	  /*Computing MIN 
	   */
	  i__1 = l1 + 1;
	  minl1n = Min (i__1, *n);
	  /*Computing MIN 
	   */
	  i__1 = l2 + 1;
	  minl2n = Min (i__1, *n);
	  /* 
	   *          Start row loop (index = K). 
	   *          K1 (K2): row index of the first (last) row of X(K,L). 
	   * 
	   */
	  knext = l;
	  /* 
	   */
	  for (k = l; k >= 1; --k)
	    {
	      if (k > knext)
		{
		  goto L110;
		}
	      k1 = k;
	      k2 = k;
	      if (k > 1)
		{
		  if (a[k + (k - 1) * a_dim1] != 0.)
		    {
		      --k1;
		    }
		  knext = k1 - 1;
		}
	      /*Computing MIN 
	       */
	      i__1 = k1 + 1;
	      mink1n = Min (i__1, *n);
	      /*Computing MIN 
	       */
	      i__1 = k2 + 1;
	      mink2n = Min (i__1, *n);
	      /* 
	       */
	      if (l1 == l2 && k1 == k2)
		{
		  i__1 = *n - k1;
		  i__2 = *n - l1;
		  vec[0] =
		    c__[k1 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__1, &a[k1 + mink1n * a_dim1], lda,
		      &c__[mink1n + l1 * c_dim1], &c__1) + C2F (ddot) (&i__2,
								       &c__[k1
									    +
									    minl1n
									    *
									    c_dim1],
								       ldc,
								       &a[l1 +
									  minl1n
									  *
									  a_dim1],
								       lda));
		  scaloc = 1.;
		  /* 
		   */
		  a11 = a[k1 + k1 * a_dim1] + a[l1 + l1 * a_dim1];
		  da11 = Abs (a11);
		  if (da11 <= smin)
		    {
		      a11 = smin;
		      da11 = smin;
		      *info = 1;
		    }
		  db = Abs (vec[0]);
		  if (da11 < 1. && db > 1.)
		    {
		      if (db > bignum * da11)
			{
			  scaloc = 1. / db;
			}
		    }
		  x[0] = vec[0] * scaloc / a11;
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc, &c__[j * c_dim1 + 1],
				       &c__1);
			  /* L70: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		    }
		  /* 
		   */
		}
	      else if (l1 == l2 && k1 != k2)
		{
		  /* 
		   */
		  i__1 = *n - k2;
		  i__2 = *n - l2;
		  vec[0] =
		    c__[k1 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__1, &a[k1 + mink2n * a_dim1], lda,
		      &c__[mink2n + l1 * c_dim1], &c__1) + C2F (ddot) (&i__2,
								       &c__[k1
									    +
									    minl2n
									    *
									    c_dim1],
								       ldc,
								       &a[l1 +
									  minl2n
									  *
									  a_dim1],
								       lda));
		  /* 
		   */
		  i__1 = *n - k2;
		  i__2 = *n - l2;
		  vec[1] =
		    c__[k2 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__1, &a[k2 + mink2n * a_dim1], lda,
		      &c__[mink2n + l1 * c_dim1], &c__1) + C2F (ddot) (&i__2,
								       &c__[k2
									    +
									    minl2n
									    *
									    c_dim1],
								       ldc,
								       &a[l1 +
									  minl2n
									  *
									  a_dim1],
								       lda));
		  /* 
		   */
		  d__1 = -a[l1 + l1 * a_dim1];
		  C2F (dlaln2) (&c_false, &c__2, &c__1, &smin, &c_b25,
				&a[k1 + k1 * a_dim1], lda, &c_b25, &c_b25,
				vec, &c__2, &d__1, &c_b29, x, &c__2, &scaloc,
				&xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc, &c__[j * c_dim1 + 1],
				       &c__1);
			  /* L80: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l1 + k2 * c_dim1] = x[1];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 == k2)
		{
		  /* 
		   */
		  i__1 = *n - k1;
		  i__2 = *n - l2;
		  vec[0] =
		    c__[k1 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__1, &a[k1 + mink1n * a_dim1], lda,
		      &c__[mink1n + l1 * c_dim1], &c__1) + C2F (ddot) (&i__2,
								       &c__[k1
									    +
									    minl2n
									    *
									    c_dim1],
								       ldc,
								       &a[l1 +
									  minl2n
									  *
									  a_dim1],
								       lda));
		  /* 
		   */
		  i__1 = *n - k1;
		  i__2 = *n - l2;
		  vec[1] =
		    c__[k1 + l2 * c_dim1] -
		    (C2F (ddot)
		     (&i__1, &a[k1 + mink1n * a_dim1], lda,
		      &c__[mink1n + l2 * c_dim1], &c__1) + C2F (ddot) (&i__2,
								       &c__[k1
									    +
									    minl2n
									    *
									    c_dim1],
								       ldc,
								       &a[l2 +
									  minl2n
									  *
									  a_dim1],
								       lda));
		  /* 
		   */
		  d__1 = -a[k1 + k1 * a_dim1];
		  C2F (dlaln2) (&c_false, &c__2, &c__1, &smin, &c_b25,
				&a[l1 + l1 * a_dim1], lda, &c_b25, &c_b25,
				vec, &c__2, &d__1, &c_b29, x, &c__2, &scaloc,
				&xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc, &c__[j * c_dim1 + 1],
				       &c__1);
			  /* L90: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[1];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l2 + k1 * c_dim1] = x[1];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 != k2)
		{
		  /* 
		   */
		  i__1 = *n - k2;
		  i__2 = *n - l2;
		  vec[0] =
		    c__[k1 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__1, &a[k1 + mink2n * a_dim1], lda,
		      &c__[mink2n + l1 * c_dim1], &c__1) + C2F (ddot) (&i__2,
								       &c__[k1
									    +
									    minl2n
									    *
									    c_dim1],
								       ldc,
								       &a[l1 +
									  minl2n
									  *
									  a_dim1],
								       lda));
		  /* 
		   */
		  i__1 = *n - k2;
		  i__2 = *n - l2;
		  vec[2] =
		    c__[k1 + l2 * c_dim1] -
		    (C2F (ddot)
		     (&i__1, &a[k1 + mink2n * a_dim1], lda,
		      &c__[mink2n + l2 * c_dim1], &c__1) + C2F (ddot) (&i__2,
								       &c__[k1
									    +
									    minl2n
									    *
									    c_dim1],
								       ldc,
								       &a[l2 +
									  minl2n
									  *
									  a_dim1],
								       lda));
		  /* 
		   */
		  i__1 = *n - k2;
		  i__2 = *n - l2;
		  vec[1] =
		    c__[k2 + l1 * c_dim1] -
		    (C2F (ddot)
		     (&i__1, &a[k2 + mink2n * a_dim1], lda,
		      &c__[mink2n + l1 * c_dim1], &c__1) + C2F (ddot) (&i__2,
								       &c__[k2
									    +
									    minl2n
									    *
									    c_dim1],
								       ldc,
								       &a[l1 +
									  minl2n
									  *
									  a_dim1],
								       lda));
		  /* 
		   */
		  i__1 = *n - k2;
		  i__2 = *n - l2;
		  vec[3] =
		    c__[k2 + l2 * c_dim1] -
		    (C2F (ddot)
		     (&i__1, &a[k2 + mink2n * a_dim1], lda,
		      &c__[mink2n + l2 * c_dim1], &c__1) + C2F (ddot) (&i__2,
								       &c__[k2
									    +
									    minl2n
									    *
									    c_dim1],
								       ldc,
								       &a[l2 +
									  minl2n
									  *
									  a_dim1],
								       lda));
		  /* 
		   */
		  if (k1 == l1)
		    {
		      nsp_slicot_sb03mw (&c_true, &lupper,
					 &a[k1 + k1 * a_dim1], lda, vec,
					 &c__2, &scaloc, x, &c__2, &xnorm,
					 &ierr);
		      if (lupper)
			{
			  x[1] = x[2];
			}
		      else
			{
			  x[2] = x[1];
			}
		    }
		  else
		    {
		      C2F (dlasy2) (&c_false, &c_true, &c__1, &c__2, &c__2,
				    &a[k1 + k1 * a_dim1], lda,
				    &a[l1 + l1 * a_dim1], lda, vec, &c__2,
				    &scaloc, x, &c__2, &xnorm, &ierr);
		    }
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc, &c__[j * c_dim1 + 1],
				       &c__1);
			  /* L100: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[2];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[k2 + l2 * c_dim1] = x[3];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		      c__[l2 + k1 * c_dim1] = x[2];
		      c__[l1 + k2 * c_dim1] = x[1];
		      c__[l2 + k2 * c_dim1] = x[3];
		    }
		}
	      /* 
	       */
	    L110:
	      ;
	    }
	  /* 
	   */
	L120:
	  ;
	}
      /* 
       */
    }
  /* 
   */
  return 0;
  /**** Last line of SB03MY *** 
   */
}				/* nsp_slicot_sb03my */
