/* SB04QY.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__0 = 0;
static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_sb04qy (int *n, int *m, int *ind, double *a,
		   int *lda, double *b, int *ldb, double *c__,
		   int *ldc, double *d__, int *ipr, int *info)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1;

  /* Local variables */
  int i__, j, k;
  int i2, k1, k2, m1;
  double dum[1];

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
   *    To construct and solve a linear algebraic system of order M whose 
   *    coefficient matrix is in upper Hessenberg form. Such systems 
   *    appear when solving discrete-time Sylvester equations using the 
   *    Hessenberg-Schur method. 
   * 
   *    ARGUMENTS 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrix B.  N >= 0. 
   * 
   *    M       (input) INT 
   *            The order of the matrix A.  M >= 0. 
   * 
   *    IND     (input) INT 
   *            The index of the column in C to be computed.  IND >= 1. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,M) 
   *            The leading M-by-M part of this array must contain an 
   *            upper Hessenberg matrix. 
   * 
   *    LDA     INT 
   *            The leading dimension of array A.  LDA >= MAX(1,M). 
   * 
   *    B       (input) DOUBLE PRECISION array, dimension (LDB,N) 
   *            The leading N-by-N part of this array must contain a 
   *            matrix in real Schur form. 
   * 
   *    LDB     INT 
   *            The leading dimension of array B.  LDB >= MAX(1,N). 
   * 
   *    C       (input/output) DOUBLE PRECISION array, dimension (LDC,N) 
   *            On entry, the leading M-by-N part of this array must 
   *            contain the coefficient matrix C of the equation. 
   *            On exit, the leading M-by-N part of this array contains 
   *            the matrix C with column IND updated. 
   * 
   *    LDC     INT 
   *            The leading dimension of array C.  LDC >= MAX(1,M). 
   * 
   *    Workspace 
   * 
   *    D       DOUBLE PRECISION array, dimension (M*(M+1)/2+2*M) 
   * 
   *    IPR     INT array, dimension (2*M) 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            > 0:  if INFO = IND, a singular matrix was encountered. 
   * 
   *    METHOD 
   * 
   *    A special linear algebraic system of order M, with coefficient 
   *    matrix in upper Hessenberg form is constructed and solved. The 
   *    coefficient matrix is stored compactly, row-wise. 
   * 
   *    REFERENCES 
   * 
   *    [1] Golub, G.H., Nash, S. and Van Loan, C.F. 
   *        A Hessenberg-Schur method for the problem AX + XB = C. 
   *        IEEE Trans. Auto. Contr., AC-24, pp. 909-913, 1979. 
   * 
   *    [2] Sima, V. 
   *        Algorithms for Linear-quadratic Optimization. 
   *        Marcel Dekker, Inc., New York, 1996. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    None. 
   * 
   *    CONTRIBUTORS 
   * 
   *    D. Sima, University of Bucharest, May 2000. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Hessenberg form, orthogonal transformation, real Schur form, 
   *    Sylvester equation. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. Scalar Arguments .. 
   *    .. Array Arguments .. 
   *    .. Local Scalars .. 
   *    .. Local Arrays .. 
   *    .. External Subroutines .. 
   *    .. Executable Statements .. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  --d__;
  --ipr;

  /* Function Body */
  if (*ind < *n)
    {
      dum[0] = 0.;
      C2F (dcopy) (m, dum, &c__0, &d__[1], &c__1);
      i__1 = *n;
      for (i__ = *ind + 1; i__ <= i__1; ++i__)
	{
	  C2F (daxpy) (m, &b[*ind + i__ * b_dim1], &c__[i__ * c_dim1 + 1],
		       &c__1, &d__[1], &c__1);
	  /* L10: */
	}
      i__1 = *m;
      for (i__ = 2; i__ <= i__1; ++i__)
	{
	  c__[i__ + *ind * c_dim1] -=
	    a[i__ + (i__ - 1) * a_dim1] * d__[i__ - 1];
	  /* L20: */
	}
      C2F (dtrmv) ("Upper", "No Transpose", "Non Unit", m, &a[a_offset], lda,
		   &d__[1], &c__1, 5L, 12L, 8L);
      i__1 = *m;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  c__[i__ + *ind * c_dim1] -= d__[i__];
	  /* L30: */
	}
    }
  /* 
   */
  m1 = *m + 1;
  i2 = *m * m1 / 2 + m1;
  k2 = 1;
  k = *m;
  /* 
   *    Construct the linear algebraic system of order M. 
   * 
   */
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      j = m1 - k;
      C2F (dcopy) (&k, &a[i__ + j * a_dim1], lda, &d__[k2], &c__1);
      C2F (dscal) (&k, &b[*ind + *ind * b_dim1], &d__[k2], &c__1);
      k1 = k2;
      k2 += k;
      if (i__ > 1)
	{
	  ++k1;
	  --k;
	}
      d__[k1] += 1.;
      /* 
       *       Store the right hand side. 
       * 
       */
      d__[i2] = c__[i__ + *ind * c_dim1];
      ++i2;
      /* L40: */
    }
  /* 
   *    Solve the linear algebraic system and store the solution in C. 
   * 
   */
  nsp_slicot_sb04mw (m, &d__[1], &ipr[1], info);
  /* 
   */
  if (*info != 0)
    {
      *info = *ind;
    }
  else
    {
      /* 
       */
      i__1 = *m;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  c__[i__ + *ind * c_dim1] = d__[ipr[i__]];
	  /* L50: */
	}
      /* 
       */
    }
  /* 
   */
  return 0;
  /**** Last line of SB04QY *** 
   */
}				/* nsp_slicot_sb04qy */
