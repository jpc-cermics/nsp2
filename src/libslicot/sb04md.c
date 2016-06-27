/* SB04MD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static double c_b11 = 1.;
static double c_b12 = 0.;

typedef logical (select_f)(const double *, const double *);

/* Subroutine */ int
nsp_slicot_sb04md (int *n, int *m, double *a, int *lda,
		   double *b, int *ldb, double *c__, int *ldc,
		   double *z__, int *ldz, int *iwork, double *dwork,
		   int *ldwork, int *info)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, z_dim1,
    z_offset, i__1, i__2;

  /* Local variables */
  int ieig, sdim, itau, i__, ifail;
  int bwork[1];
  int jwork;
  /* select_f *select = nsp_slicot_select; */
  int wrkopt, ihi, ind, ilo;

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
   *    To solve for X the continuous-time Sylvester equation 
   * 
   *       AX + XB = C 
   * 
   *    where A, B, C and X are general N-by-N, M-by-M, N-by-M and 
   *    N-by-M matrices respectively. 
   * 
   *    ARGUMENTS 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrix A.  N >= 0. 
   * 
   *    M       (input) INT 
   *            The order of the matrix B.  M >= 0. 
   * 
   *    A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) 
   *            On entry, the leading N-by-N part of this array must 
   *            contain the coefficient matrix A of the equation. 
   *            On exit, the leading N-by-N upper Hessenberg part of this 
   *            array contains the matrix H, and the remainder of the 
   *            leading N-by-N part, together with the elements 2,3,...,N 
   *            of array DWORK, contain the orthogonal transformation 
   *            matrix U (stored in factored form). 
   * 
   *    LDA     INT 
   *            The leading dimension of array A.  LDA >= MAX(1,N). 
   * 
   *    B       (input/output) DOUBLE PRECISION array, dimension (LDB,M) 
   *            On entry, the leading M-by-M part of this array must 
   *            contain the coefficient matrix B of the equation. 
   *            On exit, the leading M-by-M part of this array contains 
   *            the quasi-triangular Schur factor S of the matrix B'. 
   * 
   *    LDB     INT 
   *            The leading dimension of array B.  LDB >= MAX(1,M). 
   * 
   *    C       (input/output) DOUBLE PRECISION array, dimension (LDC,M) 
   *            On entry, the leading N-by-M part of this array must 
   *            contain the coefficient matrix C of the equation. 
   *            On exit, the leading N-by-M part of this array contains 
   *            the solution matrix X of the problem. 
   * 
   *    LDC     INT 
   *            The leading dimension of array C.  LDC >= MAX(1,N). 
   * 
   *    Z       (output) DOUBLE PRECISION array, dimension (LDZ,M) 
   *            The leading M-by-M part of this array contains the 
   *            orthogonal matrix Z used to transform B' to real upper 
   *            Schur form. 
   * 
   *    LDZ     INT 
   *            The leading dimension of array Z.  LDZ >= MAX(1,M). 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (4*N) 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0, DWORK(1) returns the optimal value 
   *            of LDWORK, and DWORK(2), DWORK(3),..., DWORK(N) contain 
   *            the scalar factors of the elementary reflectors used to 
   *            reduce A to upper Hessenberg form, as returned by LAPACK 
   *            Library routine DGEHRD. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK = MAX(1, 2*N*N + 8*N, 5*M, N + M). 
   *            For optimum performance LDWORK should be larger. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            > 0:  if INFO = i, 1 <= i <= M, the QR algorithm failed to 
   *                  compute all the eigenvalues (see LAPACK Library 
   *                  routine DGEES); 
   *            > M:  if a singular matrix was encountered whilst solving 
   *                  for the (INFO-M)-th column of matrix X. 
   * 
   *    METHOD 
   * 
   *    The matrix A is transformed to upper Hessenberg form H = U'AU by 
   *    the orthogonal transformation matrix U; matrix B' is transformed 
   *    to real upper Schur form S = Z'B'Z using the orthogonal 
   *    transformation matrix Z. The matrix C is also multiplied by the 
   *    transformations, F = U'CZ, and the solution matrix Y of the 
   *    transformed system 
   * 
   *       HY + YS' = F 
   * 
   *    is computed by back substitution. Finally, the matrix Y is then 
   *    multiplied by the orthogonal transformation matrices, X = UYZ', in 
   *    order to obtain the solution matrix X to the original problem. 
   * 
   *    REFERENCES 
   * 
   *    [1] Golub, G.H., Nash, S. and Van Loan, C.F. 
   *        A Hessenberg-Schur method for the problem AX + XB = C. 
   *        IEEE Trans. Auto. Contr., AC-24, pp. 909-913, 1979. 
   * 
   *    NUMERICAL ASPECTS 
   *                                        3       3      2         2 
   *    The algorithm requires about (5/3) N  + 10 M  + 5 N M + 2.5 M N 
   *    operations and is backward stable. 
   * 
   *    CONTRIBUTORS 
   * 
   *    Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Aug. 1997. 
   *    Supersedes Release 2.0 routine SB04AD by G. Golub, S. Nash, and 
   *    C. Van Loan, Stanford University, California, United States of 
   *    America, January 1982. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, June 2000, Aug. 2000. 
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
   *    .. Local Scalars .. 
   *    .. Local Arrays .. 
   *    .. External Subroutines .. 
   *    .. Intrinsic Functions .. 
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
  z_dim1 = *ldz;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  --iwork;
  --dwork;

  /* Function Body */
  *info = 0;
  /* 
   *    Test the input scalar arguments. 
   * 
   */
  if (*n < 0)
    {
      *info = -1;
    }
  else if (*m < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (*ldb < Max (1, *m))
    {
      *info = -6;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -8;
    }
  else if (*ldz < Max (1, *m))
    {
      *info = -10;
    }
  else				/* if(complicated condition) */
    {
      /*Computing MAX 
       */
      i__1 = 1, i__2 = (*n << 1) * *n + (*n << 3), i__1 =
	Max(i__1, i__2), i__2 = *m * 5, i__1 = Max (i__1, i__2), i__2 =
	*n + *m;
      if (*ldwork < Max (i__1, i__2))
	{
	  *info = -13;
	}
    }
  /* 
   */
  if (*info != 0)
    {
      /* 
       *       Error return. 
       * 
       */
      i__1 = -(*info);
      C2F (xerbla) ("SB04MD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0 || *m == 0)
    {
      dwork[1] = 1.;
      return 0;
    }
  /* 
   */
  ilo = 1;
  ihi = *n;
  wrkopt = 1;
  /* 
   *    Step 1 : Reduce A to upper Hessenberg and B' to quasi-upper 
   *             triangular. That is, H = U' * A * U (store U in factored 
   *             form) and S = Z' * B' * Z (save Z). 
   * 
   *    (Note: Comments in the code beginning "Workspace:" describe the 
   *    minimal amount of real workspace needed at that point in the 
   *    code, as well as the preferred amount for good performance. 
   *    NB refers to the optimal block size for the immediately 
   *    following subroutine, as returned by ILAENV.) 
   * 
   */
  i__1 = *m;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      i__2 = i__ - 1;
      C2F (dswap) (&i__2, &b[i__ * b_dim1 + 1], &c__1, &b[i__ + b_dim1], ldb);
      /* L20: */
    }
  /* 
   *    Workspace:  need   5*M; 
   *                prefer larger. 
   * 
   */
  ieig = *m + 1;
  jwork = ieig + *m;
  i__1 = *ldwork - jwork + 1;
  C2F (dgees) ("Vectors", "Not ordered", nsp_slicot_select, m, &b[b_offset], ldb, &sdim,
	       &dwork[1], &dwork[ieig], &z__[z_offset], ldz, &dwork[jwork],
	       &i__1, bwork, info, 7L, 11L);
  if (*info != 0)
    {
      return 0;
    }
  /*Computing MAX 
   */
  i__1 = wrkopt, i__2 = (int) dwork[jwork] + jwork - 1;
  wrkopt = Max (i__1, i__2);
  /* 
   *    Workspace:  need   2*N; 
   *                prefer N + N*NB. 
   * 
   */
  itau = 2;
  jwork = itau + *n - 1;
  i__1 = *ldwork - jwork + 1;
  C2F (dgehrd) (n, &ilo, &ihi, &a[a_offset], lda, &dwork[itau], &dwork[jwork],
		&i__1, &ifail);
  /*Computing MAX 
   */
  i__1 = wrkopt, i__2 = (int) dwork[jwork] + jwork - 1;
  wrkopt = Max (i__1, i__2);
  /* 
   *    Step 2 : Form  F = ( U' * C ) * Z.  Use BLAS 3, if enough space. 
   * 
   *    Workspace:  need   N + M; 
   *                prefer N + M*NB. 
   * 
   */
  i__1 = *ldwork - jwork + 1;
  C2F (dormhr) ("Left", "Transpose", n, m, &ilo, &ihi, &a[a_offset], lda,
		&dwork[itau], &c__[c_offset], ldc, &dwork[jwork], &i__1,
		&ifail, 4L, 9L);
  /*Computing MAX 
   */
  i__1 = wrkopt, i__2 = (int) dwork[jwork] + jwork - 1;
  wrkopt = Max (i__1, i__2);
  /* 
   */
  if (*ldwork >= jwork - 1 + *n * *m)
    {
      C2F (dgemm) ("No transpose", "No transpose", n, m, m, &c_b11,
		   &c__[c_offset], ldc, &z__[z_offset], ldz, &c_b12,
		   &dwork[jwork], n, 12L, 12L);
      C2F (dlacpy) ("Full", n, m, &dwork[jwork], n, &c__[c_offset], ldc, 4L);
      /*Computing MAX 
       */
      i__1 = wrkopt, i__2 = jwork - 1 + *n * *m;
      wrkopt = Max (i__1, i__2);
    }
  else
    {
      /* 
       */
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  C2F (dgemv) ("Transpose", m, m, &c_b11, &z__[z_offset], ldz,
		       &c__[i__ + c_dim1], ldc, &c_b12, &dwork[jwork], &c__1,
		       9L);
	  C2F (dcopy) (m, &dwork[jwork], &c__1, &c__[i__ + c_dim1], ldc);
	  /* L40: */
	}
      /* 
       */
    }
  /* 
   */
  ind = *m;
 L60:
  if (ind > 1)
    {
      /* 
       *       Step 3 : Solve  H * Y + Y * S' = F  for  Y. 
       * 
       */
      if (b[ind + (ind - 1) * b_dim1] == 0.)
	{
	  /* 
	   *          Solve a special linear algebraic system of order N. 
	   *          Workspace:  N*(N+1)/2 + 3*N. 
	   * 
	   */
	  nsp_slicot_sb04my (m, n, &ind, &a[a_offset], lda, &b[b_offset], ldb,
			     &c__[c_offset], ldc, &dwork[jwork], &iwork[1],
			     info);
	  /* 
	   */
	  if (*info != 0)
	    {
	      *info += *m;
	      return 0;
	    }
	  /*Computing MAX 
	   */
	  i__1 = wrkopt, i__2 = jwork + *n * (*n + 1) / 2 + (*n << 1) - 1;
	  wrkopt = Max (i__1, i__2);
	  --ind;
	}
      else
	{
	  /* 
	   *          Solve a special linear algebraic system of order 2*N. 
	   *          Workspace:  2*N*N + 8*N; 
	   * 
	   */
	  nsp_slicot_sb04mu (m, n, &ind, &a[a_offset], lda, &b[b_offset], ldb,
			     &c__[c_offset], ldc, &dwork[jwork], &iwork[1],
			     info);
	  /* 
	   */
	  if (*info != 0)
	    {
	      *info += *m;
	      return 0;
	    }
	  /*Computing MAX 
	   */
	  i__1 = wrkopt, i__2 = jwork + (*n << 1) * *n + *n * 7 - 1;
	  wrkopt = Max (i__1, i__2);
	  ind += -2;
	}
      goto L60;
    }
  else if (ind == 1)
    {
      /* 
       *       Solve a special linear algebraic system of order N. 
       *       Workspace:  N*(N+1)/2 + 3*N; 
       * 
       */
      nsp_slicot_sb04my (m, n, &ind, &a[a_offset], lda, &b[b_offset], ldb,
			 &c__[c_offset], ldc, &dwork[jwork], &iwork[1], info);
      if (*info != 0)
	{
	  *info += *m;
	  return 0;
	}
      /*Computing MAX 
       */
      i__1 = wrkopt, i__2 = jwork + *n * (*n + 1) / 2 + (*n << 1) - 1;
      wrkopt = Max (i__1, i__2);
    }
  /* 
   *    Step 4 : Form  C = ( U * Y ) * Z'.  Use BLAS 3, if enough space. 
   * 
   *    Workspace:  need   N + M; 
   *                prefer N + M*NB. 
   * 
   */
  i__1 = *ldwork - jwork + 1;
  C2F (dormhr) ("Left", "No transpose", n, m, &ilo, &ihi, &a[a_offset], lda,
		&dwork[itau], &c__[c_offset], ldc, &dwork[jwork], &i__1,
		&ifail, 4L, 12L);
  /*Computing MAX 
   */
  i__1 = wrkopt, i__2 = (int) dwork[jwork] + jwork - 1;
  wrkopt = Max (i__1, i__2);
  /* 
   */
  if (*ldwork >= jwork - 1 + *n * *m)
    {
      C2F (dgemm) ("No transpose", "Transpose", n, m, m, &c_b11,
		   &c__[c_offset], ldc, &z__[z_offset], ldz, &c_b12,
		   &dwork[jwork], n, 12L, 9L);
      C2F (dlacpy) ("Full", n, m, &dwork[jwork], n, &c__[c_offset], ldc, 4L);
    }
  else
    {
      /* 
       */
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  C2F (dgemv) ("No transpose", m, m, &c_b11, &z__[z_offset], ldz,
		       &c__[i__ + c_dim1], ldc, &c_b12, &dwork[jwork], &c__1,
		       12L);
	  C2F (dcopy) (m, &dwork[jwork], &c__1, &c__[i__ + c_dim1], ldc);
	  /* L80: */
	}
    }
  /* 
   */
  return 0;
  /**** Last line of SB04MD *** 
   */
}				/* nsp_slicot_sb04md */
