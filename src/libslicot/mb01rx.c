/* MB01RX.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b10 = 0.;
static int c__0 = 0;
static double c_b14 = 1.;
static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_mb01rx (char *side, char *uplo, char *trans, int *m, int *n,
		   double *alpha, double *beta, double *r__,
		   int *ldr, double *a, int *lda, double *b,
		   int *ldb, int *info, long int side_len, long int uplo_len,
		   long int trans_len)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, r_dim1, r_offset, i__1, i__2;

  /* Local variables */
  int j;
  int lside;
  int luplo;
  int ltrans;

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
   *    To compute either the upper or lower triangular part of one of the 
   *    matrix formulas 
   *       _ 
   *       R = alpha*R + beta*op( A )*B,                               (1) 
   *       _ 
   *       R = alpha*R + beta*B*op( A ),                               (2) 
   *                                            _ 
   *    where alpha and beta are scalars, R and R are m-by-m matrices, 
   *    op( A ) and B are m-by-n and n-by-m matrices for (1), or n-by-m 
   *    and m-by-n matrices for (2), respectively, and op( A ) is one of 
   * 
   *       op( A ) = A   or   op( A ) = A',  the transpose of A. 
   * 
   *    The result is overwritten on R. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    SIDE    CHARACTER*1 
   *            Specifies whether the matrix A appears on the left or 
   *            right in the matrix product as follows: 
   *                    _ 
   *            = 'L':  R = alpha*R + beta*op( A )*B; 
   *                    _ 
   *            = 'R':  R = alpha*R + beta*B*op( A ). 
   * 
   *    UPLO    CHARACTER*1                               _ 
   *            Specifies which triangles of the matrices R and R are 
   *            computed and given, respectively, as follows: 
   *            = 'U':  the upper triangular part; 
   *            = 'L':  the lower triangular part. 
   * 
   *    TRANS   CHARACTER*1 
   *            Specifies the form of op( A ) to be used in the matrix 
   *            multiplication as follows: 
   *            = 'N':  op( A ) = A; 
   *            = 'T':  op( A ) = A'; 
   *            = 'C':  op( A ) = A'. 
   * 
   *    Input/Output Parameters 
   * 
   *    M       (input) INT           _ 
   *            The order of the matrices R and R, the number of rows of 
   *            the matrix op( A ) and the number of columns of the 
   *            matrix B, for SIDE = 'L', or the number of rows of the 
   *            matrix B and the number of columns of the matrix op( A ), 
   *            for SIDE = 'R'.  M >= 0. 
   * 
   *    N       (input) INT 
   *            The number of rows of the matrix B and the number of 
   *            columns of the matrix op( A ), for SIDE = 'L', or the 
   *            number of rows of the matrix op( A ) and the number of 
   *            columns of the matrix B, for SIDE = 'R'.  N >= 0. 
   * 
   *    ALPHA   (input) DOUBLE PRECISION 
   *            The scalar alpha. When alpha is zero then R need not be 
   *            set before entry. 
   * 
   *    BETA    (input) DOUBLE PRECISION 
   *            The scalar beta. When beta is zero then A and B are not 
   *            referenced. 
   * 
   *    R       (input/output) DOUBLE PRECISION array, dimension (LDR,M) 
   *            On entry with UPLO = 'U', the leading M-by-M upper 
   *            triangular part of this array must contain the upper 
   *            triangular part of the matrix R; the strictly lower 
   *            triangular part of the array is not referenced. 
   *            On entry with UPLO = 'L', the leading M-by-M lower 
   *            triangular part of this array must contain the lower 
   *            triangular part of the matrix R; the strictly upper 
   *            triangular part of the array is not referenced. 
   *            On exit, the leading M-by-M upper triangular part (if 
   *            UPLO = 'U'), or lower triangular part (if UPLO = 'L') of 
   *            this array contains the corresponding triangular part of 
   *                                _ 
   *            the computed matrix R. 
   * 
   *    LDR     INT 
   *            The leading dimension of array R.  LDR >= MAX(1,M). 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,k), where 
   *            k = N  when  SIDE = 'L', and TRANS = 'N', or 
   *                         SIDE = 'R', and TRANS = 'T'; 
   *            k = M  when  SIDE = 'R', and TRANS = 'N', or 
   *                         SIDE = 'L', and TRANS = 'T'. 
   *            On entry, if SIDE = 'L', and TRANS = 'N', or 
   *                         SIDE = 'R', and TRANS = 'T', 
   *            the leading M-by-N part of this array must contain the 
   *            matrix A. 
   *            On entry, if SIDE = 'R', and TRANS = 'N', or 
   *                         SIDE = 'L', and TRANS = 'T', 
   *            the leading N-by-M part of this array must contain the 
   *            matrix A. 
   * 
   *    LDA     INT 
   *            The leading dimension of array A.  LDA >= MAX(1,l), where 
   *            l = M  when  SIDE = 'L', and TRANS = 'N', or 
   *                         SIDE = 'R', and TRANS = 'T'; 
   *            l = N  when  SIDE = 'R', and TRANS = 'N', or 
   *                         SIDE = 'L', and TRANS = 'T'. 
   * 
   *    B       (input) DOUBLE PRECISION array, dimension (LDB,p), where 
   *            p = M  when  SIDE = 'L'; 
   *            p = N  when  SIDE = 'R'. 
   *            On entry, the leading N-by-M part, if SIDE = 'L', or 
   *            M-by-N part, if SIDE = 'R', of this array must contain the 
   *            matrix B. 
   * 
   *    LDB     INT 
   *            The leading dimension of array B. 
   *            LDB >= MAX(1,N), if SIDE = 'L'; 
   *            LDB >= MAX(1,M), if SIDE = 'R'. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value. 
   * 
   *    METHOD 
   * 
   *    The matrix expression is evaluated taking the triangular 
   *    structure into account. BLAS 2 operations are used. A block 
   *    algorithm can be easily constructed; it can use BLAS 3 GEMM 
   *    operations for most computations, and calls of this BLAS 2 
   *    algorithm for computing the triangles. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    The main application of this routine is when the result should 
   *    be a symmetric matrix, e.g., when B = X*op( A )', for (1), or 
   *    B = op( A )'*X, for (2), where B is already available and X = X'. 
   * 
   *    CONTRIBUTORS 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, Feb. 1999. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Mar. 2004. 
   * 
   *    KEYWORDS 
   * 
   *    Elementary matrix operations, matrix algebra, matrix operations. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. Scalar Arguments .. 
   *    .. Array Arguments .. 
   *    .. Local Scalars .. 
   *    .. External Functions .. 
   *    .. External Subroutines .. 
   *    .. Intrinsic Functions .. 
   *    .. Executable Statements .. 
   * 
   *    Test the input scalar arguments. 
   * 
   */
  /* Parameter adjustments */
  r_dim1 = *ldr;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;

  /* Function Body */
  *info = 0;
  lside = C2F (lsame) (side, "L", 1L, 1L);
  luplo = C2F (lsame) (uplo, "U", 1L, 1L);
  ltrans = C2F (lsame) (trans, "T", 1L, 1L)
    || C2F (lsame) (trans, "C", 1L, 1L);
  /* 
   */
  if (!lside && !C2F (lsame) (side, "R", 1L, 1L))
    {
      *info = -1;
    }
  else if (!luplo && !C2F (lsame) (uplo, "L", 1L, 1L))
    {
      *info = -2;
    }
  else if (!ltrans && !C2F (lsame) (trans, "N", 1L, 1L))
    {
      *info = -3;
    }
  else if (*m < 0)
    {
      *info = -4;
    }
  else if (*n < 0)
    {
      *info = -5;
    }
  else if (*ldr < Max (1, *m))
    {
      *info = -9;
    }
  else if (*lda < 1 || (lside && !ltrans || !lside && ltrans) && *lda < *m
	   || (lside && ltrans || !lside && !ltrans) && *lda < *n)
    {
      *info = -11;
    }
  else if (*ldb < 1 || lside && *ldb < *n || !lside && *ldb < *m)
    {
      *info = -13;
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
      C2F (xerbla) ("MB01RX", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*m == 0)
    {
      return 0;
    }
  /* 
   */
  if (*beta == 0. || *n == 0)
    {
      if (*alpha == 0.)
	{
	  /* 
	   *          Special case alpha = 0. 
	   * 
	   */
	  C2F (dlaset) (uplo, m, m, &c_b10, &c_b10, &r__[r_offset], ldr, 1L);
	}
      else
	{
	  /* 
	   *          Special case beta = 0 or N = 0. 
	   * 
	   */
	  if (*alpha != 1.)
	    {
	      C2F (dlascl) (uplo, &c__0, &c__0, &c_b14, alpha, m, m,
			    &r__[r_offset], ldr, info, 1L);
	    }
	}
      return 0;
    }
  /* 
   *    General case: beta <> 0. 
   *    Compute the required triangle of (1) or (2) using BLAS 2 
   *    operations. 
   * 
   */
  if (lside)
    {
      if (luplo)
	{
	  if (ltrans)
	    {
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  C2F (dgemv) (trans, n, &j, beta, &a[a_offset], lda,
			       &b[j * b_dim1 + 1], &c__1, alpha,
			       &r__[j * r_dim1 + 1], &c__1, 1L);
		  /* L10: */
		}
	    }
	  else
	    {
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  C2F (dgemv) (trans, &j, n, beta, &a[a_offset], lda,
			       &b[j * b_dim1 + 1], &c__1, alpha,
			       &r__[j * r_dim1 + 1], &c__1, 1L);
		  /* L20: */
		}
	    }
	}
      else
	{
	  if (ltrans)
	    {
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  i__2 = *m - j + 1;
		  C2F (dgemv) (trans, n, &i__2, beta, &a[j * a_dim1 + 1], lda,
			       &b[j * b_dim1 + 1], &c__1, alpha,
			       &r__[j + j * r_dim1], &c__1, 1L);
		  /* L30: */
		}
	    }
	  else
	    {
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  i__2 = *m - j + 1;
		  C2F (dgemv) (trans, &i__2, n, beta, &a[j + a_dim1], lda,
			       &b[j * b_dim1 + 1], &c__1, alpha,
			       &r__[j + j * r_dim1], &c__1, 1L);
		  /* L40: */
		}
	    }
	}
      /* 
       */
    }
  else
    {
      if (luplo)
	{
	  if (ltrans)
	    {
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  C2F (dgemv) ("NoTranspose", &j, n, beta, &b[b_offset], ldb,
			       &a[j + a_dim1], lda, alpha,
			       &r__[j * r_dim1 + 1], &c__1, 11L);
		  /* L50: */
		}
	    }
	  else
	    {
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  C2F (dgemv) ("NoTranspose", &j, n, beta, &b[b_offset], ldb,
			       &a[j * a_dim1 + 1], &c__1, alpha,
			       &r__[j * r_dim1 + 1], &c__1, 11L);
		  /* L60: */
		}
	    }
	}
      else
	{
	  if (ltrans)
	    {
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  i__2 = *m - j + 1;
		  C2F (dgemv) ("NoTranspose", &i__2, n, beta, &b[j + b_dim1],
			       ldb, &a[j + a_dim1], lda, alpha,
			       &r__[j + j * r_dim1], &c__1, 11L);
		  /* L70: */
		}
	    }
	  else
	    {
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  i__2 = *m - j + 1;
		  C2F (dgemv) ("NoTranspose", &i__2, n, beta, &b[j + b_dim1],
			       ldb, &a[j * a_dim1 + 1], &c__1, alpha,
			       &r__[j + j * r_dim1], &c__1, 11L);
		  /* L80: */
		}
	    }
	}
    }
  /* 
   */
  return 0;
  /**** Last line of MB01RX *** 
   */
}				/* nsp_slicot_mb01rx */
