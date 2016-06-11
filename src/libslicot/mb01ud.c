/* MB01UD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b9 = 0.;
static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_mb01ud (char *side, char *trans, int *m, int *n,
		   double *alpha, double *h__, int *ldh, double *a,
		   int *lda, double *b, int *ldb, int *info,
		   long int side_len, long int trans_len)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, h_dim1, h_offset, i__1, i__2;
  double d__1;

  /* Local variables */
  int i__, j;
  int lside;
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
   *    To compute one of the matrix products 
   * 
   *       B = alpha*op( H ) * A, or B = alpha*A * op( H ), 
   * 
   *    where alpha is a scalar, A and B are m-by-n matrices, H is an 
   *    upper Hessenberg matrix, and op( H ) is one of 
   * 
   *       op( H ) = H   or   op( H ) = H',  the transpose of H. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    SIDE    CHARACTER*1 
   *            Specifies whether the Hessenberg matrix H appears on the 
   *            left or right in the matrix product as follows: 
   *            = 'L':  B = alpha*op( H ) * A; 
   *            = 'R':  B = alpha*A * op( H ). 
   * 
   *    TRANS   CHARACTER*1 
   *            Specifies the form of op( H ) to be used in the matrix 
   *            multiplication as follows: 
   *            = 'N':  op( H ) = H; 
   *            = 'T':  op( H ) = H'; 
   *            = 'C':  op( H ) = H'. 
   * 
   *    Input/Output Parameters 
   * 
   *    M       (input) INT 
   *            The number of rows of the matrices A and B.  M >= 0. 
   * 
   *    N       (input) INT 
   *            The number of columns of the matrices A and B.  N >= 0. 
   * 
   *    ALPHA   (input) DOUBLE PRECISION 
   *            The scalar alpha. When alpha is zero then H is not 
   *            referenced and A need not be set before entry. 
   * 
   *    H       (input) DOUBLE PRECISION array, dimension (LDH,k) 
   *            where k is M when SIDE = 'L' and is N when SIDE = 'R'. 
   *            On entry with SIDE = 'L', the leading M-by-M upper 
   *            Hessenberg part of this array must contain the upper 
   *            Hessenberg matrix H. 
   *            On entry with SIDE = 'R', the leading N-by-N upper 
   *            Hessenberg part of this array must contain the upper 
   *            Hessenberg matrix H. 
   *            The elements below the subdiagonal are not referenced, 
   *            except possibly for those in the first column, which 
   *            could be overwritten, but are restored on exit. 
   * 
   *    LDH     INT 
   *            The leading dimension of the array H.  LDH >= Max(1,k), 
   *            where k is M when SIDE = 'L' and is N when SIDE = 'R'. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
   *            The leading M-by-N part of this array must contain the 
   *            matrix A. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A.  LDA >= Max(1,M). 
   * 
   *    B       (output) DOUBLE PRECISION array, dimension (LDB,N) 
   *            The leading M-by-N part of this array contains the 
   *            computed product. 
   * 
   *    LDB     INT 
   *            The leading dimension of the array B.  LDB >= Max(1,M). 
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
   *    The required matrix product is computed in two steps. In the first 
   *    step, the upper triangle of H is used; in the second step, the 
   *    contribution of the subdiagonal is added. A fast BLAS 3 DTRMM 
   *    operation is used in the first step. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, January 1999. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Elementary matrix operations, matrix operations. 
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
   * 
   *    .. Executable Statements .. 
   * 
   *    Test the input scalar arguments. 
   * 
   */
  /* Parameter adjustments */
  h_dim1 = *ldh;
  h_offset = h_dim1 + 1;
  h__ -= h_offset;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;

  /* Function Body */
  *info = 0;
  lside = C2F (lsame) (side, "L", 1L, 1L);
  ltrans = C2F (lsame) (trans, "T", 1L, 1L)
    || C2F (lsame) (trans, "C", 1L, 1L);
  /* 
   */
  if (!lside && !C2F (lsame) (side, "R", 1L, 1L))
    {
      *info = -1;
    }
  else if (!ltrans && !C2F (lsame) (trans, "N", 1L, 1L))
    {
      *info = -2;
    }
  else if (*m < 0)
    {
      *info = -3;
    }
  else if (*n < 0)
    {
      *info = -4;
    }
  else if (*ldh < 1 || lside && *ldh < *m || !lside && *ldh < *n)
    {
      *info = -7;
    }
  else if (*lda < Max (1, *m))
    {
      *info = -9;
    }
  else if (*ldb < Max (1, *m))
    {
      *info = -11;
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
      C2F (xerbla) ("MB01UD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return, if possible. 
   * 
   */
  if (Min (*m, *n) == 0)
    {
      return 0;
    }
  /* 
   */
  if (*alpha == 0.)
    {
      /* 
       *       Set B to zero and return. 
       * 
       */
      C2F (dlaset) ("Full", m, n, &c_b9, &c_b9, &b[b_offset], ldb, 4L);
      return 0;
    }
  /* 
   *    Copy A in B and compute one of the matrix products 
   *      B = alpha*op( triu( H ) ) * A, or 
   *      B = alpha*A * op( triu( H ) ), 
   *    involving the upper triangle of H. 
   * 
   */
  C2F (dlacpy) ("Full", m, n, &a[a_offset], lda, &b[b_offset], ldb, 4L);
  C2F (dtrmm) (side, "Upper", trans, "Non-unit", m, n, alpha, &h__[h_offset],
	       ldh, &b[b_offset], ldb, 1L, 5L, 1L, 8L);
  /* 
   *    Add the contribution of the subdiagonal of H. 
   *    If SIDE = 'L', the subdiagonal of H is swapped with the 
   *    corresponding elements in the first column of H, and the 
   *    calculations are organized for column operations. 
   * 
   */
  if (lside)
    {
      if (*m > 2)
	{
	  i__1 = *m - 2;
	  i__2 = *ldh + 1;
	  C2F (dswap) (&i__1, &h__[(h_dim1 << 1) + 3], &i__2,
		       &h__[h_dim1 + 3], &c__1);
	}
      if (ltrans)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *m - 1;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  b[i__ + j * b_dim1] +=
		    *alpha * h__[i__ + 1 + h_dim1] * a[i__ + 1 + j * a_dim1];
		  /* L10: */
		}
	      /* L20: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *m;
	      for (i__ = 2; i__ <= i__2; ++i__)
		{
		  b[i__ + j * b_dim1] +=
		    *alpha * h__[i__ + h_dim1] * a[i__ - 1 + j * a_dim1];
		  /* L30: */
		}
	      /* L40: */
	    }
	}
      if (*m > 2)
	{
	  i__1 = *m - 2;
	  i__2 = *ldh + 1;
	  C2F (dswap) (&i__1, &h__[(h_dim1 << 1) + 3], &i__2,
		       &h__[h_dim1 + 3], &c__1);
	}
      /* 
       */
    }
  else
    {
      /* 
       */
      if (ltrans)
	{
	  i__1 = *n - 1;
	  for (j = 1; j <= i__1; ++j)
	    {
	      if (h__[j + 1 + j * h_dim1] != 0.)
		{
		  d__1 = *alpha * h__[j + 1 + j * h_dim1];
		  C2F (daxpy) (m, &d__1, &a[j * a_dim1 + 1], &c__1,
			       &b[(j + 1) * b_dim1 + 1], &c__1);
		}
	      /* L50: */
	    }
	}
      else
	{
	  i__1 = *n - 1;
	  for (j = 1; j <= i__1; ++j)
	    {
	      if (h__[j + 1 + j * h_dim1] != 0.)
		{
		  d__1 = *alpha * h__[j + 1 + j * h_dim1];
		  C2F (daxpy) (m, &d__1, &a[(j + 1) * a_dim1 + 1], &c__1,
			       &b[j * b_dim1 + 1], &c__1);
		}
	      /* L60: */
	    }
	}
    }
  /* 
   */
  return 0;
  /**** Last line of MB01UD *** 
   */
}				/* nsp_slicot_mb01ud */
