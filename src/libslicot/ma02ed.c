/* MA02ED.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_ma02ed (char *uplo, int *n, double *a, int *lda, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;

  /* Local variables */
  int j;

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
   *    To store by symmetry the upper or lower triangle of a symmetric 
   *    matrix, given the other triangle. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    UPLO    CHARACTER*1 
   *            Specifies which part of the matrix is given as follows: 
   *            = 'U':  Upper triangular part; 
   *            = 'L':  Lower triangular part. 
   *            For all other values, the array A is not referenced. 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrix A.  N >= 0. 
   * 
   *    A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) 
   *            On entry, the leading N-by-N upper triangular part 
   *            (if UPLO = 'U'), or lower triangular part (if UPLO = 'L'), 
   *            of this array must contain the corresponding upper or 
   *            lower triangle of the symmetric matrix A. 
   *            On exit, the leading N-by-N part of this array contains 
   *            the symmetric matrix A with all elements stored. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A.  LDA >= Max(1,N). 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Romania, 
   *    Oct. 1998. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    ****************************************************************** 
   * 
   *    .. Scalar Arguments .. 
   *    .. Array Arguments .. 
   *    .. Local Scalars .. 
   *    .. External Functions .. 
   *    .. External Subroutines .. 
   * 
   *    .. Executable Statements .. 
   * 
   *    For efficiency reasons, the parameters are not checked for errors. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  if (C2F (lsame) (uplo, "L", 1L, 1L))
    {
      /* 
       *       Construct the upper triangle of A. 
       * 
       */
      i__1 = *n;
      for (j = 2; j <= i__1; ++j)
	{
	  i__2 = j - 1;
	  C2F (dcopy) (&i__2, &a[j + a_dim1], lda, &a[j * a_dim1 + 1], &c__1);
	  /* L20: */
	}
      /* 
       */
    }
  else if (C2F (lsame) (uplo, "U", 1L, 1L))
    {
      /* 
       *       Construct the lower triangle of A. 
       * 
       */
      i__1 = *n;
      for (j = 2; j <= i__1; ++j)
	{
	  i__2 = j - 1;
	  C2F (dcopy) (&i__2, &a[j * a_dim1 + 1], &c__1, &a[j + a_dim1], lda);
	  /* L40: */
	}
      /* 
       */
    }
  return 0;
  /**** Last line of MA02ED *** 
   */
}				/* nsp_slicot_ma02ed */
