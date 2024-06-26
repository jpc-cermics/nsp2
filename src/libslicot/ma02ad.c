/* MA02AD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Subroutine */ int
nsp_slicot_ma02ad (char *job, int *m, int *n, double *a, int *lda,
		   double *b, int *ldb, long int job_len)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, i__1, i__2;

  /* Local variables */
  int i__, j;

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
   *    To transpose all or part of a two-dimensional matrix A into 
   *    another matrix B. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    JOB     CHARACTER*1 
   *            Specifies the part of the matrix A to be transposed into B 
   *            as follows: 
   *            = 'U': Upper triangular part; 
   *            = 'L': Lower triangular part; 
   *            Otherwise:  All of the matrix A. 
   * 
   *    Input/Output Parameters 
   * 
   *    M      (input) INT 
   *           The number of rows of the matrix A.  M >= 0. 
   * 
   *    N      (input) INT 
   *           The number of columns of the matrix A.  N >= 0. 
   * 
   *    A      (input) DOUBLE PRECISION array, dimension (LDA,N) 
   *           The m-by-n matrix A.  If JOB = 'U', only the upper 
   *           triangle or trapezoid is accessed; if JOB = 'L', only the 
   *           lower triangle or trapezoid is accessed. 
   * 
   *    LDA    INT 
   *           The leading dimension of the array A.  LDA >= Max(1,M). 
   * 
   *    B      (output) DOUBLE PRECISION array, dimension (LDB,M) 
   *           B = A' in the locations specified by JOB. 
   * 
   *    LDB    INT 
   *           The leading dimension of the array B.  LDB >= Max(1,N). 
   * 
   *    CONTRIBUTOR 
   * 
   *    A. Varga, German Aerospace Center, 
   *    DLR Oberpfaffenhofen, March 1998. 
   *    Based on the RASP routine DMTRA. 
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
   *    .. Intrinsic Functions .. 
   * 
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

  /* Function Body */
  if (C2F (lsame) (job, "U", 1L, 1L))
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = Min (j, *m);
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      b[j + i__ * b_dim1] = a[i__ + j * a_dim1];
	      /* L10: */
	    }
	  /* L20: */
	}
    }
  else if (C2F (lsame) (job, "L", 1L, 1L))
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *m;
	  for (i__ = j; i__ <= i__2; ++i__)
	    {
	      b[j + i__ * b_dim1] = a[i__ + j * a_dim1];
	      /* L30: */
	    }
	  /* L40: */
	}
    }
  else
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *m;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      b[j + i__ * b_dim1] = a[i__ + j * a_dim1];
	      /* L50: */
	    }
	  /* L60: */
	}
    }
  /* 
   */
  return 0;
  /**** Last line of MA02AD *** 
   */
}				/* nsp_slicot_ma02ad */
