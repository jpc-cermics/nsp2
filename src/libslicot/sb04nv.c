/* SB04NV.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static int c__2 = 2;
static double c_b9 = -1.;
static double c_b11 = 1.;

/* Subroutine */ int
nsp_slicot_sb04nv (char *abschr, char *ul, int *n, int *m, double *c__,
		   int *ldc, int *indx, double *ab, int *ldab,
		   double *d__, long int abschr_len, long int ul_len)
{
  /* System generated locals */
  int ab_dim1, ab_offset, c_dim1, c_offset, i__1;

  /* Local variables */

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
   *    To construct the right-hand sides D for a system of equations in 
   *    Hessenberg form solved via SB04NX (case with 2 right-hand sides). 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    ABSCHR  CHARACTER*1 
   *            Indicates whether AB contains A or B, as follows: 
   *            = 'A':  AB contains A; 
   *            = 'B':  AB contains B. 
   * 
   *    UL      CHARACTER*1 
   *            Indicates whether AB is upper or lower Hessenberg matrix, 
   *            as follows: 
   *            = 'U':  AB is upper Hessenberg; 
   *            = 'L':  AB is lower Hessenberg. 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrix A.  N >= 0. 
   * 
   *    M       (input) INT 
   *            The order of the matrix B.  M >= 0. 
   * 
   *    C       (input) DOUBLE PRECISION array, dimension (LDC,M) 
   *            The leading N-by-M part of this array must contain both 
   *            the not yet modified part of the coefficient matrix C of 
   *            the Sylvester equation AX + XB = C, and both the currently 
   *            computed part of the solution of the Sylvester equation. 
   * 
   *    LDC     INT 
   *            The leading dimension of array C.  LDC >= MAX(1,N). 
   * 
   *    INDX    (input) INT 
   *            The position of the first column/row of C to be used in 
   *            the construction of the right-hand side D. 
   * 
   *    AB      (input) DOUBLE PRECISION array, dimension (LDAB,*) 
   *            The leading N-by-N or M-by-M part of this array must 
   *            contain either A or B of the Sylvester equation 
   *            AX + XB = C. 
   * 
   *    LDAB    INT 
   *            The leading dimension of array AB. 
   *            LDAB >= MAX(1,N) or LDAB >= MAX(1,M) (depending on 
   *            ABSCHR = 'A' or ABSCHR = 'B', respectively). 
   * 
   *    D       (output) DOUBLE PRECISION array, dimension (*) 
   *            The leading 2*N or 2*M part of this array (depending on 
   *            ABSCHR = 'B' or ABSCHR = 'A', respectively) contains the 
   *            right-hand side stored as a matrix with two rows. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    None. 
   * 
   *    CONTRIBUTORS 
   * 
   *    Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Aug. 1997. 
   *    Supersedes Release 2.0 routine SB04BV by M. Vanbegin, and 
   *    P. Van Dooren, Philips Research Laboratory, Brussels, Belgium. 
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
   *    .. External Functions .. 
   *    .. External Subroutines .. 
   *    .. Executable Statements .. 
   * 
   *    For speed, no tests on the input scalar arguments are made. 
   *    Quick return if possible. 
   * 
   */
  /* Parameter adjustments */
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  ab_dim1 = *ldab;
  ab_offset = ab_dim1 + 1;
  ab -= ab_offset;
  --d__;

  /* Function Body */
  if (*n == 0 || *m == 0)
    {
      return 0;
    }
  /* 
   */
  if (C2F (lsame) (abschr, "B", 1L, 1L))
    {
      /* 
       *       Construct the 2 columns of the right-hand side. 
       * 
       */
      C2F (dcopy) (n, &c__[*indx * c_dim1 + 1], &c__1, &d__[1], &c__2);
      C2F (dcopy) (n, &c__[(*indx + 1) * c_dim1 + 1], &c__1, &d__[2], &c__2);
      if (C2F (lsame) (ul, "U", 1L, 1L))
	{
	  if (*indx > 1)
	    {
	      i__1 = *indx - 1;
	      C2F (dgemv) ("N", n, &i__1, &c_b9, &c__[c_offset], ldc,
			   &ab[*indx * ab_dim1 + 1], &c__1, &c_b11, &d__[1],
			   &c__2, 1L);
	      i__1 = *indx - 1;
	      C2F (dgemv) ("N", n, &i__1, &c_b9, &c__[c_offset], ldc,
			   &ab[(*indx + 1) * ab_dim1 + 1], &c__1, &c_b11,
			   &d__[2], &c__2, 1L);
	    }
	}
      else
	{
	  if (*indx < *m - 1)
	    {
	      i__1 = *m - *indx - 1;
	      C2F (dgemv) ("N", n, &i__1, &c_b9,
			   &c__[(*indx + 2) * c_dim1 + 1], ldc,
			   &ab[*indx + 2 + *indx * ab_dim1], &c__1, &c_b11,
			   &d__[1], &c__2, 1L);
	      i__1 = *m - *indx - 1;
	      C2F (dgemv) ("N", n, &i__1, &c_b9,
			   &c__[(*indx + 2) * c_dim1 + 1], ldc,
			   &ab[*indx + 2 + (*indx + 1) * ab_dim1], &c__1,
			   &c_b11, &d__[2], &c__2, 1L);
	    }
	}
    }
  else
    {
      /* 
       *       Construct the 2 rows of the right-hand side. 
       * 
       */
      C2F (dcopy) (m, &c__[*indx + c_dim1], ldc, &d__[1], &c__2);
      C2F (dcopy) (m, &c__[*indx + 1 + c_dim1], ldc, &d__[2], &c__2);
      if (C2F (lsame) (ul, "U", 1L, 1L))
	{
	  if (*indx < *n - 1)
	    {
	      i__1 = *n - *indx - 1;
	      C2F (dgemv) ("T", &i__1, m, &c_b9, &c__[*indx + 2 + c_dim1],
			   ldc, &ab[*indx + (*indx + 2) * ab_dim1], ldab,
			   &c_b11, &d__[1], &c__2, 1L);
	      i__1 = *n - *indx - 1;
	      C2F (dgemv) ("T", &i__1, m, &c_b9, &c__[*indx + 2 + c_dim1],
			   ldc, &ab[*indx + 1 + (*indx + 2) * ab_dim1], ldab,
			   &c_b11, &d__[2], &c__2, 1L);
	    }
	}
      else
	{
	  if (*indx > 1)
	    {
	      i__1 = *indx - 1;
	      C2F (dgemv) ("T", &i__1, m, &c_b9, &c__[c_offset], ldc,
			   &ab[*indx + ab_dim1], ldab, &c_b11, &d__[1], &c__2,
			   1L);
	      i__1 = *indx - 1;
	      C2F (dgemv) ("T", &i__1, m, &c_b9, &c__[c_offset], ldc,
			   &ab[*indx + 1 + ab_dim1], ldab, &c_b11, &d__[2],
			   &c__2, 1L);
	    }
	}
    }
  /* 
   */
  return 0;
  /**** Last line of SB04NV *** 
   */
}				/* nsp_slicot_sb04nv */
