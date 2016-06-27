/* SB04RW.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static double c_b7 = 1.;
static double c_b9 = 0.;
static double c_b12 = -1.;

/* Subroutine */ int
nsp_slicot_sb04rw (char *abschr, char *ul, int *n, int *m, double *c__,
		   int *ldc, int *indx, double *ab, int *ldab,
		   double *ba, int *ldba, double *d__,
		   double *dwork, long int abschr_len, long int ul_len)
{
  /* System generated locals */
  int ab_dim1, ab_offset, ba_dim1, ba_offset, c_dim1, c_offset, i__1;

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
   *    To construct the right-hand side D for a system of equations in 
   *    Hessenberg form solved via SB04RY (case with 1 right-hand side). 
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
   *            the Sylvester equation X + AXB = C, and both the currently 
   *            computed part of the solution of the Sylvester equation. 
   * 
   *    LDC     INT 
   *            The leading dimension of array C.  LDC >= MAX(1,N). 
   * 
   *    INDX    (input) INT 
   *            The position of the column/row of C to be used in the 
   *            construction of the right-hand side D. 
   * 
   *    AB      (input) DOUBLE PRECISION array, dimension (LDAB,*) 
   *            The leading N-by-N or M-by-M part of this array must 
   *            contain either A or B of the Sylvester equation 
   *            X + AXB = C. 
   * 
   *    LDAB    INT 
   *            The leading dimension of array AB. 
   *            LDAB >= MAX(1,N) or LDAB >= MAX(1,M) (depending on 
   *            ABSCHR = 'A' or ABSCHR = 'B', respectively). 
   * 
   *    BA      (input) DOUBLE PRECISION array, dimension (LDBA,*) 
   *            The leading N-by-N or M-by-M part of this array must 
   *            contain either A or B of the Sylvester equation 
   *            X + AXB = C, the matrix not contained in AB. 
   * 
   *    LDBA    INT 
   *            The leading dimension of array BA. 
   *            LDBA >= MAX(1,N) or LDBA >= MAX(1,M) (depending on 
   *            ABSCHR = 'B' or ABSCHR = 'A', respectively). 
   * 
   *    D       (output) DOUBLE PRECISION array, dimension (*) 
   *            The leading N or M part of this array (depending on 
   *            ABSCHR = 'B' or ABSCHR = 'A', respectively) contains the 
   *            right-hand side. 
   * 
   *    Workspace 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            where LDWORK is equal to N or M (depending on ABSCHR = 'B' 
   *            or ABSCHR = 'A', respectively). 
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
  ba_dim1 = *ldba;
  ba_offset = ba_dim1 + 1;
  ba -= ba_offset;
  --d__;
  --dwork;

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
       *       Construct the column of the right-hand side. 
       * 
       */
      C2F (dcopy) (n, &c__[*indx * c_dim1 + 1], &c__1, &d__[1], &c__1);
      if (C2F (lsame) (ul, "U", 1L, 1L))
	{
	  if (*indx > 1)
	    {
	      i__1 = *indx - 1;
	      C2F (dgemv) ("N", n, &i__1, &c_b7, &c__[c_offset], ldc,
			   &ab[*indx * ab_dim1 + 1], &c__1, &c_b9, &dwork[1],
			   &c__1, 1L);
	      C2F (dgemv) ("N", n, n, &c_b12, &ba[ba_offset], ldba, &dwork[1],
			   &c__1, &c_b7, &d__[1], &c__1, 1L);
	    }
	}
      else
	{
	  if (*indx < *m)
	    {
	      i__1 = *m - *indx;
	      C2F (dgemv) ("N", n, &i__1, &c_b7,
			   &c__[(*indx + 1) * c_dim1 + 1], ldc,
			   &ab[*indx + 1 + *indx * ab_dim1], &c__1, &c_b9,
			   &dwork[1], &c__1, 1L);
	      C2F (dgemv) ("N", n, n, &c_b12, &ba[ba_offset], ldba, &dwork[1],
			   &c__1, &c_b7, &d__[1], &c__1, 1L);
	    }
	}
    }
  else
    {
      /* 
       *       Construct the row of the right-hand side. 
       * 
       */
      C2F (dcopy) (m, &c__[*indx + c_dim1], ldc, &d__[1], &c__1);
      if (C2F (lsame) (ul, "U", 1L, 1L))
	{
	  if (*indx < *n)
	    {
	      i__1 = *n - *indx;
	      C2F (dgemv) ("T", &i__1, m, &c_b7, &c__[*indx + 1 + c_dim1],
			   ldc, &ab[*indx + (*indx + 1) * ab_dim1], ldab,
			   &c_b9, &dwork[1], &c__1, 1L);
	      C2F (dgemv) ("T", m, m, &c_b12, &ba[ba_offset], ldba, &dwork[1],
			   &c__1, &c_b7, &d__[1], &c__1, 1L);
	    }
	}
      else
	{
	  if (*indx > 1)
	    {
	      i__1 = *indx - 1;
	      C2F (dgemv) ("T", &i__1, m, &c_b7, &c__[c_offset], ldc,
			   &ab[*indx + ab_dim1], ldab, &c_b9, &dwork[1],
			   &c__1, 1L);
	      C2F (dgemv) ("T", m, m, &c_b12, &ba[ba_offset], ldba, &dwork[1],
			   &c__1, &c_b7, &d__[1], &c__1, 1L);
	    }
	}
    }
  /* 
   */
  return 0;
  /**** Last line of SB04RW *** 
   */
}				/* nsp_slicot_sb04rw */
