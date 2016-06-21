/* MB04IY.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_mb04iy (char *side, char *trans, int *n, int *m, int *k,
		   int *p, double *a, int *lda, double *tau,
		   double *c__, int *ldc, double *dwork,
		   int *ldwork, int *info, long int side_len,
		   long int trans_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, i__1, i__2;

  /* Local variables */
  int left, tran;
  int i__;
  double wrkopt, aii;

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
   *    To overwrite the real n-by-m matrix  C  with  Q' * C,  Q * C, 
   *    C * Q',  or  C * Q,  according to the following table 
   * 
   *                    SIDE = 'L'     SIDE = 'R' 
   *    TRANS = 'N':      Q * C          C * Q 
   *    TRANS = 'T':      Q'* C          C * Q' 
   * 
   *    where  Q  is a real orthogonal matrix defined as the product of 
   *    k elementary reflectors 
   * 
   *       Q = H(1) H(2) . . . H(k) 
   * 
   *    as returned by SLICOT Library routine MB04ID.  Q  is of order n 
   *    if  SIDE = 'L'  and of order m if  SIDE = 'R'. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    SIDE    CHARACTER*1 
   *            Specify if  Q  or  Q'  is applied from the left or right, 
   *            as follows: 
   *            = 'L':  apply  Q  or  Q'  from the left; 
   *            = 'R':  apply  Q  or  Q'  from the right. 
   * 
   *    TRANS   CHARACTER*1 
   *            Specify if  Q  or  Q'  is to be applied, as follows: 
   *            = 'N':  apply  Q   (No transpose); 
   *            = 'T':  apply  Q'  (Transpose). 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The number of rows of the matrix C.  N >= 0. 
   * 
   *    M       (input) INT 
   *            The number of columns of the matrix C.  M >= 0. 
   * 
   *    K       (input) INT 
   *            The number of elementary reflectors whose product defines 
   *            the matrix Q. 
   *            N >= K >= 0,  if  SIDE = 'L'; 
   *            M >= K >= 0,  if  SIDE = 'R'. 
   * 
   *    P       (input) INT 
   *            The order of the zero triagle (or the number of rows of 
   *            the zero trapezoid) in the matrix triangularized by SLICOT 
   *            Library routine MB04ID.  P >= 0. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,K) 
   *            On input, the elements in the rows  i+1:min(n,n-p-1+i)  of 
   *            the  i-th  column, and  TAU(i),  represent the orthogonal 
   *            reflector  H(i),  so that matrix  Q  is the product of 
   *            elementary reflectors:  Q = H(1) H(2) . . . H(k). 
   *            A is modified by the routine but restored on exit. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array  A. 
   *            LDA >= Max(1,N),  if  SIDE = 'L'; 
   *            LDA >= Max(1,M),  if  SIDE = 'R'. 
   * 
   *    TAU     (input) DOUBLE PRECISION array, dimension (K) 
   *            The scalar factors of the elementary reflectors. 
   * 
   *    C       (input/output) DOUBLE PRECISION array, dimension (LDC,M) 
   *            On entry, the leading N-by-M part of this array must 
   *            contain the matrix  C. 
   *            On exit, the leading N-by-M part of this array contains 
   *            the updated matrix C. 
   * 
   *    LDC     INT 
   *            The leading dimension of the array  C.  LDC >= Max(1,N). 
   * 
   *    Workspace 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0, DWORK(1) returns the optimal value 
   *            of LDWORK. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK >= MAX(1,M),  if  SIDE = 'L'; 
   *            LDWORK >= MAX(1,N),  if  SIDE = 'R'. 
   *            For optimum performance LDWORK >= M*NB if SIDE = 'L', 
   *            or LDWORK >= N*NB if SIDE = 'R', where NB is the optimal 
   *            block size. 
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
   *    If  SIDE = 'L',  each elementary reflector  H(i)  modifies 
   *    n-p  elements of each column of  C,  for  i = 1:p+1,  and 
   *    n-i+1  elements, for  i = p+2:k. 
   *    If  SIDE = 'R',  each elementary reflector  H(i)  modifies 
   *    m-p  elements of each row of  C,  for  i = 1:p+1,  and 
   *    m-i+1  elements, for  i = p+2:k. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The implemented method is numerically stable. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Aug. 1999. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Matrix operations, QR decomposition. 
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
   *    Check the scalar input arguments. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --tau;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  --dwork;

  /* Function Body */
  *info = 0;
  left = C2F (lsame) (side, "L", 1L, 1L);
  tran = C2F (lsame) (trans, "T", 1L, 1L);
  /* 
   */
  if (!left && !C2F (lsame) (side, "R", 1L, 1L))
    {
      *info = -1;
    }
  else if (!tran && !C2F (lsame) (trans, "N", 1L, 1L))
    {
      *info = -2;
    }
  else if (*n < 0)
    {
      *info = -3;
    }
  else if (*m < 0)
    {
      *info = -4;
    }
  else if (*k < 0 || left && *k > *n || !left && *k > *m)
    {
      *info = -5;
    }
  else if (*p < 0)
    {
      *info = -6;
    }
  else if (left && *lda < Max (1, *n) || !left && *lda < Max (1, *m))
    {
      *info = -8;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -11;
    }
  else if (left && *ldwork < Max (1, *m) || !left && *ldwork < Max (1, *n))
    {
      *info = -13;
    }
  /* 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("MB04IY", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*m == 0 || *n == 0 || *k == 0 || left && *n < *p || !left && *m < *p)
    {
      dwork[1] = 1.;
      return 0;
    }
  /* 
   *    (Note: Comments in the code beginning "Workspace:" describe the 
   *    minimal amount of real workspace needed at that point in the 
   *    code, as well as the preferred amount for good performance. 
   *    NB refers to the optimal block size for the immediately 
   *    following subroutine, as returned by ILAENV.) 
   * 
   */
  if (left)
    {
      wrkopt = (double) (*m);
      if (tran)
	{
	  /* 
	   */
	  i__1 = Min (*k, *p);
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      /* 
	       *             Apply H(i) to C(i:i+n-p-1,1:m), from the left. 
	       *             Workspace: need M. 
	       * 
	       */
	      aii = a[i__ + i__ * a_dim1];
	      a[i__ + i__ * a_dim1] = 1.;
	      i__2 = *n - *p;
	      C2F (dlarf) (side, &i__2, m, &a[i__ + i__ * a_dim1], &c__1,
			   &tau[i__], &c__[i__ + c_dim1], ldc, &dwork[1], 1L);
	      a[i__ + i__ * a_dim1] = aii;
	      /* L10: */
	    }
	  /* 
	   */
	  if (*p <= Min (*n, *k))
	    {
	      /* 
	       *             Apply H(i) to C, i = p+1:k, from the left. 
	       *             Workspace: need M;  prefer M*NB. 
	       * 
	       */
	      i__1 = *n - *p;
	      i__2 = *k - *p;
	      C2F (dormqr) (side, trans, &i__1, m, &i__2,
			    &a[*p + 1 + (*p + 1) * a_dim1], lda, &tau[*p + 1],
			    &c__[*p + 1 + c_dim1], ldc, &dwork[1], ldwork,
			    &i__, 1L, 1L);
	      wrkopt = Max (wrkopt, dwork[1]);
	    }
	  /* 
	   */
	}
      else
	{
	  /* 
	   */
	  if (*p <= Min (*n, *k))
	    {
	      /* 
	       *             Apply H(i) to C, i = k:p+1:-1, from the left. 
	       *             Workspace: need M;  prefer M*NB. 
	       * 
	       */
	      i__1 = *n - *p;
	      i__2 = *k - *p;
	      C2F (dormqr) (side, trans, &i__1, m, &i__2,
			    &a[*p + 1 + (*p + 1) * a_dim1], lda, &tau[*p + 1],
			    &c__[*p + 1 + c_dim1], ldc, &dwork[1], ldwork,
			    &i__, 1L, 1L);
	      wrkopt = Max (wrkopt, dwork[1]);
	    }
	  /* 
	   */
	  for (i__ = Min (*k, *p); i__ >= 1; --i__)
	    {
	      /* 
	       *             Apply H(i) to C(i:i+n-p-1,1:m), from the left. 
	       *             Workspace: need M. 
	       * 
	       */
	      aii = a[i__ + i__ * a_dim1];
	      a[i__ + i__ * a_dim1] = 1.;
	      i__1 = *n - *p;
	      C2F (dlarf) (side, &i__1, m, &a[i__ + i__ * a_dim1], &c__1,
			   &tau[i__], &c__[i__ + c_dim1], ldc, &dwork[1], 1L);
	      a[i__ + i__ * a_dim1] = aii;
	      /* L20: */
	    }
	}
      /* 
       */
    }
  else
    {
      /* 
       */
      wrkopt = (double) (*n);
      if (tran)
	{
	  /* 
	   */
	  if (*p <= Min (*m, *k))
	    {
	      /* 
	       *             Apply H(i) to C, i = k:p+1:-1, from the right. 
	       *             Workspace: need N;  prefer N*NB. 
	       * 
	       */
	      i__1 = *m - *p;
	      i__2 = *k - *p;
	      C2F (dormqr) (side, trans, n, &i__1, &i__2,
			    &a[*p + 1 + (*p + 1) * a_dim1], lda, &tau[*p + 1],
			    &c__[(*p + 1) * c_dim1 + 1], ldc, &dwork[1],
			    ldwork, &i__, 1L, 1L);
	      wrkopt = Max (wrkopt, dwork[1]);
	    }
	  /* 
	   */
	  for (i__ = Min (*k, *p); i__ >= 1; --i__)
	    {
	      /* 
	       *             Apply H(i) to C(1:n,i:i+m-p-1), from the right. 
	       *             Workspace: need N. 
	       * 
	       */
	      aii = a[i__ + i__ * a_dim1];
	      a[i__ + i__ * a_dim1] = 1.;
	      i__1 = *m - *p;
	      C2F (dlarf) (side, n, &i__1, &a[i__ + i__ * a_dim1], &c__1,
			   &tau[i__], &c__[i__ * c_dim1 + 1], ldc, &dwork[1],
			   1L);
	      a[i__ + i__ * a_dim1] = aii;
	      /* L30: */
	    }
	  /* 
	   */
	}
      else
	{
	  /* 
	   */
	  i__1 = Min (*k, *p);
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      /* 
	       *             Apply H(i) to C(1:n,i:i+m-p-1), from the right. 
	       *             Workspace: need N. 
	       * 
	       */
	      aii = a[i__ + i__ * a_dim1];
	      a[i__ + i__ * a_dim1] = 1.;
	      i__2 = *m - *p;
	      C2F (dlarf) (side, n, &i__2, &a[i__ + i__ * a_dim1], &c__1,
			   &tau[i__], &c__[i__ * c_dim1 + 1], ldc, &dwork[1],
			   1L);
	      a[i__ + i__ * a_dim1] = aii;
	      /* L40: */
	    }
	  /* 
	   */
	  if (*p <= Min (*m, *k))
	    {
	      /* 
	       *             Apply H(i) to C, i = p+1:k, from the right. 
	       *             Workspace: need N;  prefer N*NB. 
	       * 
	       */
	      i__1 = *m - *p;
	      i__2 = *k - *p;
	      C2F (dormqr) (side, trans, n, &i__1, &i__2,
			    &a[*p + 1 + (*p + 1) * a_dim1], lda, &tau[*p + 1],
			    &c__[(*p + 1) * c_dim1 + 1], ldc, &dwork[1],
			    ldwork, &i__, 1L, 1L);
	      wrkopt = Max (wrkopt, dwork[1]);
	    }
	  /* 
	   */
	}
    }
  /* 
   */
  dwork[1] = wrkopt;
  return 0;
  /* 
**** Last line of MB04IY *** 
*/
}				/* nsp_slicot_mb04iy */
