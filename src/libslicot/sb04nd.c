/* SB04ND.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static int c__2 = 2;

/* Subroutine */ int
nsp_slicot_sb04nd (char *abschu, char *ula, char *ulb, int *n, int *m,
		   double *a, int *lda, double *b, int *ldb,
		   double *c__, int *ldc, double *tol, int *iwork,
		   double *dwork, int *ldwork, int *info,
		   long int abschu_len, long int ula_len, long int ulb_len)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1;

  /* Local variables */
  int ibeg, iend, incr;
  int lula, lulb;
  int i__;
  double scale;
  int maxmn;
  int istep, jwork;
  int labscb;
  char abschr[1];
  int labscs;
  int ipincr;
  int fwd, ldw;
  double tol1;

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
   *       AX + XB = C, 
   * 
   *    with at least one of the matrices A or B in Schur form and the 
   *    other in Hessenberg or Schur form (both either upper or lower); 
   *    A, B, C and X are N-by-N, M-by-M, N-by-M, and N-by-M matrices, 
   *    respectively. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    ABSCHU  CHARACTER*1 
   *            Indicates whether A and/or B is/are in Schur or 
   *            Hessenberg form as follows: 
   *            = 'A':  A is in Schur form, B is in Hessenberg form; 
   *            = 'B':  B is in Schur form, A is in Hessenberg form; 
   *            = 'S':  Both A and B are in Schur form. 
   * 
   *    ULA     CHARACTER*1 
   *            Indicates whether A is in upper or lower Schur form or 
   *            upper or lower Hessenberg form as follows: 
   *            = 'U':  A is in upper Hessenberg form if ABSCHU = 'B' and 
   *                    upper Schur form otherwise; 
   *            = 'L':  A is in lower Hessenberg form if ABSCHU = 'B' and 
   *                    lower Schur form otherwise. 
   * 
   *    ULB     CHARACTER*1 
   *            Indicates whether B is in upper or lower Schur form or 
   *            upper or lower Hessenberg form as follows: 
   *            = 'U':  B is in upper Hessenberg form if ABSCHU = 'A' and 
   *                    upper Schur form otherwise; 
   *            = 'L':  B is in lower Hessenberg form if ABSCHU = 'A' and 
   *                    lower Schur form otherwise. 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrix A.  N >= 0. 
   * 
   *    M       (input) INT 
   *            The order of the matrix B.  M >= 0. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
   *            The leading N-by-N part of this array must contain the 
   *            coefficient matrix A of the equation. 
   * 
   *    LDA     INT 
   *            The leading dimension of array A.  LDA >= MAX(1,N). 
   * 
   *    B       (input) DOUBLE PRECISION array, dimension (LDB,M) 
   *            The leading M-by-M part of this array must contain the 
   *            coefficient matrix B of the equation. 
   * 
   *    LDB     INT 
   *            The leading dimension of array B.  LDB >= MAX(1,M). 
   * 
   *    C       (input/output) DOUBLE PRECISION array, dimension (LDC,M) 
   *            On entry, the leading N-by-M part of this array must 
   *            contain the coefficient matrix C of the equation. 
   *            On exit, if INFO = 0, the leading N-by-M part of this 
   *            array contains the solution matrix X of the problem. 
   * 
   *    LDC     INT 
   *            The leading dimension of array C.  LDC >= MAX(1,N). 
   * 
   *    Tolerances 
   * 
   *    TOL     DOUBLE PRECISION 
   *            The tolerance to be used to test for near singularity in 
   *            the Sylvester equation. If the user sets TOL > 0, then the 
   *            given value of TOL is used as a lower bound for the 
   *            reciprocal condition number; a matrix whose estimated 
   *            condition number is less than 1/TOL is considered to be 
   *            nonsingular. If the user sets TOL <= 0, then a default 
   *            tolerance, defined by TOLDEF = EPS, is used instead, where 
   *            EPS is the machine precision (see LAPACK Library routine 
   *            DLAMCH). 
   *            This parameter is not referenced if ABSCHU = 'S', 
   *            ULA = 'U', and ULB = 'U'. 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (2*MAX(M,N)) 
   *            This parameter is not referenced if ABSCHU = 'S', 
   *            ULA = 'U', and ULB = 'U'. 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            This parameter is not referenced if ABSCHU = 'S', 
   *            ULA = 'U', and ULB = 'U'. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK = 0, if ABSCHU = 'S', ULA = 'U', and ULB = 'U'; 
   *            LDWORK = 2*MAX(M,N)*(4 + 2*MAX(M,N)), otherwise. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = 1:  if a (numerically) singular matrix T was encountered 
   *                  during the computation of the solution matrix X. 
   *                  That is, the estimated reciprocal condition number 
   *                  of T is less than or equal to TOL. 
   * 
   *    METHOD 
   * 
   *    Matrices A and B are assumed to be in (upper or lower) Hessenberg 
   *    or Schur form (with at least one of them in Schur form). The 
   *    solution matrix X is then computed by rows or columns via the back 
   *    substitution scheme proposed by Golub, Nash and Van Loan (see 
   *    [1]), which involves the solution of triangular systems of 
   *    equations that are constructed recursively and which may be nearly 
   *    singular if A and -B have close eigenvalues. If near singularity 
   *    is detected, then the routine returns with the Error Indicator 
   *    (INFO) set to 1. 
   * 
   *    REFERENCES 
   * 
   *    [1] Golub, G.H., Nash, S. and Van Loan, C.F. 
   *        A Hessenberg-Schur method for the problem AX + XB = C. 
   *        IEEE Trans. Auto. Contr., AC-24, pp. 909-913, 1979. 
   * 
   *    NUMERICAL ASPECTS 
   *                                           2         2 
   *    The algorithm requires approximately 5M N + 0.5MN  operations in 
   *                           2         2 
   *    the worst case and 2.5M N + 0.5MN  operations in the best case 
   *    (where M is the order of the matrix in Hessenberg form and N is 
   *    the order of the matrix in Schur form) and is mixed stable (see 
   *    [1]). 
   * 
   *    CONTRIBUTORS 
   * 
   *    Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Aug. 1997. 
   *    Supersedes Release 2.0 routine SB04BD by M. Vanbegin, and 
   *    P. Van Dooren, Philips Research Laboratory, Brussels, Belgium. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Apr. 2000. 
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
   *    .. External Functions .. 
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
  --iwork;
  --dwork;

  /* Function Body */
  *info = 0;
  maxmn = Max (*m, *n);
  labscb = C2F (lsame) (abschu, "B", 1L, 1L);
  labscs = C2F (lsame) (abschu, "S", 1L, 1L);
  lula = C2F (lsame) (ula, "U", 1L, 1L);
  lulb = C2F (lsame) (ulb, "U", 1L, 1L);
  /* 
   *    Test the input scalar arguments. 
   * 
   */
  if (!labscb && !labscs && !C2F (lsame) (abschu, "A", 1L, 1L))
    {
      *info = -1;
    }
  else if (!lula && !C2F (lsame) (ula, "L", 1L, 1L))
    {
      *info = -2;
    }
  else if (!lulb && !C2F (lsame) (ulb, "L", 1L, 1L))
    {
      *info = -3;
    }
  else if (*n < 0)
    {
      *info = -4;
    }
  else if (*m < 0)
    {
      *info = -5;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldb < Max (1, *m))
    {
      *info = -9;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -11;
    }
  else if (*ldwork < 0 || !(labscs && lula && lulb)
	   && *ldwork < (maxmn << 1) * ((maxmn << 1) + 4))
    {
      *info = -15;
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
      C2F (xerbla) ("SB04ND", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (maxmn == 0)
    {
      return 0;
    }
  /* 
   */
  if (labscs && lula && lulb)
    {
      /* 
       *       If both matrices are in a real Schur form, use DTRSYL. 
       * 
       */
      C2F (dtrsyl) ("NoTranspose", "NoTranspose", &c__1, n, m, &a[a_offset],
		    lda, &b[b_offset], ldb, &c__[c_offset], ldc, &scale, info,
		    11L, 11L);
      if (scale != 1.)
	{
	  *info = 1;
	}
      return 0;
    }
  /* 
   */
  ldw = maxmn << 1;
  jwork = ldw * ldw + ldw * 3 + 1;
  tol1 = *tol;
  if (tol1 <= 0.)
    {
      tol1 = C2F (dlamch) ("Epsilon", 7L);
    }
  /* 
   *    Choose the smallest of both matrices as the one in Hessenberg 
   *    form when possible. 
   * 
   */
  *(unsigned char *) abschr = *(unsigned char *) abschu;
  if (labscs)
    {
      if (*n > *m)
	{
	  *(unsigned char *) abschr = 'A';
	}
      else
	{
	  *(unsigned char *) abschr = 'B';
	}
    }
  if (C2F (lsame) (abschr, "B", 1L, 1L))
    {
      /* 
       *       B is in Schur form: recursion on the columns of B. 
       * 
       */
      if (lulb)
	{
	  /* 
	   *          B is upper: forward recursion. 
	   * 
	   */
	  ibeg = 1;
	  iend = *m;
	  fwd = 1;
	  incr = 0;
	}
      else
	{
	  /* 
	   *          B is lower: backward recursion. 
	   * 
	   */
	  ibeg = *m;
	  iend = 1;
	  fwd = -1;
	  incr = -1;
	}
      i__ = ibeg;
      /*       WHILE ( ( IEND - I ) * FWD .GE. 0 ) DO 
       */
    L20:
      if ((iend - i__) * fwd >= 0)
	{
	  /* 
	   *          Test for 1-by-1 or 2-by-2 diagonal block in the Schur 
	   *          form. 
	   * 
	   */
	  if (i__ == iend)
	    {
	      istep = 1;
	    }
	  else
	    {
	      if (b[i__ + fwd + i__ * b_dim1] == 0.)
		{
		  istep = 1;
		}
	      else
		{
		  istep = 2;
		}
	    }
	  /* 
	   */
	  if (istep == 1)
	    {
	      nsp_slicot_sb04nw (abschr, ulb, n, m, &c__[c_offset], ldc, &i__,
				 &b[b_offset], ldb, &dwork[jwork], 1L, 1L);
	      nsp_slicot_sb04ny ("R", ula, n, &a[a_offset], lda,
				 &b[i__ + i__ * b_dim1], &dwork[jwork], &tol1,
				 &iwork[1], &dwork[1], &ldw, info, 1L, 1L);
	      if (*info == 1)
		{
		  return 0;
		}
	      C2F (dcopy) (n, &dwork[jwork], &c__1, &c__[i__ * c_dim1 + 1],
			   &c__1);
	    }
	  else
	    {
	      ipincr = i__ + incr;
	      nsp_slicot_sb04nv (abschr, ulb, n, m, &c__[c_offset], ldc,
				 &ipincr, &b[b_offset], ldb, &dwork[jwork],
				 1L, 1L);
	      nsp_slicot_sb04nx ("R", ula, n, &a[a_offset], lda,
				 &b[ipincr + ipincr * b_dim1],
				 &b[ipincr + 1 + ipincr * b_dim1],
				 &b[ipincr + (ipincr + 1) * b_dim1],
				 &b[ipincr + 1 + (ipincr + 1) * b_dim1],
				 &dwork[jwork], &tol1, &iwork[1], &dwork[1],
				 &ldw, info, 1L, 1L);
	      if (*info == 1)
		{
		  return 0;
		}
	      C2F (dcopy) (n, &dwork[jwork], &c__2, &c__[ipincr * c_dim1 + 1],
			   &c__1);
	      C2F (dcopy) (n, &dwork[jwork + 1], &c__2,
			   &c__[(ipincr + 1) * c_dim1 + 1], &c__1);
	    }
	  i__ += fwd * istep;
	  goto L20;
	}
      /*       END WHILE 20 
       */
    }
  else
    {
      /* 
       *       A is in Schur form: recursion on the rows of A. 
       * 
       */
      if (lula)
	{
	  /* 
	   *          A is upper: backward recursion. 
	   * 
	   */
	  ibeg = *n;
	  iend = 1;
	  fwd = -1;
	  incr = -1;
	}
      else
	{
	  /* 
	   *          A is lower: forward recursion. 
	   * 
	   */
	  ibeg = 1;
	  iend = *n;
	  fwd = 1;
	  incr = 0;
	}
      i__ = ibeg;
      /*       WHILE ( ( IEND - I ) * FWD .GE. 0 ) DO 
       */
    L40:
      if ((iend - i__) * fwd >= 0)
	{
	  /* 
	   *          Test for 1-by-1 or 2-by-2 diagonal block in the Schur 
	   *          form. 
	   * 
	   */
	  if (i__ == iend)
	    {
	      istep = 1;
	    }
	  else
	    {
	      if (a[i__ + (i__ + fwd) * a_dim1] == 0.)
		{
		  istep = 1;
		}
	      else
		{
		  istep = 2;
		}
	    }
	  /* 
	   */
	  if (istep == 1)
	    {
	      nsp_slicot_sb04nw (abschr, ula, n, m, &c__[c_offset], ldc, &i__,
				 &a[a_offset], lda, &dwork[jwork], 1L, 1L);
	      nsp_slicot_sb04ny ("C", ulb, m, &b[b_offset], ldb,
				 &a[i__ + i__ * a_dim1], &dwork[jwork], &tol1,
				 &iwork[1], &dwork[1], &ldw, info, 1L, 1L);
	      if (*info == 1)
		{
		  return 0;
		}
	      C2F (dcopy) (m, &dwork[jwork], &c__1, &c__[i__ + c_dim1], ldc);
	    }
	  else
	    {
	      ipincr = i__ + incr;
	      nsp_slicot_sb04nv (abschr, ula, n, m, &c__[c_offset], ldc,
				 &ipincr, &a[a_offset], lda, &dwork[jwork],
				 1L, 1L);
	      nsp_slicot_sb04nx ("C", ulb, m, &b[b_offset], ldb,
				 &a[ipincr + ipincr * a_dim1],
				 &a[ipincr + 1 + ipincr * a_dim1],
				 &a[ipincr + (ipincr + 1) * a_dim1],
				 &a[ipincr + 1 + (ipincr + 1) * a_dim1],
				 &dwork[jwork], &tol1, &iwork[1], &dwork[1],
				 &ldw, info, 1L, 1L);
	      if (*info == 1)
		{
		  return 0;
		}
	      C2F (dcopy) (m, &dwork[jwork], &c__2, &c__[ipincr + c_dim1],
			   ldc);
	      C2F (dcopy) (m, &dwork[jwork + 1], &c__2,
			   &c__[ipincr + 1 + c_dim1], ldc);
	    }
	  i__ += fwd * istep;
	  goto L40;
	}
      /*       END WHILE 40 
       */
    }
  /* 
   */
  return 0;
  /**** Last line of SB04ND *** 
   */
}				/* nsp_slicot_sb04nd */
