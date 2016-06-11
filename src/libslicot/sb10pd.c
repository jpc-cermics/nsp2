/* SB10PD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static double c_b27 = 1.;
static double c_b28 = 0.;

/* Subroutine */ int
nsp_slicot_sb10pd (int *n, int *m, int *np, int *ncon,
		   int *nmeas, double *a, int *lda, double *b,
		   int *ldb, double *c__, int *ldc, double *d__,
		   int *ldd, double *tu, int *ldtu, double *ty,
		   int *ldty, double *rcond, double *tol,
		   double *dwork, int *ldwork, int *info)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1,
    d_offset, tu_dim1, tu_offset, ty_dim1, ty_offset, i__1, i__2, i__3, i__4,
    i__5, i__6, i__7, i__8, i__9, i__10;
  double d__1;

  /* Builtin functions */
  

  /* Local variables */
  int iext;
  double toll;
  int iwrk, info2, j;
  int m1, m2;
  int iq;
  int lwamax, nd1, nd2, minwrk, np1, np2;
  double eps;

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
   *    To reduce the matrices D12 and D21 of the linear time-invariant 
   *    system 
   * 
   *                  | A  | B1  B2  |   | A | B | 
   *              P = |----|---------| = |---|---| 
   *                  | C1 | D11 D12 |   | C | D | 
   *                  | C2 | D21 D22 | 
   * 
   *    to unit diagonal form, to transform the matrices B, C, and D11 to 
   *    satisfy the formulas in the computation of an H2 and H-infinity 
   *    (sub)optimal controllers and to check the rank conditions. 
   * 
   *    ARGUMENTS 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the system.  N >= 0. 
   * 
   *    M       (input) INT 
   *            The column size of the matrix B.  M >= 0. 
   * 
   *    NP      (input) INT 
   *            The row size of the matrix C.  NP >= 0. 
   * 
   *    NCON    (input) INT 
   *            The number of control inputs (M2).  M >= NCON >= 0, 
   *            NP-NMEAS >= NCON. 
   * 
   *    NMEAS   (input) INT 
   *            The number of measurements (NP2).  NP >= NMEAS >= 0, 
   *            M-NCON >= NMEAS. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
   *            The leading N-by-N part of this array must contain the 
   *            system state matrix A. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A.  LDA >= Max(1,N). 
   * 
   *    B       (input/output) DOUBLE PRECISION array, dimension (LDB,M) 
   *            On entry, the leading N-by-M part of this array must 
   *            contain the system input matrix B. 
   *            On exit, the leading N-by-M part of this array contains 
   *            the transformed system input matrix B. 
   * 
   *    LDB     INT 
   *            The leading dimension of the array B.  LDB >= Max(1,N). 
   * 
   *    C       (input/output) DOUBLE PRECISION array, dimension (LDC,N) 
   *            On entry, the leading NP-by-N part of this array must 
   *            contain the system output matrix C. 
   *            On exit, the leading NP-by-N part of this array contains 
   *            the transformed system output matrix C. 
   * 
   *    LDC     INT 
   *            The leading dimension of the array C.  LDC >= Max(1,NP). 
   * 
   *    D       (input/output) DOUBLE PRECISION array, dimension (LDD,M) 
   *            On entry, the leading NP-by-M part of this array must 
   *            contain the system input/output matrix D. The 
   *            NMEAS-by-NCON trailing submatrix D22 is not referenced. 
   *            On exit, the leading (NP-NMEAS)-by-(M-NCON) part of this 
   *            array contains the transformed submatrix D11. 
   *            The transformed submatrices D12 = [ 0  Im2 ]' and 
   *            D21 = [ 0  Inp2 ] are not stored. The corresponding part 
   *            of this array contains no useful information. 
   * 
   *    LDD     INT 
   *            The leading dimension of the array D.  LDD >= Max(1,NP). 
   * 
   *    TU      (output) DOUBLE PRECISION array, dimension (LDTU,M2) 
   *            The leading M2-by-M2 part of this array contains the 
   *            control transformation matrix TU. 
   * 
   *    LDTU    INT 
   *            The leading dimension of the array TU.  LDTU >= Max(1,M2). 
   * 
   *    TY      (output) DOUBLE PRECISION array, dimension (LDTY,NP2) 
   *            The leading NP2-by-NP2 part of this array contains the 
   *            measurement transformation matrix TY. 
   * 
   *    LDTY    INT 
   *            The leading dimension of the array TY. 
   *            LDTY >= Max(1,NP2). 
   * 
   *    RCOND   (output) DOUBLE PRECISION array, dimension (2) 
   *            RCOND(1) contains the reciprocal condition number of the 
   *                     control transformation matrix TU; 
   *            RCOND(2) contains the reciprocal condition number of the 
   *                     measurement transformation matrix TY. 
   *            RCOND is set even if INFO = 3 or INFO = 4; if INFO = 3, 
   *            then RCOND(2) was not computed, but it is set to 0. 
   * 
   *    Tolerances 
   * 
   *    TOL     DOUBLE PRECISION 
   *            Tolerance used for controlling the accuracy of the applied 
   *            transformations. Transformation matrices TU and TY whose 
   *            reciprocal condition numbers are less than TOL are not 
   *            allowed. If TOL <= 0, then a default value equal to 
   *            sqrt(EPS) is used, where EPS is the relative machine 
   *            precision. 
   * 
   *    Workspace 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0, DWORK(1) contains the optimal 
   *            LDWORK. 
   * 
   *    LDWORK  INT 
   *            The dimension of the array DWORK. 
   *            LDWORK >= MAX(1,LW1,LW2,LW3,LW4), where 
   *            LW1 = (N+NP1+1)*(N+M2) + MAX(3*(N+M2)+N+NP1,5*(N+M2)), 
   *            LW2 = (N+NP2)*(N+M1+1) + MAX(3*(N+NP2)+N+M1,5*(N+NP2)), 
   *            LW3 = M2 + NP1*NP1 + MAX(NP1*MAX(N,M1),3*M2+NP1,5*M2), 
   *            LW4 = NP2 + M1*M1 + MAX(MAX(N,NP1)*M1,3*NP2+M1,5*NP2), 
   *            with M1 = M - M2 and NP1 = NP - NP2. 
   *            For good performance, LDWORK must generally be larger. 
   *            Denoting Q = MAX(M1,M2,NP1,NP2), an upper bound is 
   *            MAX(1,(N+Q)*(N+Q+6),Q*(Q+MAX(N,Q,5)+1). 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = 1:  if the matrix | A   B2  | had not full column rank 
   *                                | C1  D12 | 
   *                  in respect to the tolerance EPS; 
   *            = 2:  if the matrix | A   B1  | had not full row rank in 
   *                                | C2  D21 | 
   *                  respect to the tolerance EPS; 
   *            = 3:  if the matrix D12 had not full column rank in 
   *                  respect to the tolerance TOL; 
   *            = 4:  if the matrix D21 had not full row rank in respect 
   *                  to the tolerance TOL; 
   *            = 5:  if the singular value decomposition (SVD) algorithm 
   *                  did not converge (when computing the SVD of one of 
   *                  the matrices |A   B2 |, |A   B1 |, D12 or D21). 
   *                               |C1  D12|  |C2  D21| 
   * 
   *    METHOD 
   * 
   *    The routine performs the transformations described in [2]. 
   * 
   *    REFERENCES 
   * 
   *    [1] Glover, K. and Doyle, J.C. 
   *        State-space formulae for all stabilizing controllers that 
   *        satisfy an Hinf norm bound and relations to risk sensitivity. 
   *        Systems and Control Letters, vol. 11, pp. 167-172, 1988. 
   * 
   *    [2] Balas, G.J., Doyle, J.C., Glover, K., Packard, A., and 
   *        Smith, R. 
   *        mu-Analysis and Synthesis Toolbox. 
   *        The MathWorks Inc., Natick, Mass., 1995. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The precision of the transformations can be controlled by the 
   *    condition numbers of the matrices TU and TY as given by the 
   *    values of RCOND(1) and RCOND(2), respectively. An error return 
   *    with INFO = 3 or INFO = 4 will be obtained if the condition 
   *    number of TU or TY, respectively, would exceed 1/TOL. 
   * 
   *    CONTRIBUTORS 
   * 
   *    P.Hr. Petkov, D.W. Gu and M.M. Konstantinov, October 1998. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, May 1999, 
   *    Feb. 2000. 
   * 
   *    KEYWORDS 
   * 
   *    H-infinity optimal control, robust control, singular value 
   *    decomposition. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. 
   *    .. Scalar Arguments .. 
   *    .. 
   *    .. Array Arguments .. 
   *    .. 
   *    .. Local Scalars .. 
   *    .. 
   *    .. External Functions .. 
   *    .. 
   *    .. External Subroutines .. 
   *    .. 
   *    .. Intrinsic Functions .. 
   *    .. 
   *    .. Executable Statements .. 
   * 
   *    Decode and Test input parameters. 
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
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  tu_dim1 = *ldtu;
  tu_offset = tu_dim1 + 1;
  tu -= tu_offset;
  ty_dim1 = *ldty;
  ty_offset = ty_dim1 + 1;
  ty -= ty_offset;
  --rcond;
  --dwork;

  /* Function Body */
  m1 = *m - *ncon;
  m2 = *ncon;
  np1 = *np - *nmeas;
  np2 = *nmeas;
  /* 
   */
  *info = 0;
  if (*n < 0)
    {
      *info = -1;
    }
  else if (*m < 0)
    {
      *info = -2;
    }
  else if (*np < 0)
    {
      *info = -3;
    }
  else if (*ncon < 0 || m1 < 0 || m2 > np1)
    {
      *info = -4;
    }
  else if (*nmeas < 0 || np1 < 0 || np2 > m1)
    {
      *info = -5;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldb < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldc < Max (1, *np))
    {
      *info = -11;
    }
  else if (*ldd < Max (1, *np))
    {
      *info = -13;
    }
  else if (*ldtu < Max (1, m2))
    {
      *info = -15;
    }
  else if (*ldty < Max (1, np2))
    {
      *info = -17;
    }
  else
    {
      /* 
       *       Compute workspace. 
       * 
       *Computing MAX 
       *Computing MAX 
       */
      i__3 = (*n + m2) * 3 + *n + np1, i__4 = (*n + m2) * 5;
      /*Computing MAX 
       */
      i__5 = (*n + np2) * 3 + *n + m1, i__6 = (*n + np2) * 5;
      /*Computing MAX 
       */
      i__7 = np1 * Max (*n, m1), i__8 = m2 * 3 + np1, i__7 =
	Max(i__7, i__8), i__8 = m2 * 5;
      /*Computing MAX 
       */
      i__9 = Max (*n, np1) * m1, i__10 = np2 * 3 + m1, i__9 =
	Max(i__9, i__10), i__10 = np2 * 5;
      i__1 = 1, i__2 = (*n + np1 + 1) * (*n + m2) + Max (i__3, i__4), i__1 =
	Max(i__1, i__2), i__2 =
	(*n + np2) * (*n + m1 + 1) + Max (i__5, i__6), i__1 =
	Max(i__1, i__2), i__2 = m2 + np1 * np1 + Max (i__7, i__8), i__1 =
	Max(i__1, i__2), i__2 = np2 + m1 * m1 + Max (i__9, i__10);
      minwrk = Max (i__1, i__2);
      if (*ldwork < minwrk)
	{
	  *info = -21;
	}
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("SB10PD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0 || *m == 0 || *np == 0 || m1 == 0 || m2 == 0 || np1 == 0
      || np2 == 0)
    {
      rcond[1] = 1.;
      rcond[2] = 1.;
      dwork[1] = 1.;
      return 0;
    }
  /* 
   */
  nd1 = np1 - m2;
  nd2 = m1 - np2;
  eps = C2F (dlamch) ("Epsilon", 7L);
  toll = *tol;
  if (toll <= 0.)
    {
      /* 
       *       Set the default value of the tolerance for condition tests. 
       * 
       */
      toll = sqrt (eps);
    }
  /* 
   *    Determine if |A-jwI  B2 | has full column rank at w = 0. 
   *                 |  C1   D12| 
   *    Workspace:  need   (N+NP1+1)*(N+M2) + 
   *                       Max(3*(N+M2)+N+NP1,5*(N+M2)); 
   *                prefer larger. 
   * 
   */
  iext = *n + m2 + 1;
  iwrk = iext + (*n + np1) * (*n + m2);
  i__1 = *n + np1;
  C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &dwork[iext], &i__1, 4L);
  i__1 = *n + np1;
  C2F (dlacpy) ("Full", &np1, n, &c__[c_offset], ldc, &dwork[iext + *n],
		&i__1, 4L);
  i__1 = *n + np1;
  C2F (dlacpy) ("Full", n, &m2, &b[(m1 + 1) * b_dim1 + 1], ldb,
		&dwork[iext + (*n + np1) * *n], &i__1, 4L);
  i__1 = *n + np1;
  C2F (dlacpy) ("Full", &np1, &m2, &d__[(m1 + 1) * d_dim1 + 1], ldd,
		&dwork[iext + (*n + np1) * *n + *n], &i__1, 4L);
  i__1 = *n + np1;
  i__2 = *n + m2;
  i__3 = *n + np1;
  i__4 = *ldwork - iwrk + 1;
  C2F (dgesvd) ("N", "N", &i__1, &i__2, &dwork[iext], &i__3, &dwork[1],
		&tu[tu_offset], ldtu, &ty[ty_offset], ldty, &dwork[iwrk],
		&i__4, &info2, 1L, 1L);
  if (info2 != 0)
    {
      *info = 5;
      return 0;
    }
  if (dwork[*n + m2] / dwork[1] <= eps)
    {
      *info = 1;
      return 0;
    }
  lwamax = (int) dwork[iwrk] + iwrk - 1;
  /* 
   *    Determine if |A-jwI  B1 | has full row rank at w = 0. 
   *                 |  C2   D21| 
   *    Workspace:  need   (N+NP2)*(N+M1+1) + 
   *                       Max(3*(N+NP2)+N+M1,5*(N+NP2)); 
   *                prefer larger. 
   * 
   */
  iext = *n + np2 + 1;
  iwrk = iext + (*n + np2) * (*n + m1);
  i__1 = *n + np2;
  C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &dwork[iext], &i__1, 4L);
  i__1 = *n + np2;
  C2F (dlacpy) ("Full", &np2, n, &c__[np1 + 1 + c_dim1], ldc,
		&dwork[iext + *n], &i__1, 4L);
  i__1 = *n + np2;
  C2F (dlacpy) ("Full", n, &m1, &b[b_offset], ldb,
		&dwork[iext + (*n + np2) * *n], &i__1, 4L);
  i__1 = *n + np2;
  C2F (dlacpy) ("Full", &np2, &m1, &d__[np1 + 1 + d_dim1], ldd,
		&dwork[iext + (*n + np2) * *n + *n], &i__1, 4L);
  i__1 = *n + np2;
  i__2 = *n + m1;
  i__3 = *n + np2;
  i__4 = *ldwork - iwrk + 1;
  C2F (dgesvd) ("N", "N", &i__1, &i__2, &dwork[iext], &i__3, &dwork[1],
		&tu[tu_offset], ldtu, &ty[ty_offset], ldty, &dwork[iwrk],
		&i__4, &info2, 1L, 1L);
  if (info2 != 0)
    {
      *info = 5;
      return 0;
    }
  if (dwork[*n + np2] / dwork[1] <= eps)
    {
      *info = 2;
      return 0;
    }
  /*Computing MAX 
   */
  i__1 = (int) dwork[iwrk] + iwrk - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Determine SVD of D12, D12 = U12 S12 V12', and check if D12 has 
   *    full column rank. V12' is stored in TU. 
   *    Workspace:  need   M2 + NP1*NP1 + Max(3*M2+NP1,5*M2); 
   *                prefer larger. 
   * 
   */
  iq = m2 + 1;
  iwrk = iq + np1 * np1;
  /* 
   */
  i__1 = *ldwork - iwrk + 1;
  C2F (dgesvd) ("A", "A", &np1, &m2, &d__[(m1 + 1) * d_dim1 + 1], ldd,
		&dwork[1], &dwork[iq], &np1, &tu[tu_offset], ldtu,
		&dwork[iwrk], &i__1, &info2, 1L, 1L);
  if (info2 != 0)
    {
      *info = 5;
      return 0;
    }
  /* 
   */
  rcond[1] = dwork[m2] / dwork[1];
  if (rcond[1] <= toll)
    {
      rcond[2] = 0.;
      *info = 3;
      return 0;
    }
  /*Computing MAX 
   */
  i__1 = (int) dwork[iwrk] + iwrk - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Determine Q12. 
   * 
   */
  if (nd1 > 0)
    {
      C2F (dlacpy) ("Full", &np1, &m2, &dwork[iq], &np1,
		    &d__[(m1 + 1) * d_dim1 + 1], ldd, 4L);
      C2F (dlacpy) ("Full", &np1, &nd1, &dwork[iq + np1 * m2], &np1,
		    &dwork[iq], &np1, 4L);
      C2F (dlacpy) ("Full", &np1, &m2, &d__[(m1 + 1) * d_dim1 + 1], ldd,
		    &dwork[iq + np1 * nd1], &np1, 4L);
    }
  /* 
   *    Determine Tu by transposing in-situ and scaling. 
   * 
   */
  i__1 = m2 - 1;
  for (j = 1; j <= i__1; ++j)
    {
      C2F (dswap) (&j, &tu[j + 1 + tu_dim1], ldtu, &tu[(j + 1) * tu_dim1 + 1],
		   &c__1);
      /* L10: */
    }
  /* 
   */
  i__1 = m2;
  for (j = 1; j <= i__1; ++j)
    {
      d__1 = 1. / dwork[j];
      C2F (dscal) (&m2, &d__1, &tu[j * tu_dim1 + 1], &c__1);
      /* L20: */
    }
  /* 
   *    Determine C1 =: Q12'*C1. 
   *    Workspace:  M2 + NP1*NP1 + NP1*N. 
   * 
   */
  C2F (dgemm) ("T", "N", &np1, n, &np1, &c_b27, &dwork[iq], &np1,
	       &c__[c_offset], ldc, &c_b28, &dwork[iwrk], &np1, 1L, 1L);
  C2F (dlacpy) ("Full", &np1, n, &dwork[iwrk], &np1, &c__[c_offset], ldc, 4L);
  /*Computing MAX 
   */
  i__1 = iwrk + np1 * *n - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Determine D11 =: Q12'*D11. 
   *    Workspace:  M2 + NP1*NP1 + NP1*M1. 
   * 
   */
  C2F (dgemm) ("T", "N", &np1, &m1, &np1, &c_b27, &dwork[iq], &np1,
	       &d__[d_offset], ldd, &c_b28, &dwork[iwrk], &np1, 1L, 1L);
  C2F (dlacpy) ("Full", &np1, &m1, &dwork[iwrk], &np1, &d__[d_offset], ldd,
		4L);
  /*Computing MAX 
   */
  i__1 = iwrk + np1 * m1 - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Determine SVD of D21, D21 = U21 S21 V21', and check if D21 has 
   *    full row rank. U21 is stored in TY. 
   *    Workspace:  need   NP2 + M1*M1 + Max(3*NP2+M1,5*NP2); 
   *                prefer larger. 
   * 
   */
  iq = np2 + 1;
  iwrk = iq + m1 * m1;
  /* 
   */
  i__1 = *ldwork - iwrk + 1;
  C2F (dgesvd) ("A", "A", &np2, &m1, &d__[np1 + 1 + d_dim1], ldd, &dwork[1],
		&ty[ty_offset], ldty, &dwork[iq], &m1, &dwork[iwrk], &i__1,
		&info2, 1L, 1L);
  if (info2 != 0)
    {
      *info = 5;
      return 0;
    }
  /* 
   */
  rcond[2] = dwork[np2] / dwork[1];
  if (rcond[2] <= toll)
    {
      *info = 4;
      return 0;
    }
  /*Computing MAX 
   */
  i__1 = (int) dwork[iwrk] + iwrk - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Determine Q21. 
   * 
   */
  if (nd2 > 0)
    {
      C2F (dlacpy) ("Full", &np2, &m1, &dwork[iq], &m1,
		    &d__[np1 + 1 + d_dim1], ldd, 4L);
      C2F (dlacpy) ("Full", &nd2, &m1, &dwork[iq + np2], &m1, &dwork[iq], &m1,
		    4L);
      C2F (dlacpy) ("Full", &np2, &m1, &d__[np1 + 1 + d_dim1], ldd,
		    &dwork[iq + nd2], &m1, 4L);
    }
  /* 
   *    Determine Ty by scaling and transposing in-situ. 
   * 
   */
  i__1 = np2;
  for (j = 1; j <= i__1; ++j)
    {
      d__1 = 1. / dwork[j];
      C2F (dscal) (&np2, &d__1, &ty[j * ty_dim1 + 1], &c__1);
      /* L30: */
    }
  /* 
   */
  i__1 = np2 - 1;
  for (j = 1; j <= i__1; ++j)
    {
      C2F (dswap) (&j, &ty[j + 1 + ty_dim1], ldty, &ty[(j + 1) * ty_dim1 + 1],
		   &c__1);
      /* L40: */
    }
  /* 
   *    Determine B1 =: B1*Q21'. 
   *    Workspace:  NP2 + M1*M1 + N*M1. 
   * 
   */
  C2F (dgemm) ("N", "T", n, &m1, &m1, &c_b27, &b[b_offset], ldb, &dwork[iq],
	       &m1, &c_b28, &dwork[iwrk], n, 1L, 1L);
  C2F (dlacpy) ("Full", n, &m1, &dwork[iwrk], n, &b[b_offset], ldb, 4L);
  /*Computing MAX 
   */
  i__1 = iwrk + *n * m1 - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Determine D11 =: D11*Q21'. 
   *    Workspace:  NP2 + M1*M1 + NP1*M1. 
   * 
   */
  C2F (dgemm) ("N", "T", &np1, &m1, &m1, &c_b27, &d__[d_offset], ldd,
	       &dwork[iq], &m1, &c_b28, &dwork[iwrk], &np1, 1L, 1L);
  C2F (dlacpy) ("Full", &np1, &m1, &dwork[iwrk], &np1, &d__[d_offset], ldd,
		4L);
  /*Computing MAX 
   */
  i__1 = iwrk + np1 * m1 - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Determine B2 =: B2*Tu. 
   *    Workspace:  N*M2. 
   * 
   */
  C2F (dgemm) ("N", "N", n, &m2, &m2, &c_b27, &b[(m1 + 1) * b_dim1 + 1], ldb,
	       &tu[tu_offset], ldtu, &c_b28, &dwork[1], n, 1L, 1L);
  C2F (dlacpy) ("Full", n, &m2, &dwork[1], n, &b[(m1 + 1) * b_dim1 + 1], ldb,
		4L);
  /* 
   *    Determine C2 =: Ty*C2. 
   *    Workspace:  NP2*N. 
   * 
   */
  C2F (dgemm) ("N", "N", &np2, n, &np2, &c_b27, &ty[ty_offset], ldty,
	       &c__[np1 + 1 + c_dim1], ldc, &c_b28, &dwork[1], &np2, 1L, 1L);
  C2F (dlacpy) ("Full", &np2, n, &dwork[1], &np2, &c__[np1 + 1 + c_dim1], ldc,
		4L);
  /* 
   *Computing MAX 
   */
  i__1 = *n * Max (m2, np2);
  lwamax = Max (i__1, lwamax);
  dwork[1] = (double) lwamax;
  return 0;
  /**** Last line of SB10PD *** 
   */
}				/* nsp_slicot_sb10pd */
