/* SB10QD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b5 = 0.;
static double c_b8 = 1.;
static double c_b17 = -1.;

/* Subroutine */ int
nsp_slicot_sb10qd (int *n, int *m, int *np, int *ncon,
		   int *nmeas, double *gamma, double *a, int *lda,
		   double *b, int *ldb, double *c__, int *ldc,
		   double *d__, int *ldd, double *f, int *ldf,
		   double *h__, int *ldh, double *x, int *ldx,
		   double *y, int *ldy, double *xycond, int *iwork,
		   double *dwork, int *ldwork, int *bwork, int *info)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1,
    d_offset, f_dim1, f_offset, h_dim1, h_offset, x_dim1, x_offset, y_dim1,
    y_offset, i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9, i__10;
  double d__1;

  /* Local variables */
  double ferr;
  int iwrk, info2;
  double rcond;
  double anorm;
  int m1, m2, n2;
  int nn;
  int lwamax;
  int nd1, nd2, minwrk;
  int np1, np2, iw2, iwa, iwg, iwi;
  double eps, sep;
  int iwq, iwr, iws, iwt, iwv;

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
   *    To compute the state feedback and the output injection 
   *    matrices for an H-infinity (sub)optimal n-state controller, 
   *    using Glover's and Doyle's 1988 formulas, for the system 
   * 
   *                  | A  | B1  B2  |   | A | B | 
   *              P = |----|---------| = |---|---| 
   *                  | C1 | D11 D12 |   | C | D | 
   *                  | C2 | D21 D22 | 
   * 
   *    and for a given value of gamma, where B2 has as column size the 
   *    number of control inputs (NCON) and C2 has as row size the number 
   *    of measurements (NMEAS) being provided to the controller. 
   * 
   *    It is assumed that 
   * 
   *    (A1) (A,B2) is stabilizable and (C2,A) is detectable, 
   * 
   *    (A2) D12 is full column rank with D12 = | 0 | and D21 is 
   *                                            | I | 
   *         full row rank with D21 = | 0 I | as obtained by the 
   *         subroutine SB10PD, 
   * 
   *    (A3) | A-j*omega*I  B2  | has full column rank for all omega, 
   *         |    C1        D12 | 
   * 
   * 
   *    (A4) | A-j*omega*I  B1  |  has full row rank for all omega. 
   *         |    C2        D21 | 
   * 
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
   *    GAMMA   (input) DOUBLE PRECISION 
   *            The value of gamma. It is assumed that gamma is 
   *            sufficiently large so that the controller is admissible. 
   *            GAMMA >= 0. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
   *            The leading N-by-N part of this array must contain the 
   *            system state matrix A. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A.  LDA >= Max(1,N). 
   * 
   *    B       (input) DOUBLE PRECISION array, dimension (LDB,M) 
   *            The leading N-by-M part of this array must contain the 
   *            system input matrix B. 
   * 
   *    LDB     INT 
   *            The leading dimension of the array B.  LDB >= Max(1,N). 
   * 
   *    C       (input) DOUBLE PRECISION array, dimension (LDC,N) 
   *            The leading NP-by-N part of this array must contain the 
   *            system output matrix C. 
   * 
   *    LDC     INT 
   *            The leading dimension of the array C.  LDC >= Max(1,NP). 
   * 
   *    D       (input) DOUBLE PRECISION array, dimension (LDD,M) 
   *            The leading NP-by-M part of this array must contain the 
   *            system input/output matrix D. 
   * 
   *    LDD     INT 
   *            The leading dimension of the array D.  LDD >= Max(1,NP). 
   * 
   *    F       (output) DOUBLE PRECISION array, dimension (LDF,N) 
   *            The leading M-by-N part of this array contains the state 
   *            feedback matrix F. 
   * 
   *    LDF     INT 
   *            The leading dimension of the array F.  LDF >= Max(1,M). 
   * 
   *    H       (output) DOUBLE PRECISION array, dimension (LDH,NP) 
   *            The leading N-by-NP part of this array contains the output 
   *            injection matrix H. 
   * 
   *    LDH     INT 
   *            The leading dimension of the array H.  LDH >= Max(1,N). 
   * 
   *    X       (output) DOUBLE PRECISION array, dimension (LDX,N) 
   *            The leading N-by-N part of this array contains the matrix 
   *            X, solution of the X-Riccati equation. 
   * 
   *    LDX     INT 
   *            The leading dimension of the array X.  LDX >= Max(1,N). 
   * 
   *    Y       (output) DOUBLE PRECISION array, dimension (LDY,N) 
   *            The leading N-by-N part of this array contains the matrix 
   *            Y, solution of the Y-Riccati equation. 
   * 
   *    LDY     INT 
   *            The leading dimension of the array Y.  LDY >= Max(1,N). 
   * 
   *    XYCOND  (output) DOUBLE PRECISION array, dimension (2) 
   *            XYCOND(1) contains an estimate of the reciprocal condition 
   *                      number of the X-Riccati equation; 
   *            XYCOND(2) contains an estimate of the reciprocal condition 
   *                      number of the Y-Riccati equation. 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension Max(2*max(N,M-NCON,NP-NMEAS),N*N) 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0, DWORK(1) contains the optimal 
   *            LDWORK. 
   * 
   *    LDWORK  INT 
   *            The dimension of the array DWORK. 
   *            LDWORK >= Max(1,M*M + Max(2*M1,3*N*N + 
   *                                      Max(N*M,10*N*N+12*N+5)), 
   *                          NP*NP + Max(2*NP1,3*N*N + 
   *                                      Max(N*NP,10*N*N+12*N+5))), 
   *            where M1 = M - M2 and NP1 = NP - NP2. 
   *            For good performance, LDWORK must generally be larger. 
   *            Denoting Q = MAX(M1,M2,NP1,NP2), an upper bound is 
   *            Max(1,4*Q*Q+max(2*Q,3*N*N + Max(2*N*Q,10*N*N+12*N+5))). 
   * 
   *    BWORK   INT array, dimension (2*N) 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = 1:  if the controller is not admissible (too small value 
   *                  of gamma); 
   *            = 2:  if the X-Riccati equation was not solved 
   *                  successfully (the controller is not admissible or 
   *                  there are numerical difficulties); 
   *            = 3:  if the Y-Riccati equation was not solved 
   *                  successfully (the controller is not admissible or 
   *                  there are numerical difficulties). 
   * 
   *    METHOD 
   * 
   *    The routine implements the Glover's and Doyle's formulas [1],[2] 
   *    modified as described in [3]. The X- and Y-Riccati equations 
   *    are solved with condition and accuracy estimates [4]. 
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
   *    [3] Petkov, P.Hr., Gu, D.W., and Konstantinov, M.M. 
   *        Fortran 77 routines for Hinf and H2 design of continuous-time 
   *        linear control systems. 
   *        Rep. 98-14, Department of Engineering, Leicester University, 
   *        Leicester, U.K., 1998. 
   * 
   *    [4] Petkov, P.Hr., Konstantinov, M.M., and Mehrmann, V. 
   *        DGRSVX and DMSRIC: Fortan 77 subroutines for solving 
   *        continuous-time matrix algebraic Riccati equations with 
   *        condition and accuracy estimates. 
   *        Preprint SFB393/98-16, Fak. f. Mathematik, Tech. Univ. 
   *        Chemnitz, May 1998. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The precision of the solution of the matrix Riccati equations 
   *    can be controlled by the values of the condition numbers 
   *    XYCOND(1) and XYCOND(2) of these equations. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    The Riccati equations are solved by the Schur approach 
   *    implementing condition and accuracy estimates. 
   * 
   *    CONTRIBUTORS 
   * 
   *    P.Hr. Petkov, D.W. Gu and M.M. Konstantinov, October 1998. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, May 1999, 
   *    Sept. 1999. 
   * 
   *    KEYWORDS 
   * 
   *    Algebraic Riccati equation, H-infinity optimal control, robust 
   *    control. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Parameters .. 
   * 
   *    .. Scalar Arguments .. 
   *    .. 
   *    .. Array Arguments .. 
   * 
   *    .. 
   *    .. Local Scalars .. 
   *    .. 
   *    .. External Functions .. 
   * 
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
  f_dim1 = *ldf;
  f_offset = f_dim1 + 1;
  f -= f_offset;
  h_dim1 = *ldh;
  h_offset = h_dim1 + 1;
  h__ -= h_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  y_dim1 = *ldy;
  y_offset = y_dim1 + 1;
  y -= y_offset;
  --xycond;
  --iwork;
  --dwork;
  --bwork;

  /* Function Body */
  m1 = *m - *ncon;
  m2 = *ncon;
  np1 = *np - *nmeas;
  np2 = *nmeas;
  nn = *n * *n;
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
  else if (*gamma < 0.)
    {
      *info = -6;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -8;
    }
  else if (*ldb < Max (1, *n))
    {
      *info = -10;
    }
  else if (*ldc < Max (1, *np))
    {
      *info = -12;
    }
  else if (*ldd < Max (1, *np))
    {
      *info = -14;
    }
  else if (*ldf < Max (1, *m))
    {
      *info = -16;
    }
  else if (*ldh < Max (1, *n))
    {
      *info = -18;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -20;
    }
  else if (*ldy < Max (1, *n))
    {
      *info = -22;
    }
  else
    {
      /* 
       *       Compute workspace. 
       * 
       *Computing MAX 
       *Computing MAX 
       *Computing MAX 
       */
      i__5 = *n * *m, i__6 = nn * 10 + *n * 12 + 5;
      i__3 = m1 << 1, i__4 = nn * 3 + Max (i__5, i__6);
      /*Computing MAX 
       *Computing MAX 
       */
      i__9 = *n * *np, i__10 = nn * 10 + *n * 12 + 5;
      i__7 = np1 << 1, i__8 = nn * 3 + Max (i__9, i__10);
      i__1 = 1, i__2 = *m * *m + Max (i__3, i__4), i__1 =
	Max(i__1, i__2), i__2 = *np * *np + Max (i__7, i__8);
      minwrk = Max (i__1, i__2);
      if (*ldwork < minwrk)
	{
	  *info = -26;
	}
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("SB10QD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0 || *m == 0 || *np == 0 || m1 == 0 || m2 == 0 || np1 == 0
      || np2 == 0)
    {
      xycond[1] = 1.;
      xycond[2] = 1.;
      dwork[1] = 1.;
      return 0;
    }
  nd1 = np1 - m2;
  nd2 = m1 - np2;
  n2 = *n << 1;
  /* 
   *    Get the machine precision. 
   * 
   */
  eps = C2F (dlamch) ("Epsilon", 7L);
  /* 
   *    Workspace usage. 
   * 
   */
  iwa = *m * *m + 1;
  iwq = iwa + nn;
  iwg = iwq + nn;
  iw2 = iwg + nn;
  /* 
   *    Compute |D1111'||D1111 D1112| - gamma^2*Im1 . 
   *            |D1112'| 
   * 
   */
  d__1 = -(*gamma) * *gamma;
  C2F (dlaset) ("L", &m1, &m1, &c_b5, &d__1, &dwork[1], m, 1L);
  if (nd1 > 0)
    {
      C2F (dsyrk) ("L", "T", &m1, &nd1, &c_b8, &d__[d_offset], ldd, &c_b8,
		   &dwork[1], m, 1L, 1L);
    }
  /* 
   *    Compute inv(|D1111'|*|D1111 D1112| - gamma^2*Im1) . 
   *                |D1112'| 
   * 
   */
  iwrk = iwa;
  anorm = C2F (dlansy) ("I", "L", &m1, &dwork[1], m, &dwork[iwrk], 1L, 1L);
  i__1 = *ldwork - iwrk + 1;
  C2F (dsytrf) ("L", &m1, &dwork[1], m, &iwork[1], &dwork[iwrk], &i__1,
		&info2, 1L);
  if (info2 > 0)
    {
      *info = 1;
      return 0;
    }
  /* 
   */
  lwamax = (int) dwork[iwrk] + iwrk - 1;
  C2F (dsycon) ("L", &m1, &dwork[1], m, &iwork[1], &anorm, &rcond,
		&dwork[iwrk], &iwork[m1 + 1], &info2, 1L);
  if (rcond < eps)
    {
      *info = 1;
      return 0;
    }
  /* 
   *    Compute inv(R) block by block. 
   * 
   */
  C2F (dsytri) ("L", &m1, &dwork[1], m, &iwork[1], &dwork[iwrk], &info2, 1L);
  /* 
   *    Compute -|D1121 D1122|*inv(|D1111'|*|D1111 D1112| - gamma^2*Im1) . 
   *                               |D1112'| 
   * 
   */
  C2F (dsymm) ("R", "L", &m2, &m1, &c_b17, &dwork[1], m,
	       &d__[nd1 + 1 + d_dim1], ldd, &c_b5, &dwork[m1 + 1], m, 1L, 1L);
  /* 
   *    Compute |D1121 D1122|*inv(|D1111'|*|D1111 D1112| - 
   *                              |D1112'| 
   * 
   *                 gamma^2*Im1)*|D1121'| + Im2 . 
   *                              |D1122'| 
   * 
   */
  C2F (dlaset) ("Lower", &m2, &m2, &c_b5, &c_b8, &dwork[m1 * (*m + 1) + 1], m,
		5L);
  nsp_slicot_mb01rx ("Right", "Lower", "Transpose", &m2, &m1, &c_b8, &c_b17,
		     &dwork[m1 * (*m + 1) + 1], m, &d__[nd1 + 1 + d_dim1],
		     ldd, &dwork[m1 + 1], m, &info2, 5L, 5L, 9L);
  /* 
   *    Compute D11'*C1 . 
   * 
   */
  C2F (dgemm) ("T", "N", &m1, n, &np1, &c_b8, &d__[d_offset], ldd,
	       &c__[c_offset], ldc, &c_b5, &dwork[iw2], m, 1L, 1L);
  /* 
   *    Compute D1D'*C1 . 
   * 
   */
  C2F (dlacpy) ("Full", &m2, n, &c__[nd1 + 1 + c_dim1], ldc, &dwork[iw2 + m1],
		m, 4L);
  /* 
   *    Compute inv(R)*D1D'*C1 in F . 
   * 
   */
  C2F (dsymm) ("L", "L", m, n, &c_b8, &dwork[1], m, &dwork[iw2], m, &c_b5,
	       &f[f_offset], ldf, 1L, 1L);
  /* 
   *    Compute Ax = A - B*inv(R)*D1D'*C1 . 
   * 
   */
  C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &dwork[iwa], n, 4L);
  C2F (dgemm) ("N", "N", n, n, m, &c_b17, &b[b_offset], ldb, &f[f_offset],
	       ldf, &c_b8, &dwork[iwa], n, 1L, 1L);
  /* 
   *    Compute Cx = C1'*C1 - C1'*D1D*inv(R)*D1D'*C1 . 
   * 
   */
  if (nd1 == 0)
    {
      C2F (dlaset) ("L", n, n, &c_b5, &c_b5, &dwork[iwq], n, 1L);
    }
  else
    {
      C2F (dsyrk) ("L", "T", n, &np1, &c_b8, &c__[c_offset], ldc, &c_b5,
		   &dwork[iwq], n, 1L, 1L);
      nsp_slicot_mb01rx ("Left", "Lower", "Transpose", n, m, &c_b8, &c_b17,
			 &dwork[iwq], n, &dwork[iw2], m, &f[f_offset], ldf,
			 &info2, 4L, 5L, 9L);
    }
  /* 
   *    Compute Dx = B*inv(R)*B' . 
   * 
   */
  iwrk = iw2;
  i__1 = *m * *n;
  nsp_slicot_mb01ru ("Lower", "NoTranspose", n, m, &c_b5, &c_b8, &dwork[iwg],
		     n, &b[b_offset], ldb, &dwork[1], m, &dwork[iwrk], &i__1,
		     &info2, 5L, 11L);
  /* 
   *    Solution of the Riccati equation Ax'*X + X*Ax + Cx - X*Dx*X = 0 . 
   *    Workspace:  need   M*M + 13*N*N + 12*N + 5; 
   *                prefer larger. 
   * 
   */
  iwt = iw2;
  iwv = iwt + nn;
  iwr = iwv + nn;
  iwi = iwr + n2;
  iws = iwi + n2;
  iwrk = iws + (nn << 2);
  /* 
   */
  i__1 = *ldwork - iwrk + 1;
  nsp_slicot_sb02rd ("All", "Continuous", "NotUsed", "NoTranspose", "Lower",
		     "GeneralScaling", "Stable", "NotFactored", "Original", n,
		     &dwork[iwa], n, &dwork[iwt], n, &dwork[iwv], n,
		     &dwork[iwg], n, &dwork[iwq], n, &x[x_offset], ldx, &sep,
		     &xycond[1], &ferr, &dwork[iwr], &dwork[iwi], &dwork[iws],
		     &n2, &iwork[1], &dwork[iwrk], &i__1, &bwork[1], &info2,
		     3L, 10L, 7L, 11L, 5L, 14L, 6L, 11L, 8L);
  if (info2 > 0)
    {
      *info = 2;
      return 0;
    }
  /* 
   *Computing MAX 
   */
  i__1 = (int) dwork[iwrk] + iwrk - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Compute F = -inv(R)*|D1D'*C1 + B'*X| . 
   * 
   */
  iwrk = iw2;
  C2F (dgemm) ("T", "N", m, n, n, &c_b8, &b[b_offset], ldb, &x[x_offset], ldx,
	       &c_b5, &dwork[iwrk], m, 1L, 1L);
  C2F (dsymm) ("L", "L", m, n, &c_b17, &dwork[1], m, &dwork[iwrk], m, &c_b17,
	       &f[f_offset], ldf, 1L, 1L);
  /* 
   *    Workspace usage. 
   * 
   */
  iwa = *np * *np + 1;
  iwq = iwa + nn;
  iwg = iwq + nn;
  iw2 = iwg + nn;
  /* 
   *    Compute |D1111|*|D1111' D1121'| - gamma^2*Inp1 . 
   *            |D1121| 
   * 
   */
  d__1 = -(*gamma) * *gamma;
  C2F (dlaset) ("U", &np1, &np1, &c_b5, &d__1, &dwork[1], np, 1L);
  if (nd2 > 0)
    {
      C2F (dsyrk) ("U", "N", &np1, &nd2, &c_b8, &d__[d_offset], ldd, &c_b8,
		   &dwork[1], np, 1L, 1L);
    }
  /* 
   *    Compute inv(|D1111|*|D1111' D1121'| - gamma^2*Inp1) . 
   *                |D1121| 
   * 
   */
  iwrk = iwa;
  anorm = C2F (dlansy) ("I", "U", &np1, &dwork[1], np, &dwork[iwrk], 1L, 1L);
  i__1 = *ldwork - iwrk + 1;
  C2F (dsytrf) ("U", &np1, &dwork[1], np, &iwork[1], &dwork[iwrk], &i__1,
		&info2, 1L);
  if (info2 > 0)
    {
      *info = 1;
      return 0;
    }
  /* 
   *Computing MAX 
   */
  i__1 = (int) dwork[iwrk] + iwrk - 1;
  lwamax = Max (i__1, lwamax);
  C2F (dsycon) ("U", &np1, &dwork[1], np, &iwork[1], &anorm, &rcond,
		&dwork[iwrk], &iwork[np1 + 1], &info2, 1L);
  if (rcond < eps)
    {
      *info = 1;
      return 0;
    }
  /* 
   *    Compute inv(RT) . 
   * 
   */
  C2F (dsytri) ("U", &np1, &dwork[1], np, &iwork[1], &dwork[iwrk], &info2,
		1L);
  /* 
   *    Compute -inv(|D1111||D1111' D1121'| - gamma^2*Inp1)*|D1112| . 
   *                 |D1121|                                |D1122| 
   * 
   */
  C2F (dsymm) ("L", "U", &np1, &np2, &c_b17, &dwork[1], np,
	       &d__[(nd2 + 1) * d_dim1 + 1], ldd, &c_b5,
	       &dwork[np1 * *np + 1], np, 1L, 1L);
  /* 
   *    Compute [D1112' D1122']*inv(|D1111||D1111' D1121'| - 
   *                                |D1121| 
   * 
   *               gamma^2*Inp1)*|D1112| + Inp2 . 
   *                             |D1122| 
   * 
   */
  C2F (dlaset) ("Full", &np2, &np2, &c_b5, &c_b8, &dwork[np1 * (*np + 1) + 1],
		np, 4L);
  nsp_slicot_mb01rx ("Left", "Upper", "Transpose", &np2, &np1, &c_b8, &c_b17,
		     &dwork[np1 * (*np + 1) + 1], np,
		     &d__[(nd2 + 1) * d_dim1 + 1], ldd, &dwork[np1 * *np + 1],
		     np, &info2, 4L, 5L, 9L);
  /* 
   *    Compute B1*D11' . 
   * 
   */
  C2F (dgemm) ("N", "T", n, &np1, &m1, &c_b8, &b[b_offset], ldb,
	       &d__[d_offset], ldd, &c_b5, &dwork[iw2], n, 1L, 1L);
  /* 
   *    Compute B1*DD1' . 
   * 
   */
  C2F (dlacpy) ("Full", n, &np2, &b[(nd2 + 1) * b_dim1 + 1], ldb,
		&dwork[iw2 + np1 * *n], n, 4L);
  /* 
   *    Compute B1*DD1'*inv(RT) in H . 
   * 
   */
  C2F (dsymm) ("R", "U", n, np, &c_b8, &dwork[1], np, &dwork[iw2], n, &c_b5,
	       &h__[h_offset], ldh, 1L, 1L);
  /* 
   *    Compute Ay = A - B1*DD1'*inv(RT)*C . 
   * 
   */
  C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &dwork[iwa], n, 4L);
  C2F (dgemm) ("N", "N", n, n, np, &c_b17, &h__[h_offset], ldh,
	       &c__[c_offset], ldc, &c_b8, &dwork[iwa], n, 1L, 1L);
  /* 
   *    Compute Cy = B1*B1' - B1*DD1'*inv(RT)*DD1*B1' . 
   * 
   */
  if (nd2 == 0)
    {
      C2F (dlaset) ("U", n, n, &c_b5, &c_b5, &dwork[iwq], n, 1L);
    }
  else
    {
      C2F (dsyrk) ("U", "N", n, &m1, &c_b8, &b[b_offset], ldb, &c_b5,
		   &dwork[iwq], n, 1L, 1L);
      nsp_slicot_mb01rx ("Right", "Upper", "Transpose", n, np, &c_b8, &c_b17,
			 &dwork[iwq], n, &h__[h_offset], ldh, &dwork[iw2], n,
			 &info2, 5L, 5L, 9L);
    }
  /* 
   *    Compute Dy = C'*inv(RT)*C . 
   * 
   */
  iwrk = iw2;
  i__1 = *n * *np;
  nsp_slicot_mb01ru ("Upper", "Transpose", n, np, &c_b5, &c_b8, &dwork[iwg],
		     n, &c__[c_offset], ldc, &dwork[1], np, &dwork[iwrk],
		     &i__1, &info2, 5L, 9L);
  /* 
   *    Solution of the Riccati equation Ay*Y + Y*Ay' + Cy - Y*Dy*Y = 0 . 
   *    Workspace:  need   NP*NP + 13*N*N + 12*N + 5; 
   *                prefer larger. 
   * 
   */
  iwt = iw2;
  iwv = iwt + nn;
  iwr = iwv + nn;
  iwi = iwr + n2;
  iws = iwi + n2;
  iwrk = iws + (nn << 2);
  /* 
   */
  i__1 = *ldwork - iwrk + 1;
  nsp_slicot_sb02rd ("All", "Continuous", "NotUsed", "Transpose", "Upper",
		     "GeneralScaling", "Stable", "NotFactored", "Original", n,
		     &dwork[iwa], n, &dwork[iwt], n, &dwork[iwv], n,
		     &dwork[iwg], n, &dwork[iwq], n, &y[y_offset], ldy, &sep,
		     &xycond[2], &ferr, &dwork[iwr], &dwork[iwi], &dwork[iws],
		     &n2, &iwork[1], &dwork[iwrk], &i__1, &bwork[1], &info2,
		     3L, 10L, 7L, 9L, 5L, 14L, 6L, 11L, 8L);
  if (info2 > 0)
    {
      *info = 3;
      return 0;
    }
  /* 
   *Computing MAX 
   */
  i__1 = (int) dwork[iwrk] + iwrk - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Compute H = -|B1*DD1' + Y*C'|*inv(RT) . 
   * 
   */
  iwrk = iw2;
  C2F (dgemm) ("N", "T", n, np, n, &c_b8, &y[y_offset], ldy, &c__[c_offset],
	       ldc, &c_b5, &dwork[iwrk], n, 1L, 1L);
  C2F (dsymm) ("R", "U", n, np, &c_b17, &dwork[1], np, &dwork[iwrk], n,
	       &c_b17, &h__[h_offset], ldh, 1L, 1L);
  /* 
   */
  dwork[1] = (double) lwamax;
  return 0;
  /**** Last line of SB10QD *** 
   */
}				/* nsp_slicot_sb10qd */
