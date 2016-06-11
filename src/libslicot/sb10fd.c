/* SB10FD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Subroutine */ int
nsp_slicot_sb10fd (int *n, int *m, int *np, int *ncon,
		   int *nmeas, double *gamma, double *a, int *lda,
		   double *b, int *ldb, double *c__, int *ldc,
		   double *d__, int *ldd, double *ak, int *ldak,
		   double *bk, int *ldbk, double *ck, int *ldck,
		   double *dk, int *lddk, double *rcond,
		   double *tol, int *iwork, double *dwork,
		   int *ldwork, int *bwork, int *info)
{
  /* System generated locals */
  int a_dim1, a_offset, ak_dim1, ak_offset, b_dim1, b_offset, bk_dim1,
    bk_offset, c_dim1, c_offset, ck_dim1, ck_offset, d_dim1, d_offset,
    dk_dim1, dk_offset, i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9,
    i__10, i__11, i__12;

  /* Builtin functions */
  

  /* Local variables */
  double toll;
  int iwrk, iwtu, iwty, info2;
  int m1, m2;
  int lwamax, nd1, nd2, minwrk, np1, np2, lw1, lw2, lw3, lw4, lw5, lw6,
    iwc, iwd, iwf, iwh, iwx, iwy;

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
   *    To compute the matrices of an H-infinity (sub)optimal n-state 
   *    controller 
   * 
   *             | AK | BK | 
   *         K = |----|----|, 
   *             | CK | DK | 
   * 
   *    using modified Glover's and Doyle's 1988 formulas, for the system 
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
   *    (A2) D12 is full column rank and D21 is full row rank, 
   * 
   *    (A3) | A-j*omega*I  B2  | has full column rank for all omega, 
   *         |    C1        D12 | 
   * 
   *    (A4) | A-j*omega*I  B1  |  has full row rank for all omega. 
   *         |    C2        D21 | 
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
   *    AK      (output) DOUBLE PRECISION array, dimension (LDAK,N) 
   *            The leading N-by-N part of this array contains the 
   *            controller state matrix AK. 
   * 
   *    LDAK    INT 
   *            The leading dimension of the array AK.  LDAK >= Max(1,N). 
   * 
   *    BK      (output) DOUBLE PRECISION array, dimension (LDBK,NMEAS) 
   *            The leading N-by-NMEAS part of this array contains the 
   *            controller input matrix BK. 
   * 
   *    LDBK    INT 
   *            The leading dimension of the array BK.  LDBK >= Max(1,N). 
   * 
   *    CK      (output) DOUBLE PRECISION array, dimension (LDCK,N) 
   *            The leading NCON-by-N part of this array contains the 
   *            controller output matrix CK. 
   * 
   *    LDCK    INT 
   *            The leading dimension of the array CK. 
   *            LDCK >= Max(1,NCON). 
   * 
   *    DK      (output) DOUBLE PRECISION array, dimension (LDDK,NMEAS) 
   *            The leading NCON-by-NMEAS part of this array contains the 
   *            controller input/output matrix DK. 
   * 
   *    LDDK    INT 
   *            The leading dimension of the array DK. 
   *            LDDK >= Max(1,NCON). 
   * 
   *    RCOND   (output) DOUBLE PRECISION array, dimension (4) 
   *            RCOND(1) contains the reciprocal condition number of the 
   *                     control transformation matrix; 
   *            RCOND(2) contains the reciprocal condition number of the 
   *                     measurement transformation matrix; 
   *            RCOND(3) contains an estimate of the reciprocal condition 
   *                     number of the X-Riccati equation; 
   *            RCOND(4) contains an estimate of the reciprocal condition 
   *                     number of the Y-Riccati equation. 
   * 
   *    Tolerances 
   * 
   *    TOL     DOUBLE PRECISION 
   *            Tolerance used for controlling the accuracy of the applied 
   *            transformations for computing the normalized form in 
   *            SLICOT Library routine SB10PD. Transformation matrices 
   *            whose reciprocal condition numbers are less than TOL are 
   *            not allowed. If TOL <= 0, then a default value equal to 
   *            sqrt(EPS) is used, where EPS is the relative machine 
   *            precision. 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (LIWORK), where 
   *            LIWORK = Max(2*max(N,M-NCON,NP-NMEAS,NCON),N*N) 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0, DWORK(1) contains the optimal 
   *            LDWORK. 
   * 
   *    LDWORK  INT 
   *            The dimension of the array DWORK. 
   *            LDWORK >= N*M + NP*(N+M) + M2*M2 + NP2*NP2 + 
   *                      Max(1,LW1,LW2,LW3,LW4,LW5,LW6), where 
   *            LW1 = (N+NP1+1)*(N+M2) + Max(3*(N+M2)+N+NP1,5*(N+M2)), 
   *            LW2 = (N+NP2)*(N+M1+1) + Max(3*(N+NP2)+N+M1,5*(N+NP2)), 
   *            LW3 = M2 + NP1*NP1 + Max(NP1*max(N,M1),3*M2+NP1,5*M2), 
   *            LW4 = NP2 + M1*M1 + Max(max(N,NP1)*M1,3*NP2+M1,5*NP2), 
   *            LW5 = 2*N*N + N*(M+NP) + 
   *                  Max(1,M*M + Max(2*M1,3*N*N+max(N*M,10*N*N+12*N+5)), 
   *                      NP*NP + Max(2*NP1,3*N*N + 
   *                                  Max(N*NP,10*N*N+12*N+5))), 
   *            LW6 = 2*N*N + N*(M+NP) + 
   *                  Max(1, M2*NP2 + NP2*NP2 + M2*M2 + 
   *                      Max(D1*D1 + Max(2*D1, (D1+D2)*NP2), 
   *                          D2*D2 + Max(2*D2, D2*M2), 3*N, 
   *                          N*(2*NP2 + M2) + 
   *                          Max(2*N*M2, M2*NP2 + 
   *                                      Max(M2*M2+3*M2, NP2*(2*NP2+ 
   *                                             M2+max(NP2,N)))))), 
   *            with D1 = NP1 - M2, D2 = M1 - NP2, 
   *                NP1 = NP - NP2, M1 = M - M2. 
   *            For good performance, LDWORK must generally be larger. 
   *            Denoting Q = Max(M1,M2,NP1,NP2), an upper bound is 
   *            2*Q*(3*Q+2*N)+max(1,(N+Q)*(N+Q+6),Q*(Q+max(N,Q,5)+1), 
   *              2*N*(N+2*Q)+max(1,4*Q*Q+ 
   *                              Max(2*Q,3*N*N+max(2*N*Q,10*N*N+12*N+5)), 
   *                                Q*(3*N+3*Q+max(2*N,4*Q+max(N,Q))))). 
   * 
   *    BWORK   INT array, dimension (2*N) 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = 1:  if the matrix | A-j*omega*I  B2  | had not full 
   *                                |    C1        D12 | 
   *                  column rank in respect to the tolerance EPS; 
   *            = 2:  if the matrix | A-j*omega*I  B1  |  had not full row 
   *                                |    C2        D21 | 
   *                  rank in respect to the tolerance EPS; 
   *            = 3:  if the matrix D12 had not full column rank in 
   *                  respect to the tolerance TOL; 
   *            = 4:  if the matrix D21 had not full row rank in respect 
   *                  to the tolerance TOL; 
   *            = 5:  if the singular value decomposition (SVD) algorithm 
   *                  did not converge (when computing the SVD of one of 
   *                  the matrices |A   B2 |, |A   B1 |, D12 or D21). 
   *                               |C1  D12|  |C2  D21| 
   *            = 6:  if the controller is not admissible (too small value 
   *                  of gamma); 
   *            = 7:  if the X-Riccati equation was not solved 
   *                  successfully (the controller is not admissible or 
   *                  there are numerical difficulties); 
   *            = 8:  if the Y-Riccati equation was not solved 
   *                  successfully (the controller is not admissible or 
   *                  there are numerical difficulties); 
   *            = 9:  if the determinant of Im2 + Tu*D11HAT*Ty*D22 is 
   *                  zero [3]. 
   * 
   *    METHOD 
   * 
   *    The routine implements the Glover's and Doyle's 1988 formulas [1], 
   *    [2] modified to improve the efficiency as described in [3]. 
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
   *    NUMERICAL ASPECTS 
   * 
   *    The accuracy of the result depends on the condition numbers of the 
   *    input and output transformations and on the condition numbers of 
   *    the two Riccati equations, as given by the values of RCOND(1), 
   *    RCOND(2), RCOND(3) and RCOND(4), respectively. 
   * 
   *    CONTRIBUTORS 
   * 
   *    P.Hr. Petkov, D.W. Gu and M.M. Konstantinov, October 1998. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, May 1999, 
   *    Sept. 1999, Feb. 2000. 
   * 
   *    KEYWORDS 
   * 
   *    Algebraic Riccati equation, H-infinity optimal control, robust 
   *    control. 
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
  ak_dim1 = *ldak;
  ak_offset = ak_dim1 + 1;
  ak -= ak_offset;
  bk_dim1 = *ldbk;
  bk_offset = bk_dim1 + 1;
  bk -= bk_offset;
  ck_dim1 = *ldck;
  ck_offset = ck_dim1 + 1;
  ck -= ck_offset;
  dk_dim1 = *lddk;
  dk_offset = dk_dim1 + 1;
  dk -= dk_offset;
  --rcond;
  --iwork;
  --dwork;
  --bwork;

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
  else if (*ldak < Max (1, *n))
    {
      *info = -16;
    }
  else if (*ldbk < Max (1, *n))
    {
      *info = -18;
    }
  else if (*ldck < Max (1, m2))
    {
      *info = -20;
    }
  else if (*lddk < Max (1, m2))
    {
      *info = -22;
    }
  else
    {
      /* 
       *       Compute workspace. 
       * 
       */
      nd1 = np1 - m2;
      nd2 = m1 - np2;
      /*Computing MAX 
       */
      i__1 = (*n + m2) * 3 + *n + np1, i__2 = (*n + m2) * 5;
      lw1 = (*n + np1 + 1) * (*n + m2) + Max (i__1, i__2);
      /*Computing MAX 
       */
      i__1 = (*n + np2) * 3 + *n + m1, i__2 = (*n + np2) * 5;
      lw2 = (*n + np2) * (*n + m1 + 1) + Max (i__1, i__2);
      /*Computing MAX 
       */
      i__1 = np1 * Max (*n, m1), i__2 = m2 * 3 + np1, i__1 =
	Max(i__1, i__2), i__2 = m2 * 5;
      lw3 = m2 + np1 * np1 + Max (i__1, i__2);
      /*Computing MAX 
       */
      i__1 = Max (*n, np1) * m1, i__2 = np2 * 3 + m1, i__1 =
	Max(i__1, i__2), i__2 = np2 * 5;
      lw4 = np2 + m1 * m1 + Max (i__1, i__2);
      /*Computing MAX 
       *Computing MAX 
       *Computing MAX 
       */
      i__5 = *n * *m, i__6 = *n * 10 * *n + *n * 12 + 5;
      i__3 = m1 << 1, i__4 = *n * 3 * *n + Max (i__5, i__6);
      /*Computing MAX 
       *Computing MAX 
       */
      i__9 = *n * *np, i__10 = *n * 10 * *n + *n * 12 + 5;
      i__7 = np1 << 1, i__8 = *n * 3 * *n + Max (i__9, i__10);
      i__1 = 1, i__2 = *m * *m + Max (i__3, i__4), i__1 =
	Max(i__1, i__2), i__2 = *np * *np + Max (i__7, i__8);
      lw5 = (*n << 1) * *n + *n * (*m + *np) + Max (i__1, i__2);
      /*Computing MAX 
       *Computing MAX 
       *Computing MAX 
       */
      i__5 = nd1 << 1, i__6 = (nd1 + nd2) * np2;
      /*Computing MAX 
       */
      i__7 = nd2 << 1, i__8 = nd2 * m2;
      /*Computing MAX 
       *Computing MAX 
       */
      i__11 = m2 * m2 + m2 * 3, i__12 =
	np2 * ((np2 << 1) + m2 + Max (np2, *n));
      i__9 = (*n << 1) * m2, i__10 = m2 * np2 + Max (i__11, i__12);
      i__3 = nd1 * nd1 + Max (i__5, i__6), i__4 =
	nd2 * nd2 + Max (i__7, i__8), i__3 = Max (i__3, i__4), i__4 =
	*n * 3, i__3 = Max (i__3, i__4), i__4 =
	*n * ((np2 << 1) + m2) + Max (i__9, i__10);
      i__1 = 1, i__2 = m2 * np2 + np2 * np2 + m2 * m2 + Max (i__3, i__4);
      lw6 = (*n << 1) * *n + *n * (*m + *np) + Max (i__1, i__2);
      /*Computing MAX 
       */
      i__1 = Max (1, lw1), i__1 = Max (i__1, lw2), i__1 =
	Max(i__1, lw3), i__1 = Max (i__1, lw4), i__1 = Max (i__1, lw5);
      minwrk =
	*n * *m + *np * (*n + *m) + m2 * m2 + np2 * np2 + Max (i__1, lw6);
      if (*ldwork < minwrk)
	{
	  *info = -27;
	}
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("SB10FD", &i__1, 6L);
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
      rcond[3] = 1.;
      rcond[4] = 1.;
      dwork[1] = 1.;
      return 0;
    }
  /* 
   */
  toll = *tol;
  if (toll <= 0.)
    {
      /* 
       *       Set the default value of the tolerance. 
       * 
       */
      toll = sqrt (C2F (dlamch) ("Epsilon", 7L));
    }
  /* 
   *    Workspace usage. 
   * 
   */
  iwc = *n * *m + 1;
  iwd = iwc + *np * *n;
  iwtu = iwd + *np * *m;
  iwty = iwtu + m2 * m2;
  iwrk = iwty + np2 * np2;
  /* 
   */
  C2F (dlacpy) ("Full", n, m, &b[b_offset], ldb, &dwork[1], n, 4L);
  C2F (dlacpy) ("Full", np, n, &c__[c_offset], ldc, &dwork[iwc], np, 4L);
  C2F (dlacpy) ("Full", np, m, &d__[d_offset], ldd, &dwork[iwd], np, 4L);
  /* 
   *    Transform the system so that D12 and D21 satisfy the formulas 
   *    in the computation of the Hinf (sub)optimal controller. 
   * 
   */
  i__1 = *ldwork - iwrk + 1;
  nsp_slicot_sb10pd (n, m, np, ncon, nmeas, &a[a_offset], lda, &dwork[1], n,
		     &dwork[iwc], np, &dwork[iwd], np, &dwork[iwtu], &m2,
		     &dwork[iwty], &np2, &rcond[1], &toll, &dwork[iwrk],
		     &i__1, &info2);
  if (info2 > 0)
    {
      *info = info2;
      return 0;
    }
  lwamax = (int) dwork[iwrk] + iwrk - 1;
  /* 
   */
  iwx = iwrk;
  iwy = iwx + *n * *n;
  iwf = iwy + *n * *n;
  iwh = iwf + *m * *n;
  iwrk = iwh + *n * *np;
  /* 
   *    Compute the (sub)optimal state feedback and output injection 
   *    matrices. 
   * 
   */
  i__1 = *ldwork - iwrk + 1;
  nsp_slicot_sb10qd (n, m, np, ncon, nmeas, gamma, &a[a_offset], lda,
		     &dwork[1], n, &dwork[iwc], np, &dwork[iwd], np,
		     &dwork[iwf], m, &dwork[iwh], n, &dwork[iwx], n,
		     &dwork[iwy], n, &rcond[3], &iwork[1], &dwork[iwrk],
		     &i__1, &bwork[1], &info2);
  if (info2 > 0)
    {
      *info = info2 + 5;
      return 0;
    }
  /*Computing MAX 
   */
  i__1 = (int) dwork[iwrk] + iwrk - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Compute the Hinf (sub)optimal controller. 
   * 
   */
  i__1 = *ldwork - iwrk + 1;
  nsp_slicot_sb10rd (n, m, np, ncon, nmeas, gamma, &a[a_offset], lda,
		     &dwork[1], n, &dwork[iwc], np, &dwork[iwd], np,
		     &dwork[iwf], m, &dwork[iwh], n, &dwork[iwtu], &m2,
		     &dwork[iwty], &np2, &dwork[iwx], n, &dwork[iwy], n,
		     &ak[ak_offset], ldak, &bk[bk_offset], ldbk,
		     &ck[ck_offset], ldck, &dk[dk_offset], lddk, &iwork[1],
		     &dwork[iwrk], &i__1, &info2);
  if (info2 == 1)
    {
      *info = 6;
      return 0;
    }
  else if (info2 == 2)
    {
      *info = 9;
      return 0;
    }
  /*Computing MAX 
   */
  i__1 = (int) dwork[iwrk] + iwrk - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   */
  dwork[1] = (double) lwamax;
  return 0;
  /**** Last line of SB10FD *** 
   */
}				/* nsp_slicot_sb10fd */
