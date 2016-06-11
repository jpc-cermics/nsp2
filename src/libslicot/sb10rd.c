/* SB10RD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b7 = 0.;
static double c_b8 = 1.;
static double c_b19 = -1.;

/* Subroutine */ int
nsp_slicot_sb10rd (int *n, int *m, int *np, int *ncon,
		   int *nmeas, double *gamma, double *a, int *lda,
		   double *b, int *ldb, double *c__, int *ldc,
		   double *d__, int *ldd, double *f, int *ldf,
		   double *h__, int *ldh, double *tu, int *ldtu,
		   double *ty, int *ldty, double *x, int *ldx,
		   double *y, int *ldy, double *ak, int *ldak,
		   double *bk, int *ldbk, double *ck, int *ldck,
		   double *dk, int *lddk, int *iwork, double *dwork,
		   int *ldwork, int *info)
{
  /* System generated locals */
  int a_dim1, a_offset, ak_dim1, ak_offset, b_dim1, b_offset, bk_dim1,
    bk_offset, c_dim1, c_offset, ck_dim1, ck_offset, d_dim1, d_offset,
    dk_dim1, dk_offset, f_dim1, f_offset, h_dim1, h_offset, tu_dim1,
    tu_offset, ty_dim1, ty_offset, x_dim1, x_offset, y_dim1, y_offset, i__1,
    i__2, i__3, i__4, i__5, i__6, i__7, i__8, i__9, i__10, i__11, i__12;
  double d__1, d__2;

  /* Local variables */
  int iwrk, info2;
  int i__, j;
  double rcond;
  double anorm;
  int m1, m2, ij;
  int lwamax;
  int nd1, nd2, minwrk;
  int np1, np2, iw1, iw2, iw3, iw4;
  int id11, id12, id21, iwb, iwc;
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
   *    To compute the matrices of an H-infinity (sub)optimal controller 
   * 
   *             | AK | BK | 
   *         K = |----|----|, 
   *             | CK | DK | 
   * 
   *    from the state feedback matrix F and output injection matrix H as 
   *    determined by the SLICOT Library routine SB10QD. 
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
   *            The number of control inputs (M2).  M >= NCON >= 0. 
   *            NP-NMEAS >= NCON. 
   * 
   *    NMEAS   (input) INT 
   *            The number of measurements (NP2).  NP >= NMEAS >= 0. 
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
   *    F       (input) DOUBLE PRECISION array, dimension (LDF,N) 
   *            The leading M-by-N part of this array must contain the 
   *            state feedback matrix F. 
   * 
   *    LDF     INT 
   *            The leading dimension of the array F.  LDF >= Max(1,M). 
   * 
   *    H       (input) DOUBLE PRECISION array, dimension (LDH,NP) 
   *            The leading N-by-NP part of this array must contain the 
   *            output injection matrix H. 
   * 
   *    LDH     INT 
   *            The leading dimension of the array H.  LDH >= Max(1,N). 
   * 
   *    TU      (input) DOUBLE PRECISION array, dimension (LDTU,M2) 
   *            The leading M2-by-M2 part of this array must contain the 
   *            control transformation matrix TU, as obtained by the 
   *            SLICOT Library routine SB10PD. 
   * 
   *    LDTU    INT 
   *            The leading dimension of the array TU.  LDTU >= Max(1,M2). 
   * 
   *    TY      (input) DOUBLE PRECISION array, dimension (LDTY,NP2) 
   *            The leading NP2-by-NP2 part of this array must contain the 
   *            measurement transformation matrix TY, as obtained by the 
   *            SLICOT Library routine SB10PD. 
   * 
   *    LDTY    INT 
   *            The leading dimension of the array TY. 
   *            LDTY >= Max(1,NP2). 
   * 
   *    X       (input) DOUBLE PRECISION array, dimension (LDX,N) 
   *            The leading N-by-N part of this array must contain the 
   *            matrix X, solution of the X-Riccati equation, as obtained 
   *            by the SLICOT Library routine SB10QD. 
   * 
   *    LDX     INT 
   *            The leading dimension of the array X.  LDX >= Max(1,N). 
   * 
   *    Y       (input) DOUBLE PRECISION array, dimension (LDY,N) 
   *            The leading N-by-N part of this array must contain the 
   *            matrix Y, solution of the Y-Riccati equation, as obtained 
   *            by the SLICOT Library routine SB10QD. 
   * 
   *    LDY     INT 
   *            The leading dimension of the array Y.  LDY >= Max(1,N). 
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
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (LIWORK), where 
   *            LIWORK = Max(2*(Max(NP,M)-M2-NP2,M2,N),NP2) 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0, DWORK(1) contains the optimal 
   *            LDWORK. 
   * 
   *    LDWORK  INT 
   *            The dimension of the array DWORK. 
   *            LDWORK >= Max(1, M2*NP2 + NP2*NP2 + M2*M2 + 
   *                          Max(D1*D1 + Max(2*D1, (D1+D2)*NP2), 
   *                              D2*D2 + Max(2*D2, D2*M2), 3*N, 
   *                              N*(2*NP2 + M2) + 
   *                              Max(2*N*M2, M2*NP2 + 
   *                                          Max(M2*M2+3*M2, NP2*(2*NP2+ 
   *                                                 M2+max(NP2,N)))))) 
   *            where D1 = NP1 - M2, D2 = M1 - NP2, 
   *                 NP1 = NP - NP2, M1 = M - M2. 
   *            For good performance, LDWORK must generally be larger. 
   *            Denoting Q = Max(M1,M2,NP1,NP2), an upper bound is 
   *            Max( 1, Q*(3*Q + 3*N + Max(2*N, 4*Q + Max(Q, N)))). 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = 1:  if the controller is not admissible (too small value 
   *                  of gamma); 
   *            = 2:  if the determinant of Im2 + Tu*D11HAT*Ty*D22 is zero. 
   * 
   *    METHOD 
   * 
   *    The routine implements the Glover's and Doyle's formulas [1],[2]. 
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
   *    The accuracy of the result depends on the condition numbers of the 
   *    input and output transformations. 
   * 
   *    CONTRIBUTORS 
   * 
   *    P.Hr. Petkov, D.W. Gu and M.M. Konstantinov, October 1998. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, May 1999, 
   *    Sept. 1999, Oct. 2001. 
   * 
   *    KEYWORDS 
   * 
   *    Algebraic Riccati equation, H-infinity optimal control, robust 
   *    control. 
   * 
   * ********************************************************************* 
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
  f_dim1 = *ldf;
  f_offset = f_dim1 + 1;
  f -= f_offset;
  h_dim1 = *ldh;
  h_offset = h_dim1 + 1;
  h__ -= h_offset;
  tu_dim1 = *ldtu;
  tu_offset = tu_dim1 + 1;
  tu -= tu_offset;
  ty_dim1 = *ldty;
  ty_offset = ty_dim1 + 1;
  ty -= ty_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  y_dim1 = *ldy;
  y_offset = y_dim1 + 1;
  y -= y_offset;
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
  --iwork;
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
  else if (*ldtu < Max (1, m2))
    {
      *info = -20;
    }
  else if (*ldty < Max (1, np2))
    {
      *info = -22;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -24;
    }
  else if (*ldy < Max (1, *n))
    {
      *info = -26;
    }
  else if (*ldak < Max (1, *n))
    {
      *info = -28;
    }
  else if (*ldbk < Max (1, *n))
    {
      *info = -30;
    }
  else if (*ldck < Max (1, m2))
    {
      *info = -32;
    }
  else if (*lddk < Max (1, m2))
    {
      *info = -34;
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
      minwrk = Max (i__1, i__2);
      if (*ldwork < minwrk)
	{
	  *info = -37;
	}
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("SB10RD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0 || *m == 0 || *np == 0 || m1 == 0 || m2 == 0 || np1 == 0
      || np2 == 0)
    {
      dwork[1] = 1.;
      return 0;
    }
  /* 
   *    Get the machine precision. 
   * 
   */
  eps = C2F (dlamch) ("Epsilon", 7L);
  /* 
   *    Workspace usage. 
   * 
   */
  id11 = 1;
  id21 = id11 + m2 * np2;
  id12 = id21 + np2 * np2;
  iw1 = id12 + m2 * m2;
  iw2 = iw1 + nd1 * nd1;
  iw3 = iw2 + nd1 * np2;
  iwrk = iw2;
  /* 
   *    Set D11HAT := -D1122 . 
   * 
   */
  ij = id11;
  i__1 = np2;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = m2;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  dwork[ij] = -d__[nd1 + i__ + (nd2 + j) * d_dim1];
	  ++ij;
	  /* L10: */
	}
      /* L20: */
    }
  /* 
   *    Set D21HAT := Inp2 . 
   * 
   */
  C2F (dlaset) ("Upper", &np2, &np2, &c_b7, &c_b8, &dwork[id21], &np2, 5L);
  /* 
   *    Set D12HAT := Im2 . 
   * 
   */
  C2F (dlaset) ("Lower", &m2, &m2, &c_b7, &c_b8, &dwork[id12], &m2, 5L);
  /* 
   *    Compute D11HAT, D21HAT, D12HAT . 
   * 
   */
  lwamax = 0;
  if (nd1 > 0)
    {
      if (nd2 == 0)
	{
	  /* 
	   *          Compute D21HAT'*D21HAT = Inp2 - D1112'*D1112/gamma^2 . 
	   * 
	   *Computing 2nd power 
	   */
	  d__2 = *gamma;
	  d__1 = -1. / (d__2 * d__2);
	  C2F (dsyrk) ("U", "T", &np2, &nd1, &d__1, &d__[d_offset], ldd,
		       &c_b8, &dwork[id21], &np2, 1L, 1L);
	}
      else
	{
	  /* 
	   *          Compute gdum = gamma^2*Ind1 - D1111*D1111' . 
	   * 
	   *Computing 2nd power 
	   */
	  d__2 = *gamma;
	  d__1 = d__2 * d__2;
	  C2F (dlaset) ("U", &nd1, &nd1, &c_b7, &d__1, &dwork[iw1], &nd1, 1L);
	  C2F (dsyrk) ("U", "N", &nd1, &nd2, &c_b19, &d__[d_offset], ldd,
		       &c_b8, &dwork[iw1], &nd1, 1L, 1L);
	  anorm =
	    C2F (dlansy) ("I", "U", &nd1, &dwork[iw1], &nd1, &dwork[iwrk], 1L,
			  1L);
	  i__1 = *ldwork - iwrk + 1;
	  C2F (dsytrf) ("U", &nd1, &dwork[iw1], &nd1, &iwork[1], &dwork[iwrk],
			&i__1, &info2, 1L);
	  if (info2 > 0)
	    {
	      *info = 1;
	      return 0;
	    }
	  lwamax = (int) dwork[iwrk] + iwrk - 1;
	  C2F (dsycon) ("U", &nd1, &dwork[iw1], &nd1, &iwork[1], &anorm,
			&rcond, &dwork[iwrk], &iwork[nd1 + 1], &info2, 1L);
	  /* 
	   *          Return if the matrix is singular to working precision. 
	   * 
	   */
	  if (rcond < eps)
	    {
	      *info = 1;
	      return 0;
	    }
	  /* 
	   *          Compute inv(gdum)*D1112 . 
	   * 
	   */
	  C2F (dlacpy) ("Full", &nd1, &np2, &d__[(nd2 + 1) * d_dim1 + 1], ldd,
			&dwork[iw2], &nd1, 4L);
	  C2F (dsytrs) ("U", &nd1, &np2, &dwork[iw1], &nd1, &iwork[1],
			&dwork[iw2], &nd1, &info2, 1L);
	  /* 
	   *          Compute D11HAT = -D1121*D1111'*inv(gdum)*D1112 - D1122 . 
	   * 
	   */
	  C2F (dgemm) ("T", "N", &nd2, &np2, &nd1, &c_b8, &d__[d_offset], ldd,
		       &dwork[iw2], &nd1, &c_b7, &dwork[iw3], &nd2, 1L, 1L);
	  C2F (dgemm) ("N", "N", &m2, &np2, &nd2, &c_b19,
		       &d__[nd1 + 1 + d_dim1], ldd, &dwork[iw3], &nd2, &c_b8,
		       &dwork[id11], &m2, 1L, 1L);
	  /* 
	   *          Compute D21HAT'*D21HAT = Inp2 - D1112'*inv(gdum)*D1112 . 
	   * 
	   */
	  nsp_slicot_mb01rx ("Left", "Upper", "Transpose", &np2, &nd1, &c_b8,
			     &c_b19, &dwork[id21], &np2,
			     &d__[(nd2 + 1) * d_dim1 + 1], ldd, &dwork[iw2],
			     &nd1, &info2, 4L, 5L, 9L);
	  /* 
	   */
	  iw2 = iw1 + nd2 * nd2;
	  iwrk = iw2;
	  /* 
	   *          Compute gdum = gamma^2*Ind2 - D1111'*D1111 . 
	   * 
	   *Computing 2nd power 
	   */
	  d__2 = *gamma;
	  d__1 = d__2 * d__2;
	  C2F (dlaset) ("L", &nd2, &nd2, &c_b7, &d__1, &dwork[iw1], &nd2, 1L);
	  C2F (dsyrk) ("L", "T", &nd2, &nd1, &c_b19, &d__[d_offset], ldd,
		       &c_b8, &dwork[iw1], &nd2, 1L, 1L);
	  anorm =
	    C2F (dlansy) ("I", "L", &nd2, &dwork[iw1], &nd2, &dwork[iwrk], 1L,
			  1L);
	  i__1 = *ldwork - iwrk + 1;
	  C2F (dsytrf) ("L", &nd2, &dwork[iw1], &nd2, &iwork[1], &dwork[iwrk],
			&i__1, &info2, 1L);
	  if (info2 > 0)
	    {
	      *info = 1;
	      return 0;
	    }
	  /*Computing MAX 
	   */
	  i__1 = (int) dwork[iwrk] + iwrk - 1;
	  lwamax = Max (i__1, lwamax);
	  C2F (dsycon) ("L", &nd2, &dwork[iw1], &nd2, &iwork[1], &anorm,
			&rcond, &dwork[iwrk], &iwork[nd2 + 1], &info2, 1L);
	  /* 
	   *          Return if the matrix is singular to working precision. 
	   * 
	   */
	  if (rcond < eps)
	    {
	      *info = 1;
	      return 0;
	    }
	  /* 
	   *          Compute inv(gdum)*D1121' . 
	   * 
	   */
	  nsp_slicot_ma02ad ("Full", &m2, &nd2, &d__[nd1 + 1 + d_dim1], ldd,
			     &dwork[iw2], &nd2, 4L);
	  C2F (dsytrs) ("L", &nd2, &m2, &dwork[iw1], &nd2, &iwork[1],
			&dwork[iw2], &nd2, &info2, 1L);
	  /* 
	   *          Compute D12HAT*D12HAT' = Im2 - D1121*inv(gdum)*D1121' . 
	   * 
	   */
	  nsp_slicot_mb01rx ("Left", "Lower", "NoTranspose", &m2, &nd2, &c_b8,
			     &c_b19, &dwork[id12], &m2,
			     &d__[nd1 + 1 + d_dim1], ldd, &dwork[iw2], &nd2,
			     &info2, 4L, 5L, 11L);
	}
    }
  else
    {
      if (nd2 > 0)
	{
	  /* 
	   *          Compute D12HAT*D12HAT' = Im2 - D1121*D1121'/gamma^2 . 
	   * 
	   *Computing 2nd power 
	   */
	  d__2 = *gamma;
	  d__1 = -1. / (d__2 * d__2);
	  C2F (dsyrk) ("L", "N", &m2, &nd2, &d__1, &d__[d_offset], ldd, &c_b8,
		       &dwork[id12], &m2, 1L, 1L);
	}
    }
  /* 
   *    Compute D21HAT using Cholesky decomposition. 
   * 
   */
  C2F (dpotrf) ("U", &np2, &dwork[id21], &np2, &info2, 1L);
  if (info2 > 0)
    {
      *info = 1;
      return 0;
    }
  /* 
   *    Compute D12HAT using Cholesky decomposition. 
   * 
   */
  C2F (dpotrf) ("L", &m2, &dwork[id12], &m2, &info2, 1L);
  if (info2 > 0)
    {
      *info = 1;
      return 0;
    }
  /*            _ 
   *    Compute Z = In - Y*X/gamma^2 and its LU factorization in AK . 
   * 
   */
  iwrk = iw1;
  C2F (dlaset) ("Full", n, n, &c_b7, &c_b8, &ak[ak_offset], ldak, 4L);
  /*Computing 2nd power 
   */
  d__2 = *gamma;
  d__1 = -1. / (d__2 * d__2);
  C2F (dgemm) ("N", "N", n, n, n, &d__1, &y[y_offset], ldy, &x[x_offset], ldx,
	       &c_b8, &ak[ak_offset], ldak, 1L, 1L);
  anorm = C2F (dlange) ("1", n, n, &ak[ak_offset], ldak, &dwork[iwrk], 1L);
  C2F (dgetrf) (n, n, &ak[ak_offset], ldak, &iwork[1], &info2);
  if (info2 > 0)
    {
      *info = 1;
      return 0;
    }
  C2F (dgecon) ("1", n, &ak[ak_offset], ldak, &anorm, &rcond, &dwork[iwrk],
		&iwork[*n + 1], info, 1L);
  /* 
   *    Return if the matrix is singular to working precision. 
   * 
   */
  if (rcond < eps)
    {
      *info = 1;
      return 0;
    }
  /* 
   */
  iwb = iw1;
  iwc = iwb + *n * np2;
  iw1 = iwc + (m2 + np2) * *n;
  iw2 = iw1 + *n * m2;
  /* 
   *    Compute C2' + F12' in BK . 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = np2;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  bk[j + i__ * bk_dim1] =
	    c__[np1 + i__ + j * c_dim1] + f[nd2 + i__ + j * f_dim1];
	  /* L30: */
	}
      /* L40: */
    }
  /*                                                         _ 
   *    Compute the transpose of (C2 + F12)*Z , with Z = inv(Z) . 
   * 
   */
  C2F (dgetrs) ("Transpose", n, &np2, &ak[ak_offset], ldak, &iwork[1],
		&bk[bk_offset], ldbk, &info2, 9L);
  /* 
   *    Compute the transpose of F2*Z . 
   * 
   */
  nsp_slicot_ma02ad ("Full", &m2, n, &f[m1 + 1 + f_dim1], ldf, &dwork[iw1], n,
		     4L);
  C2F (dgetrs) ("Transpose", n, &m2, &ak[ak_offset], ldak, &iwork[1],
		&dwork[iw1], n, &info2, 9L);
  /* 
   *    Compute the transpose of C1HAT = F2*Z - D11HAT*(C2 + F12)*Z . 
   * 
   */
  C2F (dgemm) ("N", "T", n, &m2, &np2, &c_b19, &bk[bk_offset], ldbk,
	       &dwork[id11], &m2, &c_b8, &dwork[iw1], n, 1L, 1L);
  /* 
   *    Compute CHAT . 
   * 
   */
  i__1 = m2 + np2;
  C2F (dgemm) ("N", "T", &m2, n, &m2, &c_b8, &tu[tu_offset], ldtu,
	       &dwork[iw1], n, &c_b7, &dwork[iwc], &i__1, 1L, 1L);
  i__1 = m2 + np2;
  nsp_slicot_ma02ad ("Full", n, &np2, &bk[bk_offset], ldbk, &dwork[iwc + m2],
		     &i__1, 4L);
  i__1 = m2 + np2;
  C2F (dtrmm) ("L", "U", "N", "N", &np2, n, &c_b19, &dwork[id21], &np2,
	       &dwork[iwc + m2], &i__1, 1L, 1L, 1L, 1L);
  /* 
   *    Compute B2 + H12 . 
   * 
   */
  ij = iw2;
  i__1 = m2;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  dwork[ij] =
	    b[i__ + (m1 + j) * b_dim1] + h__[i__ + (nd1 + j) * h_dim1];
	  ++ij;
	  /* L50: */
	}
      /* L60: */
    }
  /* 
   *    Compute A + HC in AK . 
   * 
   */
  C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &ak[ak_offset], ldak, 4L);
  C2F (dgemm) ("N", "N", n, n, np, &c_b8, &h__[h_offset], ldh, &c__[c_offset],
	       ldc, &c_b8, &ak[ak_offset], ldak, 1L, 1L);
  /* 
   *    Compute AHAT = A + HC + (B2 + H12)*C1HAT in AK . 
   * 
   */
  C2F (dgemm) ("N", "T", n, n, &m2, &c_b8, &dwork[iw2], n, &dwork[iw1], n,
	       &c_b8, &ak[ak_offset], ldak, 1L, 1L);
  /* 
   *    Compute B1HAT = -H2 + (B2 + H12)*D11HAT in BK . 
   * 
   */
  C2F (dlacpy) ("Full", n, &np2, &h__[(np1 + 1) * h_dim1 + 1], ldh,
		&bk[bk_offset], ldbk, 4L);
  C2F (dgemm) ("N", "N", n, &np2, &m2, &c_b8, &dwork[iw2], n, &dwork[id11],
	       &m2, &c_b19, &bk[bk_offset], ldbk, 1L, 1L);
  /* 
   *    Compute the first block of BHAT, BHAT1 . 
   * 
   */
  C2F (dgemm) ("N", "N", n, &np2, &np2, &c_b8, &bk[bk_offset], ldbk,
	       &ty[ty_offset], ldty, &c_b7, &dwork[iwb], n, 1L, 1L);
  /* 
   *    Compute Tu*D11HAT . 
   * 
   */
  C2F (dgemm) ("N", "N", &m2, &np2, &m2, &c_b8, &tu[tu_offset], ldtu,
	       &dwork[id11], &m2, &c_b7, &dwork[iw1], &m2, 1L, 1L);
  /* 
   *    Compute Tu*D11HAT*Ty in DK . 
   * 
   */
  C2F (dgemm) ("N", "N", &m2, &np2, &np2, &c_b8, &dwork[iw1], &m2,
	       &ty[ty_offset], ldty, &c_b7, &dk[dk_offset], lddk, 1L, 1L);
  /* 
   *    Compute P = Im2 + Tu*D11HAT*Ty*D22 and its condition. 
   * 
   */
  iw2 = iw1 + m2 * np2;
  iwrk = iw2 + m2 * m2;
  C2F (dlaset) ("Full", &m2, &m2, &c_b7, &c_b8, &dwork[iw2], &m2, 4L);
  C2F (dgemm) ("N", "N", &m2, &m2, &np2, &c_b8, &dk[dk_offset], lddk,
	       &d__[np1 + 1 + (m1 + 1) * d_dim1], ldd, &c_b8, &dwork[iw2],
	       &m2, 1L, 1L);
  anorm = C2F (dlange) ("1", &m2, &m2, &dwork[iw2], &m2, &dwork[iwrk], 1L);
  C2F (dgetrf) (&m2, &m2, &dwork[iw2], &m2, &iwork[1], &info2);
  if (info2 > 0)
    {
      *info = 2;
      return 0;
    }
  C2F (dgecon) ("1", &m2, &dwork[iw2], &m2, &anorm, &rcond, &dwork[iwrk],
		&iwork[m2 + 1], &info2, 1L);
  /* 
   *    Return if the matrix is singular to working precision. 
   * 
   */
  if (rcond < eps)
    {
      *info = 2;
      return 0;
    }
  /* 
   *    Find the controller matrix CK, CK = inv(P)*CHAT(1:M2,:) . 
   * 
   */
  i__1 = m2 + np2;
  C2F (dlacpy) ("Full", &m2, n, &dwork[iwc], &i__1, &ck[ck_offset], ldck, 4L);
  C2F (dgetrs) ("NoTranspose", &m2, n, &dwork[iw2], &m2, &iwork[1],
		&ck[ck_offset], ldck, &info2, 11L);
  /* 
   *    Find the controller matrices AK, BK, and DK, exploiting the 
   *    special structure of the relations. 
   * 
   *    Compute Q = Inp2 + D22*Tu*D11HAT*Ty and its LU factorization. 
   * 
   */
  iw3 = iw2 + np2 * np2;
  iw4 = iw3 + np2 * m2;
  iwrk = iw4 + np2 * np2;
  C2F (dlaset) ("Full", &np2, &np2, &c_b7, &c_b8, &dwork[iw2], &np2, 4L);
  C2F (dgemm) ("N", "N", &np2, &np2, &m2, &c_b8,
	       &d__[np1 + 1 + (m1 + 1) * d_dim1], ldd, &dk[dk_offset], lddk,
	       &c_b8, &dwork[iw2], &np2, 1L, 1L);
  C2F (dgetrf) (&np2, &np2, &dwork[iw2], &np2, &iwork[1], &info2);
  if (info2 > 0)
    {
      *info = 2;
      return 0;
    }
  /* 
   *    Compute A1 = inv(Q)*D22 and inv(Q) . 
   * 
   */
  C2F (dlacpy) ("Full", &np2, &m2, &d__[np1 + 1 + (m1 + 1) * d_dim1], ldd,
		&dwork[iw3], &np2, 4L);
  C2F (dgetrs) ("NoTranspose", &np2, &m2, &dwork[iw2], &np2, &iwork[1],
		&dwork[iw3], &np2, &info2, 11L);
  i__1 = *ldwork - iwrk + 1;
  C2F (dgetri) (&np2, &dwork[iw2], &np2, &iwork[1], &dwork[iwrk], &i__1,
		&info2);
  /*Computing MAX 
   */
  i__1 = (int) dwork[iwrk] + iwrk - 1;
  lwamax = Max (i__1, lwamax);
  /* 
   *    Compute A2 = ( inv(Ty) - inv(Q)*inv(Ty) - 
   *                   A1*Tu*D11HAT )*inv(D21HAT) . 
   * 
   */
  C2F (dlacpy) ("Full", &np2, &np2, &ty[ty_offset], ldty, &dwork[iw4], &np2,
		4L);
  C2F (dgetrf) (&np2, &np2, &dwork[iw4], &np2, &iwork[1], &info2);
  i__1 = *ldwork - iwrk + 1;
  C2F (dgetri) (&np2, &dwork[iw4], &np2, &iwork[1], &dwork[iwrk], &i__1,
		&info2);
  /* 
   */
  C2F (dlacpy) ("Full", &np2, &np2, &dwork[iw4], &np2, &dwork[iwrk], &np2,
		4L);
  C2F (dgemm) ("N", "N", &np2, &np2, &np2, &c_b19, &dwork[iw2], &np2,
	       &dwork[iwrk], &np2, &c_b8, &dwork[iw4], &np2, 1L, 1L);
  C2F (dgemm) ("N", "N", &np2, &np2, &m2, &c_b19, &dwork[iw3], &np2,
	       &dwork[iw1], &m2, &c_b8, &dwork[iw4], &np2, 1L, 1L);
  C2F (dtrmm) ("R", "U", "N", "N", &np2, &np2, &c_b8, &dwork[id21], &np2,
	       &dwork[iw4], &np2, 1L, 1L, 1L, 1L);
  /* 
   *    Compute [ A1  A2 ]*CHAT . 
   * 
   */
  i__1 = m2 + np2;
  i__2 = m2 + np2;
  C2F (dgemm) ("N", "N", &np2, n, &i__1, &c_b8, &dwork[iw3], &np2,
	       &dwork[iwc], &i__2, &c_b7, &dwork[iwrk], &np2, 1L, 1L);
  /* 
   *    Compute AK := AHAT - BHAT1*[ A1  A2 ]*CHAT . 
   * 
   */
  C2F (dgemm) ("N", "N", n, n, &np2, &c_b19, &dwork[iwb], n, &dwork[iwrk],
	       &np2, &c_b8, &ak[ak_offset], ldak, 1L, 1L);
  /* 
   *    Compute BK := BHAT1*inv(Q) . 
   * 
   */
  C2F (dgemm) ("N", "N", n, &np2, &np2, &c_b8, &dwork[iwb], n, &dwork[iw2],
	       &np2, &c_b7, &bk[bk_offset], ldbk, 1L, 1L);
  /* 
   *    Compute DK := Tu*D11HAT*Ty*inv(Q) . 
   * 
   */
  C2F (dgemm) ("N", "N", &m2, &np2, &np2, &c_b8, &dk[dk_offset], lddk,
	       &dwork[iw2], &np2, &c_b7, &dwork[iw3], &m2, 1L, 1L);
  C2F (dlacpy) ("Full", &m2, &np2, &dwork[iw3], &m2, &dk[dk_offset], lddk,
		4L);
  /* 
   */
  dwork[1] = (double) lwamax;
  return 0;
  /**** Last line of SB10RD *** 
   */
}				/* nsp_slicot_sb10rd */
