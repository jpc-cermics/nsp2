/* SB04PX.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__4 = 4;
static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_sb04px (int *ltranl, int *ltranr, int *isgn, int *n1,
		   int *n2, double *tl, int *ldtl, double *tr,
		   int *ldtr, double *b, int *ldb, double *scale,
		   double *x, int *ldx, double *xnorm, int *info)
{
  /* Initialized data */

  static int locu12[4] = { 3, 4, 1, 2 };
  static int locl21[4] = { 2, 1, 4, 3 };
  static int locu22[4] = { 4, 3, 2, 1 };
  static int xswpiv[4] = { FALSE, FALSE, TRUE, TRUE };
  static int bswpiv[4] = { FALSE, TRUE, FALSE, TRUE };

  /* System generated locals */
  int b_dim1, b_offset, tl_dim1, tl_offset, tr_dim1, tr_offset, x_dim1,
    x_offset;
  double d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8;

  /* Local variables */
  double btmp[4], smin;
  int ipiv;
  double temp;
  int jpiv[4];
  double xmax;
  int ipsv, jpsv, i__, j, k;
  int bswap;
  int xswap;
  double x2[2], l21, u11, u12;
  int ip, jp;
  double u22, t16[16] /* was [4][4] */ ;
  double smlnum, gam, bet, eps, sgn, tmp[4], tau1;

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
   *    To solve for the N1-by-N2 matrix X, 1 <= N1,N2 <= 2, in 
   * 
   *           op(TL)*X*op(TR) + ISGN*X = SCALE*B, 
   * 
   *    where TL is N1-by-N1, TR is N2-by-N2, B is N1-by-N2, and ISGN = 1 
   *    or -1.  op(T) = T or T', where T' denotes the transpose of T. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    LTRANL  INT 
   *            Specifies the form of op(TL) to be used, as follows: 
   *            = .FALSE.:  op(TL) = TL, 
   *            = .TRUE. :  op(TL) = TL'. 
   * 
   *    LTRANR  INT 
   *            Specifies the form of op(TR) to be used, as follows: 
   *            = .FALSE.:  op(TR) = TR, 
   *            = .TRUE. :  op(TR) = TR'. 
   * 
   *    ISGN    INT 
   *            Specifies the sign of the equation as described before. 
   *            ISGN may only be 1 or -1. 
   * 
   *    Input/Output Parameters 
   * 
   *    N1      (input) INT 
   *            The order of matrix TL.  N1 may only be 0, 1 or 2. 
   * 
   *    N2      (input) INT 
   *            The order of matrix TR.  N2 may only be 0, 1 or 2. 
   * 
   *    TL      (input) DOUBLE PRECISION array, dimension (LDTL,N1) 
   *            The leading N1-by-N1 part of this array must contain the 
   *            matrix TL. 
   * 
   *    LDTL    INT 
   *            The leading dimension of array TL.  LDTL >= MAX(1,N1). 
   * 
   *    TR      (input) DOUBLE PRECISION array, dimension (LDTR,N2) 
   *            The leading N2-by-N2 part of this array must contain the 
   *            matrix TR. 
   * 
   *    LDTR    INT 
   *            The leading dimension of array TR.  LDTR >= MAX(1,N2). 
   * 
   *    B       (input) DOUBLE PRECISION array, dimension (LDB,N2) 
   *            The leading N1-by-N2 part of this array must contain the 
   *            right-hand side of the equation. 
   * 
   *    LDB     INT 
   *            The leading dimension of array B.  LDB >= MAX(1,N1). 
   * 
   *    SCALE   (output) DOUBLE PRECISION 
   *            The scale factor. SCALE is chosen less than or equal to 1 
   *            to prevent the solution overflowing. 
   * 
   *    X       (output) DOUBLE PRECISION array, dimension (LDX,N2) 
   *            The leading N1-by-N2 part of this array contains the 
   *            solution of the equation. 
   *            Note that X may be identified with B in the calling 
   *            statement. 
   * 
   *    LDX     INT 
   *            The leading dimension of array X.  LDX >= MAX(1,N1). 
   * 
   *    XNORM   (output) DOUBLE PRECISION 
   *            The infinity-norm of the solution. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            = 1:  if TL and -ISGN*TR have almost reciprocal 
   *                  eigenvalues, so TL or TR is perturbed to get a 
   *                  nonsingular equation. 
   * 
   *            NOTE: In the interests of speed, this routine does not 
   *                  check the inputs for errors. 
   * 
   *    METHOD 
   * 
   *    The equivalent linear algebraic system of equations is formed and 
   *    solved using Gaussian elimination with complete pivoting. 
   * 
   *    REFERENCES 
   * 
   *    [1] Anderson, E., Bai, Z., Bischof, C., Demmel, J., Dongarra, J., 
   *        Du Croz, J., Greenbaum, A., Hammarling, S., McKenney, A., 
   *        Ostrouchov, S., and Sorensen, D. 
   *        LAPACK Users' Guide: Second Edition. 
   *        SIAM, Philadelphia, 1995. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The algorithm is stable and reliable, since Gaussian elimination 
   *    with complete pivoting is used. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, May 2000. 
   *    This is a modification and slightly more efficient version of 
   *    SLICOT Library routine SB03MU. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Discrete-time system, Sylvester equation, matrix algebra. 
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
   *    .. Local Arrays .. 
   *    .. 
   *    .. External Functions .. 
   *    .. 
   *    .. External Subroutines .. 
   *    .. 
   *    .. Intrinsic Functions .. 
   *    .. 
   *    .. Data statements .. 
   */
  /* Parameter adjustments */
  tl_dim1 = *ldtl;
  tl_offset = tl_dim1 + 1;
  tl -= tl_offset;
  tr_dim1 = *ldtr;
  tr_offset = tr_dim1 + 1;
  tr -= tr_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;

  /* Function Body */
  /*    .. 
   *    .. Executable Statements .. 
   * 
   *    Do not check the input parameters for errors. 
   * 
   */
  *info = 0;
  *scale = 1.;
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n1 == 0 || *n2 == 0)
    {
      *xnorm = 0.;
      return 0;
    }
  /* 
   *    Set constants to control overflow. 
   * 
   */
  eps = C2F (dlamch) ("P", 1L);
  smlnum = C2F (dlamch) ("S", 1L) / eps;
  sgn = (double) (*isgn);
  /* 
   */
  k = *n1 + *n1 + *n2 - 2;
  switch (k)
    {
    case 1:
      goto L10;
    case 2:
      goto L20;
    case 3:
      goto L30;
    case 4:
      goto L50;
    }
  /* 
   *    1-by-1: TL11*X*TR11 + ISGN*X = B11. 
   * 
   */
 L10:
  tau1 = tl[tl_dim1 + 1] * tr[tr_dim1 + 1] + sgn;
  bet = Abs (tau1);
  if (bet <= smlnum)
    {
      tau1 = smlnum;
      bet = smlnum;
      *info = 1;
    }
  /* 
   */
  gam = (d__1 = b[b_dim1 + 1], Abs (d__1));
  if (smlnum * gam > bet)
    {
      *scale = 1. / gam;
    }
  /* 
   */
  x[x_dim1 + 1] = b[b_dim1 + 1] * *scale / tau1;
  *xnorm = (d__1 = x[x_dim1 + 1], Abs (d__1));
  return 0;
  /* 
   *    1-by-2: 
   *    TL11*[X11 X12]*op[TR11 TR12] + ISGN*[X11 X12] = [B11 B12]. 
   *                     [TR21 TR22] 
   * 
   */
 L20:
  /* 
   *Computing MAX 
   *Computing MAX 
   */
  d__7 = (d__1 = tr[tr_dim1 + 1], Abs (d__1)), d__8 = (d__2 =
						       tr[(tr_dim1 << 1) + 1],
						       Abs (d__2)), d__7 =
    Max (d__7, d__8), d__8 = (d__3 = tr[tr_dim1 + 2], Abs (d__3)), d__7 =
    Max (d__7, d__8), d__8 = (d__4 = tr[(tr_dim1 << 1) + 2], Abs (d__4));
  d__6 = Max (d__7, d__8) * (d__5 = tl[tl_dim1 + 1], Abs (d__5)) * eps;
  smin = Max (d__6, smlnum);
  tmp[0] = tl[tl_dim1 + 1] * tr[tr_dim1 + 1] + sgn;
  tmp[3] = tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 2] + sgn;
  if (*ltranr)
    {
      tmp[1] = tl[tl_dim1 + 1] * tr[tr_dim1 + 2];
      tmp[2] = tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 1];
    }
  else
    {
      tmp[1] = tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 1];
      tmp[2] = tl[tl_dim1 + 1] * tr[tr_dim1 + 2];
    }
  btmp[0] = b[b_dim1 + 1];
  btmp[1] = b[(b_dim1 << 1) + 1];
  goto L40;
  /* 
   *    2-by-1: 
   *    op[TL11 TL12]*[X11]*TR11 + ISGN*[X11] = [B11]. 
   *      [TL21 TL22] [X21]             [X21]   [B21] 
   * 
   */
 L30:
  /*Computing MAX 
   *Computing MAX 
   */
  d__7 = (d__1 = tl[tl_dim1 + 1], Abs (d__1)), d__8 = (d__2 =
						       tl[(tl_dim1 << 1) + 1],
						       Abs (d__2)), d__7 =
    Max (d__7, d__8), d__8 = (d__3 = tl[tl_dim1 + 2], Abs (d__3)), d__7 =
    Max (d__7, d__8), d__8 = (d__4 = tl[(tl_dim1 << 1) + 2], Abs (d__4));
  d__6 = Max (d__7, d__8) * (d__5 = tr[tr_dim1 + 1], Abs (d__5)) * eps;
  smin = Max (d__6, smlnum);
  tmp[0] = tl[tl_dim1 + 1] * tr[tr_dim1 + 1] + sgn;
  tmp[3] = tl[(tl_dim1 << 1) + 2] * tr[tr_dim1 + 1] + sgn;
  if (*ltranl)
    {
      tmp[1] = tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 1];
      tmp[2] = tl[tl_dim1 + 2] * tr[tr_dim1 + 1];
    }
  else
    {
      tmp[1] = tl[tl_dim1 + 2] * tr[tr_dim1 + 1];
      tmp[2] = tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 1];
    }
  btmp[0] = b[b_dim1 + 1];
  btmp[1] = b[b_dim1 + 2];
 L40:
  /* 
   *    Solve 2-by-2 system using complete pivoting. 
   *    Set pivots less than SMIN to SMIN. 
   * 
   */
  ipiv = C2F (idamax) (&c__4, tmp, &c__1);
  u11 = tmp[ipiv - 1];
  if (Abs (u11) <= smin)
    {
      *info = 1;
      u11 = smin;
    }
  u12 = tmp[locu12[ipiv - 1] - 1];
  l21 = tmp[locl21[ipiv - 1] - 1] / u11;
  u22 = tmp[locu22[ipiv - 1] - 1] - u12 * l21;
  xswap = xswpiv[ipiv - 1];
  bswap = bswpiv[ipiv - 1];
  if (Abs (u22) <= smin)
    {
      *info = 1;
      u22 = smin;
    }
  if (bswap)
    {
      temp = btmp[1];
      btmp[1] = btmp[0] - l21 * temp;
      btmp[0] = temp;
    }
  else
    {
      btmp[1] -= l21 * btmp[0];
    }
  if (smlnum * 2. * Abs (btmp[1]) > Abs (u22)
      || smlnum * 2. * Abs (btmp[0]) > Abs (u11))
    {
      /*Computing MAX 
       */
      d__1 = Abs (btmp[0]), d__2 = Abs (btmp[1]);
      *scale = .5 / Max (d__1, d__2);
      btmp[0] *= *scale;
      btmp[1] *= *scale;
    }
  x2[1] = btmp[1] / u22;
  x2[0] = btmp[0] / u11 - u12 / u11 * x2[1];
  if (xswap)
    {
      temp = x2[1];
      x2[1] = x2[0];
      x2[0] = temp;
    }
  x[x_dim1 + 1] = x2[0];
  if (*n1 == 1)
    {
      x[(x_dim1 << 1) + 1] = x2[1];
      *xnorm = Abs (x2[0]) + Abs (x2[1]);
    }
  else
    {
      x[x_dim1 + 2] = x2[1];
      /*Computing MAX 
       */
      d__1 = Abs (x2[0]), d__2 = Abs (x2[1]);
      *xnorm = Max (d__1, d__2);
    }
  return 0;
  /* 
   *    2-by-2: 
   *    op[TL11 TL12]*[X11 X12]*op[TR11 TR12] + ISGN*[X11 X12] = [B11 B12] 
   *      [TL21 TL22] [X21 X22]   [TR21 TR22]        [X21 X22]   [B21 B22] 
   * 
   *    Solve equivalent 4-by-4 system using complete pivoting. 
   *    Set pivots less than SMIN to SMIN. 
   * 
   */
 L50:
  /*Computing MAX 
   */
  d__5 = (d__1 = tr[tr_dim1 + 1], Abs (d__1)), d__6 = (d__2 =
						       tr[(tr_dim1 << 1) + 1],
						       Abs (d__2)), d__5 =
    Max (d__5, d__6), d__6 = (d__3 = tr[tr_dim1 + 2], Abs (d__3)), d__5 =
    Max (d__5, d__6), d__6 = (d__4 = tr[(tr_dim1 << 1) + 2], Abs (d__4));
  smin = Max (d__5, d__6);
  /*Computing MAX 
   */
  d__5 = (d__1 = tl[tl_dim1 + 1], Abs (d__1)), d__6 = (d__2 =
						       tl[(tl_dim1 << 1) + 1],
						       Abs (d__2)), d__5 =
    Max (d__5, d__6), d__6 = (d__3 = tl[tl_dim1 + 2], Abs (d__3)), d__5 =
    Max (d__5, d__6), d__6 = (d__4 = tl[(tl_dim1 << 1) + 2], Abs (d__4));
  smin = Max (d__5, d__6) * smin;
  /*Computing MAX 
   */
  d__1 = eps * smin;
  smin = Max (d__1, smlnum);
  t16[0] = tl[tl_dim1 + 1] * tr[tr_dim1 + 1] + sgn;
  t16[5] = tl[(tl_dim1 << 1) + 2] * tr[tr_dim1 + 1] + sgn;
  t16[10] = tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 2] + sgn;
  t16[15] = tl[(tl_dim1 << 1) + 2] * tr[(tr_dim1 << 1) + 2] + sgn;
  if (*ltranl)
    {
      t16[4] = tl[tl_dim1 + 2] * tr[tr_dim1 + 1];
      t16[1] = tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 1];
      t16[14] = tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 2];
      t16[11] = tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 2];
    }
  else
    {
      t16[4] = tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 1];
      t16[1] = tl[tl_dim1 + 2] * tr[tr_dim1 + 1];
      t16[14] = tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 2];
      t16[11] = tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 2];
    }
  if (*ltranr)
    {
      t16[8] = tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 1];
      t16[13] = tl[(tl_dim1 << 1) + 2] * tr[(tr_dim1 << 1) + 1];
      t16[2] = tl[tl_dim1 + 1] * tr[tr_dim1 + 2];
      t16[7] = tl[(tl_dim1 << 1) + 2] * tr[tr_dim1 + 2];
    }
  else
    {
      t16[8] = tl[tl_dim1 + 1] * tr[tr_dim1 + 2];
      t16[13] = tl[(tl_dim1 << 1) + 2] * tr[tr_dim1 + 2];
      t16[2] = tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 1];
      t16[7] = tl[(tl_dim1 << 1) + 2] * tr[(tr_dim1 << 1) + 1];
    }
  if (*ltranl && *ltranr)
    {
      t16[12] = tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 1];
      t16[9] = tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 1];
      t16[6] = tl[tl_dim1 + 2] * tr[tr_dim1 + 2];
      t16[3] = tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 2];
    }
  else if (*ltranl && !(*ltranr))
    {
      t16[12] = tl[tl_dim1 + 2] * tr[tr_dim1 + 2];
      t16[9] = tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 2];
      t16[6] = tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 1];
      t16[3] = tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 1];
    }
  else if (!(*ltranl) && *ltranr)
    {
      t16[12] = tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 1];
      t16[9] = tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 1];
      t16[6] = tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 2];
      t16[3] = tl[tl_dim1 + 2] * tr[tr_dim1 + 2];
    }
  else
    {
      t16[12] = tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 2];
      t16[9] = tl[tl_dim1 + 2] * tr[tr_dim1 + 2];
      t16[6] = tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 1];
      t16[3] = tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 1];
    }
  btmp[0] = b[b_dim1 + 1];
  btmp[1] = b[b_dim1 + 2];
  btmp[2] = b[(b_dim1 << 1) + 1];
  btmp[3] = b[(b_dim1 << 1) + 2];
  /* 
   *    Perform elimination. 
   * 
   */
  for (i__ = 1; i__ <= 3; ++i__)
    {
      xmax = 0.;
      /* 
       */
      for (ip = i__; ip <= 4; ++ip)
	{
	  /* 
	   */
	  for (jp = i__; jp <= 4; ++jp)
	    {
	      if ((d__1 = t16[ip + (jp << 2) - 5], Abs (d__1)) >= xmax)
		{
		  xmax = (d__1 = t16[ip + (jp << 2) - 5], Abs (d__1));
		  ipsv = ip;
		  jpsv = jp;
		}
	      /* L60: */
	    }
	  /* 
	   */
	  /* L70: */
	}
      /* 
       */
      if (ipsv != i__)
	{
	  C2F (dswap) (&c__4, &t16[ipsv - 1], &c__4, &t16[i__ - 1], &c__4);
	  temp = btmp[i__ - 1];
	  btmp[i__ - 1] = btmp[ipsv - 1];
	  btmp[ipsv - 1] = temp;
	}
      if (jpsv != i__)
	{
	  C2F (dswap) (&c__4, &t16[(jpsv << 2) - 4], &c__1,
		       &t16[(i__ << 2) - 4], &c__1);
	}
      jpiv[i__ - 1] = jpsv;
      if ((d__1 = t16[i__ + (i__ << 2) - 5], Abs (d__1)) < smin)
	{
	  *info = 1;
	  t16[i__ + (i__ << 2) - 5] = smin;
	}
      /* 
       */
      for (j = i__ + 1; j <= 4; ++j)
	{
	  t16[j + (i__ << 2) - 5] /= t16[i__ + (i__ << 2) - 5];
	  btmp[j - 1] -= t16[j + (i__ << 2) - 5] * btmp[i__ - 1];
	  /* 
	   */
	  for (k = i__ + 1; k <= 4; ++k)
	    {
	      t16[j + (k << 2) - 5] -=
		t16[j + (i__ << 2) - 5] * t16[i__ + (k << 2) - 5];
	      /* L80: */
	    }
	  /* 
	   */
	  /* L90: */
	}
      /* 
       */
      /* L100: */
    }
  /* 
   */
  if (Abs (t16[15]) < smin)
    {
      t16[15] = smin;
    }
  if (smlnum * 8. * Abs (btmp[0]) > Abs (t16[0])
      || smlnum * 8. * Abs (btmp[1]) > Abs (t16[5])
      || smlnum * 8. * Abs (btmp[2]) > Abs (t16[10])
      || smlnum * 8. * Abs (btmp[3]) > Abs (t16[15]))
    {
      /*Computing MAX 
       */
      d__1 = Abs (btmp[0]), d__2 = Abs (btmp[1]), d__1 =
	Max(d__1, d__2), d__2 = Abs (btmp[2]), d__1 =
	Max(d__1, d__2), d__2 = Abs (btmp[3]);
      *scale = .125 / Max (d__1, d__2);
      btmp[0] *= *scale;
      btmp[1] *= *scale;
      btmp[2] *= *scale;
      btmp[3] *= *scale;
    }
  /* 
   */
  for (i__ = 1; i__ <= 4; ++i__)
    {
      k = 5 - i__;
      temp = 1. / t16[k + (k << 2) - 5];
      tmp[k - 1] = btmp[k - 1] * temp;
      /* 
       */
      for (j = k + 1; j <= 4; ++j)
	{
	  tmp[k - 1] -= temp * t16[k + (j << 2) - 5] * tmp[j - 1];
	  /* L110: */
	}
      /* 
       */
      /* L120: */
    }
  /* 
   */
  for (i__ = 1; i__ <= 3; ++i__)
    {
      if (jpiv[4 - i__ - 1] != 4 - i__)
	{
	  temp = tmp[4 - i__ - 1];
	  tmp[4 - i__ - 1] = tmp[jpiv[4 - i__ - 1] - 1];
	  tmp[jpiv[4 - i__ - 1] - 1] = temp;
	}
      /* L130: */
    }
  /* 
   */
  x[x_dim1 + 1] = tmp[0];
  x[x_dim1 + 2] = tmp[1];
  x[(x_dim1 << 1) + 1] = tmp[2];
  x[(x_dim1 << 1) + 2] = tmp[3];
  /*Computing MAX 
   */
  d__1 = Abs (tmp[0]) + Abs (tmp[2]), d__2 = Abs (tmp[1]) + Abs (tmp[3]);
  *xnorm = Max (d__1, d__2);
  /* 
   */
  return 0;
  /**** Last line of SB04PX *** 
   */
}				/* nsp_slicot_sb04px */
