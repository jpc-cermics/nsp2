
#include "ctrlpack.h"

static int nsp_ctrlpack_voiddummy (const double *ar, const double *ai);

/* Table of constant values */

static int c__9 = 9;
static int c__0 = 0;
static int c__1 = 1;
static int c__3 = 3;
static int c__4 = 4;
static int c__16 = 16;
static double c_b79 = -1.;
static double c_b82 = 1.;
static double c_b83 = 0.;
static int c_true = TRUE;
static int c__2 = 2;
static int c_false = FALSE;
static double c_b806 = .5;

/* 
 * -- RICCPACK auxiliary routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * DLALD2 solves for the 2 by 2 symmetric matrix X in 
 * 
 *        op(T')*X*op(T) - X = SCALE*B, 
 * 
 * where T is 2 by 2, B is symmetric 2 by 2, and op(T) = T or T', 
 * where T' denotes the transpose of T. 
 * 
 * Arguments 
 * ========= 
 * 
 * LTRAN   (input) INT 
 *         On entry, LTRAN specifies the op(T): 
 *            = .FALSE., op(T) = T, 
 *            = .TRUE., op(T) = T'. 
 * 
 * T       (input) DOUBLE PRECISION array, dimension (LDT,2) 
 *         On entry, T contains an 2 by 2 matrix. 
 * 
 * LDT     (input) INT 
 *         The leading dimension of the matrix T. LDT >= 2. 
 * 
 * B       (input) DOUBLE PRECISION array, dimension (LDB,2) 
 *         On entry, the 2 by 2 matrix B contains the symmetric 
 *         right-hand side of the equation. 
 * 
 * LDB     (input) INT 
 *         The leading dimension of the matrix B. LDB >= 2. 
 * 
 * SCALE   (output) DOUBLE PRECISION 
 *         On exit, SCALE contains the scale factor. SCALE is chosen 
 *         less than or equal to 1 to prevent the solution overflowing. 
 * 
 * X       (output) DOUBLE PRECISION array, dimension (LDX,2) 
 *         On exit, X contains the 2 by 2 symmetric solution. 
 * 
 * LDX     (input) INT 
 *         The leading dimension of the matrix X. LDX >= 2. 
 * 
 * XNORM   (output) DOUBLE PRECISION 
 *         On exit, XNORM is the infinity-norm of the solution. 
 * 
 * INFO    (output) INT 
 *         On exit, INFO is set to 
 *            0: successful exit. 
 *            1: T has almost reciprocal eigenvalues, so T 
 *               is perturbed to get a nonsingular equation. 
 *         NOTE: In the interests of speed, this routine does not 
 *               check the inputs for errors. 
 * 
 */


int
nsp_ctrlpack_dlald2 (int *ltran, double *t, int *ldt, double *b, int *ldb,
		     double *scale, double *x, int *ldx, double *xnorm,
		     int *info)
{
  /* System generated locals */
  int b_dim1, b_offset, t_dim1, t_offset, x_dim1, x_offset;
  double d__1, d__2, d__3, d__4, d__5, d__6;

  /* Local variables */
  double btmp[3], temp, smin;
  int jpiv[3];
  double xmax;
  int ipsv=0, jpsv=0, i__, j, k;
  double t9[9] /* was [3][3] */ ;
  int ip, jp;
  double smlnum, eps, tmp[3];

  /* Parameter adjustments */
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;

  /* Function Body */
  *info = 0;
  /* 
   *    Set constants to control overflow 
   * 
   */
  eps = C2F (dlamch) ("P", 1L);
  smlnum = C2F (dlamch) ("S", 1L) / eps;
  /* 
   *    Solve equivalent 3 by 3 system using complete pivoting. 
   *    Set pivots less than SMIN to SMIN. 
   * 
   *Computing MAX 
   */
  d__5 = (d__1 = t[t_dim1 + 1], Abs (d__1)), d__6 = (d__2 =
						     t[(t_dim1 << 1) + 1],
						     Abs (d__2)), d__5 =
    Max (d__5, d__6), d__6 = (d__3 = t[t_dim1 + 2], Abs (d__3)), d__5 =
    Max (d__5, d__6), d__6 = (d__4 = t[(t_dim1 << 1) + 2], Abs (d__4));
  smin = Max (d__5, d__6);
  /*Computing MAX 
   */
  d__1 = eps * smin;
  smin = Max (d__1, smlnum);
  btmp[0] = 0.;
  C2F (dcopy) (&c__9, btmp, &c__0, t9, &c__1);
  t9[0] = t[t_dim1 + 1] * t[t_dim1 + 1] - 1.;
  t9[4] =
    t[t_dim1 + 1] * t[(t_dim1 << 1) + 2] + t[(t_dim1 << 1) + 1] * t[t_dim1 +
								    2] - 1.;
  t9[8] = t[(t_dim1 << 1) + 2] * t[(t_dim1 << 1) + 2] - 1.;
  if (*ltran)
    {
      t9[3] =
	t[t_dim1 + 1] * t[(t_dim1 << 1) + 1] + t[t_dim1 +
						 1] * t[(t_dim1 << 1) + 1];
      t9[6] = t[(t_dim1 << 1) + 1] * t[(t_dim1 << 1) + 1];
      t9[1] = t[t_dim1 + 1] * t[t_dim1 + 2];
      t9[7] = t[(t_dim1 << 1) + 1] * t[(t_dim1 << 1) + 2];
      t9[2] = t[t_dim1 + 2] * t[t_dim1 + 2];
      t9[5] =
	t[t_dim1 + 2] * t[(t_dim1 << 1) + 2] + t[t_dim1 +
						 2] * t[(t_dim1 << 1) + 2];
    }
  else
    {
      t9[3] = t[t_dim1 + 1] * t[t_dim1 + 2] + t[t_dim1 + 1] * t[t_dim1 + 2];
      t9[6] = t[t_dim1 + 2] * t[t_dim1 + 2];
      t9[1] = t[t_dim1 + 1] * t[(t_dim1 << 1) + 1];
      t9[7] = t[t_dim1 + 2] * t[(t_dim1 << 1) + 2];
      t9[2] = t[(t_dim1 << 1) + 1] * t[(t_dim1 << 1) + 1];
      t9[5] =
	t[(t_dim1 << 1) + 1] * t[(t_dim1 << 1) + 2] + t[(t_dim1 << 1) +
							1] * t[(t_dim1 << 1) +
							       2];
    }
  btmp[0] = b[b_dim1 + 1];
  btmp[1] = b[b_dim1 + 2];
  btmp[2] = b[(b_dim1 << 1) + 2];
  /* 
   *    Perform elimination 
   * 
   */
  for (i__ = 1; i__ <= 2; ++i__)
    {
      xmax = 0.;
      for (ip = i__; ip <= 3; ++ip)
	{
	  for (jp = i__; jp <= 3; ++jp)
	    {
	      if ((d__1 = t9[ip + jp * 3 - 4], Abs (d__1)) >= xmax)
		{
		  xmax = (d__1 = t9[ip + jp * 3 - 4], Abs (d__1));
		  ipsv = ip;
		  jpsv = jp;
		}
	      /* L10: */
	    }
	  /* L20: */
	}
      if (ipsv != i__)
	{
	  C2F (dswap) (&c__3, &t9[ipsv - 1], &c__3, &t9[i__ - 1], &c__3);
	  temp = btmp[i__ - 1];
	  btmp[i__ - 1] = btmp[ipsv - 1];
	  btmp[ipsv - 1] = temp;
	}
      if (jpsv != i__)
	{
	  C2F (dswap) (&c__3, &t9[jpsv * 3 - 3], &c__1,
		       &t9[i__ * 3 - 3], &c__1);
	}
      jpiv[i__ - 1] = jpsv;
      if ((d__1 = t9[i__ + i__ * 3 - 4], Abs (d__1)) < smin)
	{
	  *info = 1;
	  t9[i__ + i__ * 3 - 4] = smin;
	}
      for (j = i__ + 1; j <= 3; ++j)
	{
	  t9[j + i__ * 3 - 4] /= t9[i__ + i__ * 3 - 4];
	  btmp[j - 1] -= t9[j + i__ * 3 - 4] * btmp[i__ - 1];
	  for (k = i__ + 1; k <= 3; ++k)
	    {
	      t9[j + k * 3 - 4] -= t9[j + i__ * 3 - 4] * t9[i__ + k * 3 - 4];
	      /* L30: */
	    }
	  /* L40: */
	}
      /* L50: */
    }
  if (Abs (t9[8]) < smin)
    {
      t9[8] = smin;
    }
  *scale = 1.;
  if (smlnum * 4. * Abs (btmp[0]) > Abs (t9[0])
      || smlnum * 4. * Abs (btmp[1]) > Abs (t9[4])
      || smlnum * 4. * Abs (btmp[2]) > Abs (t9[8]))
    {
      /*Computing MAX 
       */
      d__1 = Abs (btmp[0]), d__2 = Abs (btmp[1]), d__1 =
	Max (d__1, d__2), d__2 = Abs (btmp[2]);
      *scale = .25 / Max (d__1, d__2);
      btmp[0] *= *scale;
      btmp[1] *= *scale;
      btmp[2] *= *scale;
    }
  for (i__ = 1; i__ <= 3; ++i__)
    {
      k = 4 - i__;
      temp = 1. / t9[k + k * 3 - 4];
      tmp[k - 1] = btmp[k - 1] * temp;
      for (j = k + 1; j <= 3; ++j)
	{
	  tmp[k - 1] -= temp * t9[k + j * 3 - 4] * tmp[j - 1];
	  /* L60: */
	}
      /* L70: */
    }
  for (i__ = 1; i__ <= 2; ++i__)
    {
      if (jpiv[3 - i__ - 1] != 3 - i__)
	{
	  temp = tmp[3 - i__ - 1];
	  tmp[3 - i__ - 1] = tmp[jpiv[3 - i__ - 1] - 1];
	  tmp[jpiv[3 - i__ - 1] - 1] = temp;
	}
      /* L80: */
    }
  x[x_dim1 + 1] = tmp[0];
  x[x_dim1 + 2] = tmp[1];
  x[(x_dim1 << 1) + 1] = tmp[1];
  x[(x_dim1 << 1) + 2] = tmp[2];
  /*Computing MAX 
   */
  d__1 = Abs (tmp[0]) + Abs (tmp[1]), d__2 = Abs (tmp[1]) + Abs (tmp[2]);
  *xnorm = Max (d__1, d__2);
  return 0;
  /* 
   *    End of DLALD2 
   * 
   */
}				/* dlald2_ */

/* 
 * -- RICCPACK auxiliary routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * DLALY2 solves for the 2 by 2 symmetric matrix X in 
 * 
 *        op(T')*X + X*op(T) = SCALE*B, 
 * 
 * where T is 2 by 2, B is symmetric 2 by 2, and op(T) = T or T', 
 * where T' denotes the transpose of T. 
 * 
 * Arguments 
 * ========= 
 * 
 * LTRAN   (input) INT 
 *         On entry, LTRAN specifies the op(T): 
 *            = .FALSE., op(T) = T, 
 *            = .TRUE., op(T) = T'. 
 * 
 * T       (input) DOUBLE PRECISION array, dimension (LDT,2) 
 *         On entry, T contains an 2 by 2 matrix. 
 * 
 * LDT     (input) INT 
 *         The leading dimension of the matrix T. LDT >= 2. 
 * 
 * B       (input) DOUBLE PRECISION array, dimension (LDB,2) 
 *         On entry, the 2 by 2 matrix B contains the symmetric 
 *         right-hand side of the equation. 
 * 
 * LDB     (input) INT 
 *         The leading dimension of the matrix B. LDB >= 2. 
 * 
 * SCALE   (output) DOUBLE PRECISION 
 *         On exit, SCALE contains the scale factor. SCALE is chosen 
 *         less than or equal to 1 to prevent the solution overflowing. 
 * 
 * X       (output) DOUBLE PRECISION array, dimension (LDX,2) 
 *         On exit, X contains the 2 by 2 symmetric solution. 
 * 
 * LDX     (input) INT 
 *         The leading dimension of the matrix X. LDX >= 2. 
 * 
 * XNORM   (output) DOUBLE PRECISION 
 *         On exit, XNORM is the infinity-norm of the solution. 
 * 
 * INFO    (output) INT 
 *         On exit, INFO is set to 
 *            0: successful exit. 
 *            1: T and -T have too close eigenvalues, so T 
 *               is perturbed to get a nonsingular equation. 
 *         NOTE: In the interests of speed, this routine does not 
 *               check the inputs for errors. 
 */

int
nsp_ctrlpack_dlaly2 (int *ltran, double *t, int *ldt, double *b, int *ldb,
		     double *scale, double *x, int *ldx, double *xnorm,
		     int *info)
{
  int b_dim1, b_offset, t_dim1, t_offset, x_dim1, x_offset;
  double d__1, d__2, d__3, d__4, d__5, d__6;


  double btmp[3], temp, smin;
  int jpiv[3];
  double xmax;
  int ipsv=0, jpsv=0, i__, j, k;
  double t9[9] /* was [3][3] */ ;
  int ip, jp;
  double smlnum, eps, tmp[3];

  /* Parameter adjustments */
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;

  /* Function Body */
  *info = 0;
  /* 
   *    Set constants to control overflow 
   * 
   */
  eps = C2F (dlamch) ("P", 1L);
  smlnum = C2F (dlamch) ("S", 1L) / eps;
  /* 
   *    Solve equivalent 3 by 3 system using complete pivoting. 
   *    Set pivots less than SMIN to SMIN. 
   * 
   *Computing MAX 
   */
  d__5 = (d__1 = t[t_dim1 + 1], Abs (d__1)), d__6 = (d__2 =
						     t[(t_dim1 << 1) + 1],
						     Abs (d__2)), d__5 =
    Max (d__5, d__6), d__6 = (d__3 = t[t_dim1 + 2], Abs (d__3)), d__5 =
    Max (d__5, d__6), d__6 = (d__4 = t[(t_dim1 << 1) + 2], Abs (d__4));
  smin = Max (d__5, d__6);
  /*Computing MAX 
   */
  d__1 = eps * smin;
  smin = Max (d__1, smlnum);
  btmp[0] = 0.;
  C2F (dcopy) (&c__9, btmp, &c__0, t9, &c__1);
  t9[0] = t[t_dim1 + 1] + t[t_dim1 + 1];
  t9[4] = t[t_dim1 + 1] + t[(t_dim1 << 1) + 2];
  t9[8] = t[(t_dim1 << 1) + 2] + t[(t_dim1 << 1) + 2];
  if (*ltran)
    {
      t9[3] = t[(t_dim1 << 1) + 1] + t[(t_dim1 << 1) + 1];
      t9[1] = t[t_dim1 + 2];
      t9[7] = t[(t_dim1 << 1) + 1];
      t9[5] = t[t_dim1 + 2] + t[t_dim1 + 2];
    }
  else
    {
      t9[3] = t[t_dim1 + 2] + t[t_dim1 + 2];
      t9[1] = t[(t_dim1 << 1) + 1];
      t9[7] = t[t_dim1 + 2];
      t9[5] = t[(t_dim1 << 1) + 1] + t[(t_dim1 << 1) + 1];
    }
  btmp[0] = b[b_dim1 + 1];
  btmp[1] = b[b_dim1 + 2];
  btmp[2] = b[(b_dim1 << 1) + 2];
  /* 
   *    Perform elimination 
   * 
   */
  for (i__ = 1; i__ <= 2; ++i__)
    {
      xmax = 0.;
      for (ip = i__; ip <= 3; ++ip)
	{
	  for (jp = i__; jp <= 3; ++jp)
	    {
	      if ((d__1 = t9[ip + jp * 3 - 4], Abs (d__1)) >= xmax)
		{
		  xmax = (d__1 = t9[ip + jp * 3 - 4], Abs (d__1));
		  ipsv = ip;
		  jpsv = jp;
		}
	      /* L10: */
	    }
	  /* L20: */
	}
      if (ipsv != i__)
	{
	  C2F (dswap) (&c__3, &t9[ipsv - 1], &c__3, &t9[i__ - 1], &c__3);
	  temp = btmp[i__ - 1];
	  btmp[i__ - 1] = btmp[ipsv - 1];
	  btmp[ipsv - 1] = temp;
	}
      if (jpsv != i__)
	{
	  C2F (dswap) (&c__3, &t9[jpsv * 3 - 3], &c__1,
		       &t9[i__ * 3 - 3], &c__1);
	}
      jpiv[i__ - 1] = jpsv;
      if ((d__1 = t9[i__ + i__ * 3 - 4], Abs (d__1)) < smin)
	{
	  *info = 1;
	  t9[i__ + i__ * 3 - 4] = smin;
	}
      for (j = i__ + 1; j <= 3; ++j)
	{
	  t9[j + i__ * 3 - 4] /= t9[i__ + i__ * 3 - 4];
	  btmp[j - 1] -= t9[j + i__ * 3 - 4] * btmp[i__ - 1];
	  for (k = i__ + 1; k <= 3; ++k)
	    {
	      t9[j + k * 3 - 4] -= t9[j + i__ * 3 - 4] * t9[i__ + k * 3 - 4];
	      /* L30: */
	    }
	  /* L40: */
	}
      /* L50: */
    }
  if (Abs (t9[8]) < smin)
    {
      t9[8] = smin;
    }
  *scale = 1.;
  if (smlnum * 4. * Abs (btmp[0]) > Abs (t9[0])
      || smlnum * 4. * Abs (btmp[1]) > Abs (t9[4])
      || smlnum * 4. * Abs (btmp[2]) > Abs (t9[8]))
    {
      /*Computing MAX 
       */
      d__1 = Abs (btmp[0]), d__2 = Abs (btmp[1]), d__1 =
	Max (d__1, d__2), d__2 = Abs (btmp[2]);
      *scale = .25 / Max (d__1, d__2);
      btmp[0] *= *scale;
      btmp[1] *= *scale;
      btmp[2] *= *scale;
    }
  for (i__ = 1; i__ <= 3; ++i__)
    {
      k = 4 - i__;
      temp = 1. / t9[k + k * 3 - 4];
      tmp[k - 1] = btmp[k - 1] * temp;
      for (j = k + 1; j <= 3; ++j)
	{
	  tmp[k - 1] -= temp * t9[k + j * 3 - 4] * tmp[j - 1];
	  /* L60: */
	}
      /* L70: */
    }
  for (i__ = 1; i__ <= 2; ++i__)
    {
      if (jpiv[3 - i__ - 1] != 3 - i__)
	{
	  temp = tmp[3 - i__ - 1];
	  tmp[3 - i__ - 1] = tmp[jpiv[3 - i__ - 1] - 1];
	  tmp[jpiv[3 - i__ - 1] - 1] = temp;
	}
      /* L80: */
    }
  x[x_dim1 + 1] = tmp[0];
  x[x_dim1 + 2] = tmp[1];
  x[(x_dim1 << 1) + 1] = tmp[1];
  x[(x_dim1 << 1) + 2] = tmp[2];
  /*Computing MAX 
   */
  d__1 = Abs (tmp[0]) + Abs (tmp[1]), d__2 = Abs (tmp[1]) + Abs (tmp[2]);
  *xnorm = Max (d__1, d__2);
  return 0;
  /* 
   *    End of DLALY2 
   * 
   */
}				/* dlaly2_ */


/* 
 * -- RICCPACK auxiliary routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * DLASD2 solves for the N1 by N2 matrix X, 1 <= N1,N2 <= 2, in 
 * 
 *        ISGN*op(TL)*X*op(TR) - X = SCALE*B, 
 * 
 * where TL is N1 by N1, TR is N2 by N2, B is N1 by N2, and ISGN = 1 or 
 * -1.  op(T) = T or T', where T' denotes the transpose of T. 
 * 
 * Arguments 
 * ========= 
 * 
 * LTRANL  (input) INT 
 *         On entry, LTRANL specifies the op(TL): 
 *            = .FALSE., op(TL) = TL, 
 *            = .TRUE., op(TL) = TL'. 
 * 
 * LTRANR  (input) INT 
 *         On entry, LTRANR specifies the op(TR): 
 *           = .FALSE., op(TR) = TR, 
 *           = .TRUE., op(TR) = TR'. 
 * 
 * ISGN    (input) INT 
 *         On entry, ISGN specifies the sign of the equation 
 *         as described before. ISGN may only be 1 or -1. 
 * 
 * N1      (input) INT 
 *         On entry, N1 specifies the order of matrix TL. 
 *         N1 may only be 0, 1 or 2. 
 * 
 * N2      (input) INT 
 *         On entry, N2 specifies the order of matrix TR. 
 *         N2 may only be 0, 1 or 2. 
 * 
 * TL      (input) DOUBLE PRECISION array, dimension (LDTL,2) 
 *         On entry, TL contains an N1 by N1 matrix. 
 * 
 * LDTL    (input) INT 
 *         The leading dimension of the matrix TL. LDTL >= Max(1,N1). 
 * 
 * TR      (input) DOUBLE PRECISION array, dimension (LDTR,2) 
 *         On entry, TR contains an N2 by N2 matrix. 
 * 
 * LDTR    (input) INT 
 *         The leading dimension of the matrix TR. LDTR >= Max(1,N2). 
 * 
 * B       (input) DOUBLE PRECISION array, dimension (LDB,2) 
 *         On entry, the N1 by N2 matrix B contains the right-hand 
 *         side of the equation. 
 * 
 * LDB     (input) INT 
 *         The leading dimension of the matrix B. LDB >= Max(1,N1). 
 * 
 * SCALE   (output) DOUBLE PRECISION 
 *         On exit, SCALE contains the scale factor. SCALE is chosen 
 *         less than or equal to 1 to prevent the solution overflowing. 
 * 
 * X       (output) DOUBLE PRECISION array, dimension (LDX,2) 
 *         On exit, X contains the N1 by N2 solution. 
 * 
 * LDX     (input) INT 
 *         The leading dimension of the matrix X. LDX >= Max(1,N1). 
 * 
 * XNORM   (output) DOUBLE PRECISION 
 *         On exit, XNORM is the infinity-norm of the solution. 
 * 
 * INFO    (output) INT 
 *         On exit, INFO is set to 
 *            0: successful exit. 
 *            1: TL and TR have almost reciprocal eigenvalues, so TL or 
 *               TR is perturbed to get a nonsingular equation. 
 *         NOTE: In the interests of speed, this routine does not 
 *               check the inputs for errors. 
 * 
 */

int
nsp_ctrlpack_dlasd2 (int *ltranl, int *ltranr, int *isgn, int *n1, int *n2,
		     double *tl, int *ldtl, double *tr, int *ldtr, double *b,
		     int *ldb, double *scale, double *x, int *ldx,
		     double *xnorm, int *info)
{
  /* Initialized data */

  const int locu12[4] = { 3, 4, 1, 2 };
  const int locl21[4] = { 2, 1, 4, 3 };
  const int locu22[4] = { 4, 3, 2, 1 };
  const int xswpiv[4] = { FALSE, FALSE, TRUE, TRUE };
  const int bswpiv[4] = { FALSE, TRUE, FALSE, TRUE };

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
  int ipsv=0, jpsv=0, i__, j, k;
  int bswap;
  int xswap;
  double x2[2], l21, u11, u12;
  int ip, jp;
  double u22, t16[16] /* was [4][4] */ ;
  double smlnum, gam, bet, eps, sgn, tmp[4], tau1;

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
   *    Do not check the input parameters for errors 
   * 
   */
  *info = 0;
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n1 == 0 || *n2 == 0)
    {
      return 0;
    }
  /* 
   *    Set constants to control overflow 
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
   *    1 by 1: SGN*TL11*X*TR11 - X = B11 
   * 
   */
 L10:
  tau1 = sgn * tl[tl_dim1 + 1] * tr[tr_dim1 + 1] - 1.;
  bet = Abs (tau1);
  if (bet <= smlnum)
    {
      tau1 = smlnum;
      bet = smlnum;
      *info = 1;
    }
  /* 
   */
  *scale = 1.;
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
   *    1 by 2: 
   *    ISGN*TL11*[X11 X12]*op[TR11 TR12]  = [B11 B12] 
   *                          [TR21 TR22] 
   * 
   */
 L20:
  /* 
   *Computing MAX 
   *Computing MAX 
   */
  d__7 = (d__1 = tl[tl_dim1 + 1], Abs (d__1)), d__8 = (d__2 =
						       tr[tr_dim1 + 1],
						       Abs (d__2)), d__7 =
    Max (d__7, d__8), d__8 = (d__3 =
			      tr[(tr_dim1 << 1) + 1], Abs (d__3)), d__7 =
    Max (d__7, d__8), d__8 = (d__4 = tr[tr_dim1 + 2], Abs (d__4)), d__7 =
    Max (d__7, d__8), d__8 = (d__5 = tr[(tr_dim1 << 1) + 2], Abs (d__5));
  d__6 = eps * Max (d__7, d__8);
  smin = Max (d__6, smlnum);
  tmp[0] = sgn * tl[tl_dim1 + 1] * tr[tr_dim1 + 1] - 1.;
  tmp[3] = sgn * tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 2] - 1.;
  if (*ltranr)
    {
      tmp[1] = sgn * tl[tl_dim1 + 1] * tr[tr_dim1 + 2];
      tmp[2] = sgn * tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 1];
    }
  else
    {
      tmp[1] = sgn * tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 1];
      tmp[2] = sgn * tl[tl_dim1 + 1] * tr[tr_dim1 + 2];
    }
  btmp[0] = b[b_dim1 + 1];
  btmp[1] = b[(b_dim1 << 1) + 1];
  goto L40;
  /* 
   *    2 by 1: 
   *         ISGN*op[TL11 TL12]*[X11]*TR11  = [B11] 
   *                [TL21 TL22] [X21]         [B21] 
   * 
   */
 L30:
  /*Computing MAX 
   *Computing MAX 
   */
  d__7 = (d__1 = tr[tr_dim1 + 1], Abs (d__1)), d__8 = (d__2 =
						       tl[tl_dim1 + 1],
						       Abs (d__2)), d__7 =
    Max (d__7, d__8), d__8 = (d__3 =
			      tl[(tl_dim1 << 1) + 1], Abs (d__3)), d__7 =
    Max (d__7, d__8), d__8 = (d__4 = tl[tl_dim1 + 2], Abs (d__4)), d__7 =
    Max (d__7, d__8), d__8 = (d__5 = tl[(tl_dim1 << 1) + 2], Abs (d__5));
  d__6 = eps * Max (d__7, d__8);
  smin = Max (d__6, smlnum);
  tmp[0] = sgn * tl[tl_dim1 + 1] * tr[tr_dim1 + 1] - 1.;
  tmp[3] = sgn * tl[(tl_dim1 << 1) + 2] * tr[tr_dim1 + 1] - 1.;
  if (*ltranl)
    {
      tmp[1] = sgn * tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 1];
      tmp[2] = sgn * tl[tl_dim1 + 2] * tr[tr_dim1 + 1];
    }
  else
    {
      tmp[1] = sgn * tl[tl_dim1 + 2] * tr[tr_dim1 + 1];
      tmp[2] = sgn * tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 1];
    }
  btmp[0] = b[b_dim1 + 1];
  btmp[1] = b[b_dim1 + 2];
 L40:
  /* 
   *    Solve 2 by 2 system using complete pivoting. 
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
  *scale = 1.;
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
      *xnorm = (d__1 = x[x_dim1 + 1], Abs (d__1)) + (d__2 =
						     x[(x_dim1 << 1) + 1],
						     Abs (d__2));
    }
  else
    {
      x[x_dim1 + 2] = x2[1];
      /*Computing MAX 
       */
      d__3 = (d__1 = x[x_dim1 + 1], Abs (d__1)), d__4 = (d__2 =
							 x[x_dim1 + 2],
							 Abs (d__2));
      *xnorm = Max (d__3, d__4);
    }
  return 0;
  /* 
   *    2 by 2: 
   *    ISGN*op[TL11 TL12]*[X11 X12]*op[TR11 TR12]-[X11 X12] = [B11 B12] 
   *           [TL21 TL22] [X21 X22]   [TR21 TR22] [X21 X22]   [B21 B22] 
   * 
   *    Solve equivalent 4 by 4 system using complete pivoting. 
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
  d__5 = smin, d__6 = (d__1 = tl[tl_dim1 + 1], Abs (d__1)), d__5 =
    Max (d__5, d__6), d__6 = (d__2 =
			      tl[(tl_dim1 << 1) + 1], Abs (d__2)), d__5 =
    Max (d__5, d__6), d__6 = (d__3 = tl[tl_dim1 + 2], Abs (d__3)), d__5 =
    Max (d__5, d__6), d__6 = (d__4 = tl[(tl_dim1 << 1) + 2], Abs (d__4));
  smin = Max (d__5, d__6);
  /*Computing MAX 
   */
  d__1 = eps * smin;
  smin = Max (d__1, smlnum);
  btmp[0] = 0.;
  C2F (dcopy) (&c__16, btmp, &c__0, t16, &c__1);
  t16[0] = sgn * tl[tl_dim1 + 1] * tr[tr_dim1 + 1] - 1.;
  t16[5] = sgn * tl[(tl_dim1 << 1) + 2] * tr[tr_dim1 + 1] - 1.;
  t16[10] = sgn * tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 2] - 1.;
  t16[15] = sgn * tl[(tl_dim1 << 1) + 2] * tr[(tr_dim1 << 1) + 2] - 1.;
  if (*ltranl)
    {
      t16[4] = sgn * tl[tl_dim1 + 2] * tr[tr_dim1 + 1];
      t16[1] = sgn * tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 1];
      t16[14] = sgn * tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 2];
      t16[11] = sgn * tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 2];
    }
  else
    {
      t16[4] = sgn * tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 1];
      t16[1] = sgn * tl[tl_dim1 + 2] * tr[tr_dim1 + 1];
      t16[14] = sgn * tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 2];
      t16[11] = sgn * tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 2];
    }
  if (*ltranr)
    {
      t16[8] = sgn * tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 1];
      t16[13] = sgn * tl[(tl_dim1 << 1) + 2] * tr[(tr_dim1 << 1) + 1];
      t16[2] = sgn * tl[tl_dim1 + 1] * tr[tr_dim1 + 2];
      t16[7] = sgn * tl[(tl_dim1 << 1) + 2] * tr[tr_dim1 + 2];
    }
  else
    {
      t16[8] = sgn * tl[tl_dim1 + 1] * tr[tr_dim1 + 2];
      t16[13] = sgn * tl[(tl_dim1 << 1) + 2] * tr[tr_dim1 + 2];
      t16[2] = sgn * tl[tl_dim1 + 1] * tr[(tr_dim1 << 1) + 1];
      t16[7] = sgn * tl[(tl_dim1 << 1) + 2] * tr[(tr_dim1 << 1) + 1];
    }
  if (*ltranl && *ltranr)
    {
      t16[12] = sgn * tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 1];
      t16[9] = sgn * tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 1];
      t16[6] = sgn * tl[tl_dim1 + 2] * tr[tr_dim1 + 2];
      t16[3] = sgn * tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 2];
    }
  if (*ltranl && !(*ltranr))
    {
      t16[12] = sgn * tl[tl_dim1 + 2] * tr[tr_dim1 + 2];
      t16[9] = sgn * tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 2];
      t16[6] = sgn * tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 1];
      t16[3] = sgn * tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 1];
    }
  if (!(*ltranl) && *ltranr)
    {
      t16[12] = sgn * tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 1];
      t16[9] = sgn * tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 1];
      t16[6] = sgn * tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 2];
      t16[3] = sgn * tl[tl_dim1 + 2] * tr[tr_dim1 + 2];
    }
  if (!(*ltranl) && !(*ltranr))
    {
      t16[12] = sgn * tl[(tl_dim1 << 1) + 1] * tr[tr_dim1 + 2];
      t16[9] = sgn * tl[tl_dim1 + 2] * tr[tr_dim1 + 2];
      t16[6] = sgn * tl[(tl_dim1 << 1) + 1] * tr[(tr_dim1 << 1) + 1];
      t16[3] = sgn * tl[tl_dim1 + 2] * tr[(tr_dim1 << 1) + 1];
    }
  btmp[0] = b[b_dim1 + 1];
  btmp[1] = b[b_dim1 + 2];
  btmp[2] = b[(b_dim1 << 1) + 1];
  btmp[3] = b[(b_dim1 << 1) + 2];
  /* 
   *    Perform elimination 
   * 
   */
  for (i__ = 1; i__ <= 3; ++i__)
    {
      xmax = 0.;
      for (ip = i__; ip <= 4; ++ip)
	{
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
	  /* L70: */
	}
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
      for (j = i__ + 1; j <= 4; ++j)
	{
	  t16[j + (i__ << 2) - 5] /= t16[i__ + (i__ << 2) - 5];
	  btmp[j - 1] -= t16[j + (i__ << 2) - 5] * btmp[i__ - 1];
	  for (k = i__ + 1; k <= 4; ++k)
	    {
	      t16[j + (k << 2) - 5] -=
		t16[j + (i__ << 2) - 5] * t16[i__ + (k << 2) - 5];
	      /* L80: */
	    }
	  /* L90: */
	}
      /* L100: */
    }
  if (Abs (t16[15]) < smin)
    {
      t16[15] = smin;
    }
  *scale = 1.;
  if (smlnum * 8. * Abs (btmp[0]) > Abs (t16[0])
      || smlnum * 8. * Abs (btmp[1]) > Abs (t16[5])
      || smlnum * 8. * Abs (btmp[2]) > Abs (t16[10])
      || smlnum * 8. * Abs (btmp[3]) > Abs (t16[15]))
    {
      /*Computing MAX 
       */
      d__1 = Abs (btmp[0]), d__2 = Abs (btmp[1]), d__1 =
	Max (d__1, d__2), d__2 = Abs (btmp[2]), d__1 =
	Max (d__1, d__2), d__2 = Abs (btmp[3]);
      *scale = .125 / Max (d__1, d__2);
      btmp[0] *= *scale;
      btmp[1] *= *scale;
      btmp[2] *= *scale;
      btmp[3] *= *scale;
    }
  for (i__ = 1; i__ <= 4; ++i__)
    {
      k = 5 - i__;
      temp = 1. / t16[k + (k << 2) - 5];
      tmp[k - 1] = btmp[k - 1] * temp;
      for (j = k + 1; j <= 4; ++j)
	{
	  tmp[k - 1] -= temp * t16[k + (j << 2) - 5] * tmp[j - 1];
	  /* L110: */
	}
      /* L120: */
    }
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
  x[x_dim1 + 1] = tmp[0];
  x[x_dim1 + 2] = tmp[1];
  x[(x_dim1 << 1) + 1] = tmp[2];
  x[(x_dim1 << 1) + 2] = tmp[3];
  /*Computing MAX 
   */
  d__1 = Abs (tmp[0]) + Abs (tmp[2]), d__2 = Abs (tmp[1]) + Abs (tmp[3]);
  *xnorm = Max (d__1, d__2);
  return 0;
  /* 
   *    End of DLASD2 
   * 
   */
}				/* dlasd2_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * LYPCFR estimates the forward error bound for the computed solution of 
 * the matrix Lyapunov equation 
 * 
 *        transpose(op(A))*X + X*op(A) = scale*C 
 * 
 * where op(A) = A or A**T and C is symmetric (C = C**T). A is N-by-N, 
 * the right hand side C and the solution X are N-by-N, and scale is a 
 * scale factor, set <= 1 during the solution of the equation to avoid 
 * overflow in X. If the equation is not scaled, scale should be set 
 * equal to 1. 
 * 
 * Arguments 
 * ========= 
 * 
 * TRANA   (input) CHARACTER*1 
 *         Specifies the option op(A): 
 *         = 'N': op(A) = A    (No transpose) 
 *         = 'T': op(A) = A**T (Transpose) 
 *         = 'C': op(A) = A**T (Conjugate transpose = Transpose) 
 * 
 * N       (input) INT 
 *         The order of the matrix A, and the order of the 
 *         matrices C and X. N >= 0. 
 * 
 * A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
 *         The N-by-N matrix A. 
 * 
 * LDA     (input) INT 
 *         The leading dimension of the array A. LDA >= Max(1,N). 
 * 
 * UPLO    (input) CHARACTER*1 
 *         = 'U':  Upper triangle of C is stored; 
 *         = 'L':  Lower triangle of C is stored. 
 * 
 * C       (input) DOUBLE PRECISION array, dimension (LDC,N) 
 *         If UPLO = 'U', the leading N-by-N upper triangular part of C 
 *         contains the upper triangular part of the matrix C. 
 *         If UPLO = 'L', the leading N-by-N lower triangular part of C 
 *         contains the lower triangular part of the matrix C. 
 * 
 * LDC     (input) INT 
 *         The leading dimension of the array C. LDC >= Max(1,N) 
 * 
 * T       (input) DOUBLE PRECISION array, dimension (LDT,N) 
 *         The upper quasi-triangular matrix in Schur canonical 
 *         form from the Schur factorization of A. 
 * 
 * LDT     (input) INT 
 *         The leading dimension of the array T. LDT >= Max(1,N) 
 * 
 * U       (input) DOUBLE PRECISION array, dimension (LDU,N) 
 *         The orthogonal matrix U from the real Schur 
 *         factorization of A. 
 * 
 * LDU     (input) INT 
 *         The leading dimension of the array U. LDU >= Max(1,N) 
 * 
 * X       (input) DOUBLE PRECISION array, dimension (LDX,N) 
 *         The N-by-N solution matrix X. 
 * 
 * LDX     (input) INT 
 *         The leading dimension of the array X. LDX >= Max(1,N) 
 * 
 * SCALE   (input) DOUBLE PRECISION 
 *         The scale factor, scale. 
 * 
 * FERR    (output) DOUBLE PRECISION 
 *         On exit, an estimated forward error bound for the solution X. 
 *         If XTRUE is the true solution, FERR bounds the magnitude 
 *         of the largest entry in (X - XTRUE) divided by the magnitude 
 *         of the largest entry in X. 
 * 
 * WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK) 
 * 
 * LWORK   (input) INT 
 *         The dimension of the array WORK. 
 *         LWORK >= 6*N*N. 
 * 
 * IWORK   (workspace) INT array, dimension (N*N) 
 * 
 * INFO    (output) INT 
 *         = 0: successful exit 
 *         < 0: if INFO = -i, the i-th argument had an illegal value 
 * 
 * Further Details 
 * =============== 
 * 
 * The forward error bound is estimated using the practical error bound 
 * proposed in [1]. 
 * 
 * References 
 * ========== 
 * 
 * [1] N.J. Higham, Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * 
 */

int
nsp_ctrlpack_lypcfr (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *t, int *ldt, double *u,
		     int *ldu, double *x, int *ldx, double *scale,
		     double *ferr, double *work, int *lwork, int *iwork,
		     int *info, long int trana_len, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, t_dim1, t_offset, u_dim1, u_offset,
    x_dim1, x_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int idlc, iabs, kase, ires, ixbs, itmp, info2, i__, j;
  int lower;
  double xnorm, scale2;
  int ij;
  char tranat[1];
  int notrna;
  int minwrk;
  double eps, est;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --work;
  --iwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldt < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -11;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -13;
    }
  /* 
   *    Get the machine precision 
   * 
   */
  eps = C2F (dlamch) ("Epsilon", 7L);
  /* 
   *    Compute workspace 
   * 
   */
  minwrk = *n * 6 * *n;
  if (*lwork < minwrk)
    {
      *info = -17;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("LYPCFR", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  xnorm = C2F (dlansy) ("1", uplo, n, &x[x_offset], ldx, &work[1], 1L, 1L);
  if (xnorm == 0.)
    {
      /* 
       *       Matrix all zero 
       * 
       */
      *ferr = 0.;
      return 0;
    }
  /* 
   *    Workspace usage 
   * 
   */
  idlc = *n * *n;
  itmp = idlc + *n * *n;
  iabs = itmp + *n * *n;
  ixbs = iabs + *n * *n;
  ires = ixbs + *n * *n;
  /* 
   */
  if (notrna)
    {
      *(unsigned char *) tranat = 'T';
    }
  else
    {
      *(unsigned char *) tranat = 'N';
    }
  /* 
   *    Form residual matrix R = C - op(A')*X - X*op(A) 
   * 
   */
  C2F (dlacpy) (uplo, n, n, &c__[c_offset], ldc, &work[ires + 1], n, 1L);
  C2F (dsyr2k) (uplo, tranat, n, n, &c_b79, &a[a_offset], lda,
		&x[x_offset], ldx, scale, &work[ires + 1], n, 1L, 1L);
  /* 
   *    Add to Abs(R) a term that takes account of rounding errors in 
   *    forming R: 
   *      Abs(R) := Abs(R) + EPS*(3*abs(C) + (n+3)*(Abs(op(A'))*abs(X) + 
   *                       Abs(X)*abs(op(A)))) 
   *    where EPS is the machine precision 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  work[iabs + i__ + (j - 1) * *n] = (d__1 =
					     a[i__ + j * a_dim1], Abs (d__1));
	  work[ixbs + i__ + (j - 1) * *n] = (d__1 =
					     x[i__ + j * x_dim1], Abs (d__1));
	  /* L10: */
	}
      /* L20: */
    }
  C2F (dsyr2k) (uplo, tranat, n, n, &c_b82, &work[iabs + 1], n,
		&work[ixbs + 1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
  if (lower)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = j; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] = (d__1 =
						 work[ires + i__ +
						      (j - 1) * *n],
						 Abs (d__1)) +
		eps * 3. * *scale * (d__2 =
				     c__[i__ + j * c_dim1],
				     Abs (d__2)) + (double) (*n +
							     3) * eps *
		work[itmp + i__ + (j - 1) * *n];
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
	  i__2 = j;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] = (d__1 =
						 work[ires + i__ +
						      (j - 1) * *n],
						 Abs (d__1)) +
		eps * 3. * *scale * (d__2 =
				     c__[i__ + j * c_dim1],
				     Abs (d__2)) + (double) (*n +
							     3) * eps *
		work[itmp + i__ + (j - 1) * *n];
	      /* L50: */
	    }
	  /* L60: */
	}
    }
  /* 
   *    Compute forward error bound, using matrix norm estimator 
   * 
   */
  est = 0.;
  kase = 0;
 L70:
  i__1 = *n * (*n + 1) / 2;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] =
			work[ij] * work[ires + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Unpack the lower triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] = work[ij];
		    }
		  /* L80: */
		}
	      /* L90: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] =
			work[ij] * work[ires + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Unpack the upper triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] = work[ij];
		    }
		  /* L100: */
		}
	      /* L110: */
	    }
	}
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      if (kase == 2)
	{
	  /* 
	   *          Solve op(A')*Y + Y*op(A) = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (trana, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale2, &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(A)*Z + Z*op(A') = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (tranat, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale2, &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n,
		   &u[u_offset], ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Pack the lower triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[ij] = work[itmp + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[ij] =
			work[itmp + i__ + (j - 1) * *n] * work[ires + i__ +
							       (j - 1) * *n];
		    }
		  /* L120: */
		}
	      /* L130: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Pack the upper triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[ij] = work[itmp + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[ij] =
			work[itmp + i__ + (j - 1) * *n] * work[ires + i__ +
							       (j - 1) * *n];
		    }
		  /* L140: */
		}
	      /* L150: */
	    }
	}
      goto L70;
    }
  /* 
   *    Compute the estimate of the forward error 
   * 
   */
  *ferr =
    est * 2. / C2F (dlansy) ("Max", uplo, n, &x[x_offset], ldx,
			     &work[1], 3L, 1L) / scale2;
  if (*ferr > 1.)
    {
      *ferr = 1.;
    }
  /* 
   */
  return 0;
  /* 
   *    End of LYPCFR 
   * 
   */
}				/* lypcfr_ */


/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * LYPCRC estimates the reciprocal of the condition number of the matrix 
 * Lyapunov equation 
 * 
 *        transpose(op(A))*X + X*op(A) = scale*C 
 * 
 * where op(A) = A or A**T and C is symmetric (C = C**T). A is N-by-N, 
 * the right hand side C and the solution X are N-by-N, and scale is a 
 * scale factor, set <= 1 during the solution of the equation to avoid 
 * overflow in X. If the equation is not scaled, scale should be set 
 * equal to 1. 
 * 
 * Arguments 
 * ========= 
 * 
 * FACT    (input) CHARACTER*1 
 *         Specifies whether or not the real Schur factorization 
 *         of the matrix A is supplied on entry: 
 *         = 'F':   On entry, T and U contain the factors from the 
 *                    real Schur factorization of the matrix A. 
 *         = 'N':   The Schur factorization of A will be computed 
 *                    and the factors will be stored in T and U. 
 * 
 * TRANA   (input) CHARACTER*1 
 *         Specifies the option op(A): 
 *         = 'N': op(A) = A    (No transpose) 
 *         = 'T': op(A) = A**T (Transpose) 
 *         = 'C': op(A) = A**T (Conjugate transpose = Transpose) 
 * 
 * N       (input) INT 
 *         The order of the matrix A, and the order of the 
 *         matrices C and X. N >= 0. 
 * 
 * A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
 *         The N-by-N matrix A. 
 * 
 * LDA     (input) INT 
 *         The leading dimension of the array A. LDA >= Max(1,N). 
 * 
 * UPLO    (input) CHARACTER*1 
 *         = 'U':  Upper triangle of C is stored; 
 *         = 'L':  Lower triangle of C is stored. 
 * 
 * C       (input) DOUBLE PRECISION array, dimension (LDC,N) 
 *         If UPLO = 'U', the leading N-by-N upper triangular part of C 
 *         contains the upper triangular part of the matrix C. 
 *         If UPLO = 'L', the leading N-by-N lower triangular part of C 
 *         contains the lower triangular part of the matrix C. 
 * 
 * LDC     (input) INT 
 *         The leading dimension of the array C. LDC >= Max(1,N) 
 * 
 * T       (input or output) DOUBLE PRECISION array, dimension (LDT,N) 
 *         If FACT = 'F', then T is an input argument and on entry 
 *         contains the upper quasi-triangular matrix in Schur canonical 
 *         form from the Schur factorization of A. 
 *         If FACT = 'N', then T is an output argument and on exit 
 *         returns the upper quasi-triangular matrix in Schur 
 *         canonical form from the Schur factorization of A. 
 * 
 * LDT     (input) INT 
 *         The leading dimension of the array T. LDT >= Max(1,N) 
 * 
 * U       (input or output) DOUBLE PRECISION array, dimension (LDU,N) 
 *         If FACT = 'F', then U is an input argument and on entry 
 *         contains the orthogonal matrix U from the real Schur 
 *         factorization of A. 
 *         If FACT = 'N', then U is an output argument and on exit 
 *         returns the orthogonal N-by-N matrix from the real Schur 
 *         factorization of A. 
 * 
 * LDU     (input) INT 
 *         The leading dimension of the array U. LDU >= Max(1,N) 
 * 
 * X       (input) DOUBLE PRECISION array, dimension (LDX,N) 
 *         The N-by-N solution matrix X. 
 * 
 * LDX     (input) INT 
 *         The leading dimension of the array X. LDX >= Max(1,N) 
 * 
 * SCALE   (input) DOUBLE PRECISION 
 *         The scale factor, scale. 
 * 
 * RCOND   (output) DOUBLE PRECISION 
 *         On exit, an estimate of the reciprocal condition number 
 *         of the Lyapunov equation. 
 * 
 * WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK) 
 *         On exit, if INFO = 0, WORK(1) contains the optimal LWORK. 
 * 
 * LWORK   (input) INT 
 *         The dimension of the array WORK. 
 *         LWORK >= 3*N*N + 2*N + Max(1,3*N). 
 *         For good performance, LWORK must generally be larger. 
 * 
 * IWORK   (workspace) INT array, dimension (N*N) 
 * 
 * INFO    (output) INT 
 *         = 0: successful exit 
 *         < 0: if INFO = -i, the i-th argument had an illegal value 
 *         = 1: the matrix A cannot be reduced to Schur canonical form 
 * 
 * Further Details 
 * =============== 
 * 
 * The condition number of the Lyapunov equation is estimated as 
 * 
 * cond = (norm(Theta)*norm(A) + norm(inv(Omega))*norm(C))/norm(X) 
 * 
 * where Omega and Theta are linear operators defined by 
 * 
 * Omega(Z) = transpose(op(A))*Z + Z*op(A), 
 * Theta(Z) = inv(Omega(transpose(op(Z))*X + X*op(Z))). 
 * 
 * The program estimates the quantities 
 * 
 * sep(op(A),-transpose(op(A)) = 1 / norm(inv(Omega)) 
 * 
 * and norm(Theta) using 1-norm condition estimator. 
 * 
 * References 
 * ========== 
 * 
 * [1] N.J. Higham, Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * 
 */

int
nsp_ctrlpack_lypcrc (char *fact, char *trana, int *n, double *a, int *lda,
		     char *uplo, double *c__, int *ldc, double *t, int *ldt,
		     double *u, int *ldu, double *x, int *ldx, double *scale,
		     double *rcond, double *work, int *lwork, int *iwork,
		     int *info, long int fact_len, long int trana_len,
		     long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, t_dim1, t_offset, u_dim1, u_offset,
    x_dim1, x_offset, i__1, i__2;

  /* Local variables */
  int idlc, kase, sdim, itmp, iwrk, info2, i__, j;
  double anorm, cnorm;
  int bwork[1], lower;
  double xnorm, scale2;
  int ij;
  int nofact;
  char tranat[1];
  int notrna;
  int minwrk;
  double thnorm;
  int lwa, iwi;
  double sep, est;
  int iwr;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --work;
  --iwork;

  /* Function Body */
  nofact = C2F (lsame) (fact, "N", 1L, 1L);
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!nofact && !C2F (lsame) (fact, "F", 1L, 1L))
    {
      *info = -1;
    }
  else if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
	   && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -2;
    }
  else if (*n < 0)
    {
      *info = -3;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -5;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -6;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -8;
    }
  else if (*ldt < Max (1, *n))
    {
      *info = -10;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -12;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -14;
    }
  /* 
   *    Compute workspace 
   * 
   *Computing MAX 
   */
  i__1 = 1, i__2 = *n * 3;
  minwrk = *n * 3 * *n + (*n << 1) + Max (i__1, i__2);
  if (*lwork < minwrk)
    {
      *info = -18;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("LYPCRC", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Compute the norms of the matrices A, C and X 
   * 
   */
  anorm = C2F (dlange) ("1", n, n, &a[a_offset], lda, &work[1], 1L);
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  xnorm = C2F (dlansy) ("1", uplo, n, &x[x_offset], ldx, &work[1], 1L, 1L);
  if (xnorm == 0.)
    {
      /* 
       *       Matrix all zero 
       * 
       */
      *rcond = 0.;
      return 0;
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa = *n * 3 * *n + (*n << 1);
  idlc = *n * *n;
  itmp = idlc + *n * *n;
  iwr = itmp + *n * *n;
  iwi = iwr + *n;
  iwrk = iwi + *n;
  /* 
   */
  if (nofact)
    {
      /* 
       *       Compute the Schur factorization of A 
       * 
       */
      C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &t[t_offset], ldt, 4L);
      i__1 = *lwork - iwrk;
      C2F (dgees) ("V", "N", nsp_ctrlpack_voiddummy, n, &t[t_offset], ldt,
		   &sdim, &work[iwr + 1], &work[iwi + 1], &u[u_offset],
		   ldu, &work[iwrk + 1], &i__1, bwork, &info2, 1L, 1L);
      if (info2 > 0)
	{
	  *info = 1;
	  return 0;
	}
      lwa += (int) work[iwrk + 1];
    }
  /* 
   *    Estimate sep(op(A),-transpose(op(A))) 
   * 
   */
  if (notrna)
    {
      *(unsigned char *) tranat = 'T';
    }
  else
    {
      *(unsigned char *) tranat = 'N';
    }
  /* 
   */
  est = 0.;
  kase = 0;
 L10:
  i__1 = *n * (*n + 1) / 2;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      /* 
       *       Unpack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L20: */
		}
	      /* L30: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L40: */
		}
	      /* L50: */
	    }
	}
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      if (kase == 1)
	{
	  /* 
	   *          Solve op(A')*Y + Y*op(A) = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (trana, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale2, &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(A)*Z + Z*op(A') = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (tranat, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale2, &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n,
		   &u[u_offset], ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      /* 
       *       Pack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L60: */
		}
	      /* L70: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L80: */
		}
	      /* L90: */
	    }
	}
      goto L10;
    }
  /* 
   */
  sep = scale2 / 2. / est;
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (sep == 0.)
    {
      *rcond = 0.;
      return 0;
    }
  /* 
   *    Estimate norm(Theta) 
   * 
   */
  est = 0.;
  kase = 0;
 L100:
  i__1 = *n * *n;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      /* 
       *       Compute RHS = op(W')*X + X*op(W) 
       * 
       */
      C2F (dsyr2k) (uplo, tranat, n, n, &c_b82, &work[1], n,
		    &x[x_offset], ldx, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dlacpy) (uplo, n, n, &work[itmp + 1], n, &work[1], n, 1L);
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[1], n, &u[u_offset],
		   ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[itmp + 1], n, &c_b83, &work[1], n, 1L, 1L);
      if (kase == 1)
	{
	  /* 
	   *          Solve op(A')*Y + Y*op(A) = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (trana, n, &t[t_offset], ldt, &work[1], n,
			       &scale2, &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(A)*Z + Z*op(A') = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (tranat, n, &t[t_offset], ldt, &work[1], n,
			       &scale2, &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[1], n, &u[u_offset],
		   ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      goto L100;
    }
  /* 
   */
  thnorm = est / scale2;
  /* 
   *    Estimate the reciprocal condition number 
   * 
   */
  *rcond = sep * xnorm / (cnorm * *scale + sep * (thnorm * anorm));
  if (*rcond > 1.)
    {
      *rcond = 1.;
    }
  /* 
   */
  work[1] = (double) lwa;
  return 0;
  /* 
   *    End of LYPCRC 
   * 
   */
}				/* lypcrc_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * LYPCSL solves the matrix Lyapunov equation 
 *         On exit, if INFO = 0, WORK(1) contains the optimal LWORK. 
 * 
 * LWORK   (input) INT 
 *         The dimension of the array WORK. 
 *         LWORK >= 6*N*N + Max(1,3*N). 
 *         For good performance, LWORK must generally be larger. 
 * 
 * IWORK   (workspace) INT array, dimension (N*N) 
 * 
 * INFO    (output) INT 
 *         = 0: successful exit 
 *         < 0: if INFO = -i, the i-th argument had an illegal value 
 *         = 1: the matrix A cannot be reduced to Schur canonical form 
 *         = 2: A and -transpose(A) have common or very close 
 *              eigenvalues; perturbed values were used to solve the 
 *              equation (but the matrix A is unchanged). 
 * 
 * Further Details 
 * =============== 
 * 
 * The matrix Lyapunov equation is solved by the Bartels-Stewart 
 * algorithm [1]. 
 * 
 * The condition number of the equation is estimated using 1-norm 
 * condition estimator. 
 * 
 * The forward error bound is estimated using the practical error bound 
 * proposed in [2]. 
 * 
 * References 
 * ========== 
 * 
 * [1] R.H. Bartels and G.W. Stewart. Algorithm 432: Solution of the 
 *     matrix equation AX + XB = C. Comm. ACM, vol. 15, pp. 820-826, 
 *     1972. 
 * [2] N.J. Higham, Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, 1993, pp. 124-136. 
 * 
 */


int
nsp_ctrlpack_lypcsl (char *fact, char *trana, int *n, double *a, int *lda,
		     char *uplo, double *c__, int *ldc, double *t, int *ldt,
		     double *u, int *ldu, double *wr, double *wi, double *x,
		     int *ldx, double *scale, double *rcond, double *ferr,
		     double *work, int *lwork, int *iwork, int *info,
		     long int fact_len, long int trana_len, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, t_dim1, t_offset, u_dim1, u_offset,
    x_dim1, x_offset, i__1, i__2;

  /* Local variables */
  int sdim, info2;
  double cnorm;
  int bwork[1], lower;
  int nofact;
  int lwamax;
  int notrna;
  int minwrk;
  int lwa;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  --wr;
  --wi;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --work;
  --iwork;

  /* Function Body */
  nofact = C2F (lsame) (fact, "N", 1L, 1L);
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!nofact && !C2F (lsame) (fact, "F", 1L, 1L))
    {
      *info = -1;
    }
  else if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
	   && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -2;
    }
  else if (*n < 0)
    {
      *info = -3;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -5;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -6;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -8;
    }
  else if (*ldt < Max (1, *n))
    {
      *info = -10;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -12;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -16;
    }
  /* 
   *    Compute workspace 
   * 
   *Computing MAX 
   */
  i__1 = 1, i__2 = *n * 3;
  minwrk = *n * 6 * *n + Max (i__1, i__2);
  if (*lwork < minwrk)
    {
      *info = -21;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("LYPCSL", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  if (cnorm == 0.)
    {
      /* 
       *       Matrix all zero. Return zero solution 
       * 
       */
      C2F (dlaset) ("F", n, n, &c_b83, &c_b83, &x[x_offset], ldx, 1L);
      *scale = 1.;
      *rcond = 0.;
      *ferr = 0.;
      return 0;
    }
  /* 
   */
  lwa = 0;
  /* 
   */
  if (nofact)
    {
      /* 
       *       Compute the Schur factorization of A 
       * 
       */
      C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &t[t_offset], ldt, 4L);
      C2F (dgees) ("V", "N", nsp_ctrlpack_voiddummy, n, &t[t_offset], ldt,
		   &sdim, &wr[1], &wi[1], &u[u_offset], ldu, &work[1],
		   lwork, bwork, &info2, 1L, 1L);
      if (info2 > 0)
	{
	  *info = 1;
	  return 0;
	}
      lwa = (int) work[1];
    }
  lwamax = lwa;
  /* 
   *    Transform the right-hand side: C := U'*C*U. 
   *    Form TEMP = C*U then X = U'*TEMP 
   * 
   */
  C2F (dsymm) ("L", uplo, n, n, &c_b82, &c__[c_offset], ldc,
	       &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
  C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu, &work[1],
	       n, &c_b83, &x[x_offset], ldx, 1L, 1L);
  /* 
   *    Solve the quasi-triangular Lyapunov equation. 
   *    The answer overwrites the right-hand side 
   * 
   */
  nsp_ctrlpack_lypctr (trana, n, &t[t_offset], ldt, &x[x_offset], ldx, scale,
		       &info2, 1L);
  if (info2 > 0)
    {
      *info = 2;
    }
  /* 
   *    Transform back to obtain the solution: X := U*X*U'. 
   *    Form TEMP = U*X then X = TEMP*U' 
   * 
   */
  C2F (dsymm) ("R", uplo, n, n, &c_b82, &x[x_offset], ldx,
	       &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
  C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n, &u[u_offset],
	       ldu, &c_b83, &x[x_offset], ldx, 1L, 1L);
  /* 
   *    Estimate the reciprocal of the condition number 
   * 
   */
  nsp_ctrlpack_lypcrc ("F", trana, n, &a[a_offset], lda, uplo, &c__[c_offset],
		       ldc, &t[t_offset], ldt, &u[u_offset], ldu,
		       &x[x_offset], ldx, scale, rcond, &work[1], lwork,
		       &iwork[1], &info2, 1L, 1L, 1L);
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (*rcond == 0.)
    {
      *ferr = 1.;
      return 0;
    }
  lwa = (int) work[1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Estimate the bound on the forward error 
   * 
   */
  nsp_ctrlpack_lypcfr (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &t[t_offset], ldt, &u[u_offset], ldu, &x[x_offset],
		       ldx, scale, ferr, &work[1], lwork, &iwork[1], &info2,
		       1L, 1L);
  lwa = *n * 6 * *n;
  lwamax = Max (lwa, lwamax);
  /* 
   */
  work[1] = (double) lwamax;
  return 0;
  /* 
   *    End of LYPCSL 
   * 
   */
}				/* lypcsl_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * LYPCTR solves the matrix Lyapunov equation 
 * 
 *        transpose(op(A))*X + X*op(A) = scale*C 
 * 
 * where op(A) = A or A**T,  A is upper quasi-triangular and C is 
 * symmetric (C = C**T). A is N-by-N, the right hand side C and the 
 * solution X are N-by-N, and scale is an output scale factor, 
 * set <= 1 to avoid overflow in X. 
 * 
 * A must be in Schur canonical form (as returned by DHSEQR), that is, 
 * block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; 
 * each 2-by-2 diagonal block has its diagonal elements equal and its 
 * off-diagonal elements of opposite sign. 
 * 
 * Arguments 
 * ========= 
 * 
 * TRANA   (input) CHARACTER*1 
 *         Specifies the option op(A): 
 *         = 'N': op(A) = A    (No transpose) 
 *         = 'T': op(A) = A**T (Transpose) 
 *         = 'C': op(A) = A**H (Conjugate transpose = Transpose) 
 * 
 * N       (input) INT 
 *         The order of the matrix A, and the order of the 
 *         matrices C and X. N >= 0. 
 * 
 * A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
 *         The upper quasi-triangular matrix A, in Schur canonical form. 
 * 
 * LDA     (input) INT 
 *         The leading dimension of the array A. LDA >= Max(1,N). 
 * 
 * C       (input/output) DOUBLE PRECISION array, dimension (LDC,N) 
 *         On entry, the symmetric N-by-N right hand side matrix C. 
 *         On exit, C is overwritten by the solution matrix X. 
 * 
 * LDC     (input) INT 
 *         The leading dimension of the array C. LDC >= Max(1,N) 
 * 
 * SCALE   (output) DOUBLE PRECISION 
 *         The scale factor, scale, set <= 1 to avoid overflow in X. 
 * 
 * INFO    (output) INT 
 *         = 0: successful exit 
 *         < 0: if INFO = -i, the i-th argument had an illegal value 
 *         = 1: A and -A have common or very close eigenvalues; 
 *               perturbed values were used to solve the equation 
 *               (but the matrix A is unchanged). 
 * 
 */

int
nsp_ctrlpack_lypctr (char *trana, int *n, double *a, int *lda, double *c__,
		     int *ldc, double *scale, int *info, long int trana_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Local variables */
  int ierr;
  double smin, suml, sumr;
  int j, k, l;
  double x[4] /* was [2][2] */ ;
  int knext, lnext, k1, k2, l1, l2;
  double xnorm;
  double a11, db;
  double scaloc;
  double bignum;
  int notrna;
  double smlnum, da11, vec[4] /* was [2][2] */ , dum[1], eps;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -6;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("LYPCTR", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Set constants to control overflow 
   * 
   */
  eps = C2F (dlamch) ("P", 1L);
  smlnum = C2F (dlamch) ("S", 1L);
  bignum = 1. / smlnum;
  C2F (dlabad) (&smlnum, &bignum);
  smlnum = smlnum * (double) (*n * *n) / eps;
  bignum = 1. / smlnum;
  /* 
   *Computing MAX 
   */
  d__1 = smlnum, d__2 =
    eps * C2F (dlange) ("M", n, n, &a[a_offset], lda, dum, 1L);
  smin = Max (d__1, d__2);
  /* 
   */
  *scale = 1.;
  /* 
   */
  if (notrna)
    {
      /* 
       *       Solve    A'*X + X*A = scale*C. 
       * 
       *       The (K,L)th block of X is determined starting from 
       *       upper-left corner column by column by 
       * 
       *         A(K,K)'*X(K,L) + X(K,L)*A(L,L) = C(K,L) - R(K,L) 
       * 
       *       Where 
       *                  K-1                   L-1 
       *         R(K,L) = SUM [A(I,K)'*X(I,L)] +SUM [X(K,J)*A(J,L)] 
       *                  I=1                   J=1 
       * 
       *       Start column loop (index = L) 
       *       L1 (L2): column index of the first (last) row of X(K,L) 
       * 
       */
      lnext = 1;
      i__1 = *n;
      for (l = 1; l <= i__1; ++l)
	{
	  if (l < lnext)
	    {
	      goto L60;
	    }
	  if (l == *n)
	    {
	      l1 = l;
	      l2 = l;
	    }
	  else
	    {
	      if (a[l + 1 + l * a_dim1] != 0.)
		{
		  l1 = l;
		  l2 = l + 1;
		  lnext = l + 2;
		}
	      else
		{
		  l1 = l;
		  l2 = l;
		  lnext = l + 1;
		}
	    }
	  /* 
	   *          Start row loop (index = K) 
	   *          K1 (K2): row index of the first (last) row of X(K,L) 
	   * 
	   */
	  knext = l;
	  i__2 = *n;
	  for (k = l; k <= i__2; ++k)
	    {
	      if (k < knext)
		{
		  goto L50;
		}
	      if (k == *n)
		{
		  k1 = k;
		  k2 = k;
		}
	      else
		{
		  if (a[k + 1 + k * a_dim1] != 0.)
		    {
		      k1 = k;
		      k2 = k + 1;
		      knext = k + 2;
		    }
		  else
		    {
		      k1 = k;
		      k2 = k;
		      knext = k + 1;
		    }
		}
	      /* 
	       */
	      if (l1 == l2 && k1 == k2)
		{
		  i__3 = k1 - 1;
		  suml =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  sumr =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  scaloc = 1.;
		  /* 
		   */
		  a11 = a[k1 + k1 * a_dim1] + a[l1 + l1 * a_dim1];
		  da11 = Abs (a11);
		  if (da11 <= smin)
		    {
		      a11 = smin;
		      da11 = smin;
		      *info = 1;
		    }
		  db = Abs (vec[0]);
		  if (da11 < 1. && db > 1.)
		    {
		      if (db > bignum * da11)
			{
			  scaloc = 1. / db;
			}
		    }
		  x[0] = vec[0] * scaloc / a11;
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L10: */
			}
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		    }
		  /* 
		   */
		}
	      else if (l1 == l2 && k1 != k2)
		{
		  /* 
		   */
		  i__3 = k1 - 1;
		  suml =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  sumr =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__3 = k1 - 1;
		  suml =
		    C2F (ddot) (&i__3, &a[k2 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  sumr =
		    C2F (ddot) (&i__3, &c__[k2 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  vec[1] = c__[k2 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  d__1 = -a[l1 + l1 * a_dim1];
		  C2F (dlaln2) (&c_true, &c__2, &c__1, &smin, &c_b82,
				&a[k1 + k1 * a_dim1], lda, &c_b82,
				&c_b82, vec, &c__2, &d__1, &c_b83, x,
				&c__2, &scaloc, &xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L20: */
			}
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l1 + k2 * c_dim1] = x[1];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 == k2)
		{
		  /* 
		   */
		  i__3 = k1 - 1;
		  suml =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  sumr =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__3 = k1 - 1;
		  suml =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l2 * c_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  sumr =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l2 * a_dim1 + 1], &c__1);
		  vec[1] = c__[k1 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  d__1 = -a[k1 + k1 * a_dim1];
		  C2F (dlaln2) (&c_true, &c__2, &c__1, &smin, &c_b82,
				&a[l1 + l1 * a_dim1], lda, &c_b82,
				&c_b82, vec, &c__2, &d__1, &c_b83, x,
				&c__2, &scaloc, &xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L30: */
			}
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[1];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l2 + k1 * c_dim1] = x[1];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 != k2)
		{
		  /* 
		   */
		  i__3 = k1 - 1;
		  suml =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  sumr =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__3 = k1 - 1;
		  suml =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l2 * c_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  sumr =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l2 * a_dim1 + 1], &c__1);
		  vec[2] = c__[k1 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__3 = k1 - 1;
		  suml =
		    C2F (ddot) (&i__3, &a[k2 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  sumr =
		    C2F (ddot) (&i__3, &c__[k2 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  vec[1] = c__[k2 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__3 = k1 - 1;
		  suml =
		    C2F (ddot) (&i__3, &a[k2 * a_dim1 + 1], &c__1,
				&c__[l2 * c_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  sumr =
		    C2F (ddot) (&i__3, &c__[k2 + c_dim1], ldc,
				&a[l2 * a_dim1 + 1], &c__1);
		  vec[3] = c__[k2 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  if (k1 == l1)
		    {
		      nsp_ctrlpack_dlaly2 (&c_false, &a[k1 + k1 * a_dim1],
					   lda, vec, &c__2, &scaloc, x, &c__2,
					   &xnorm, &ierr);
		    }
		  else
		    {
		      C2F (dlasy2) (&c_true, &c_false, &c__1, &c__2,
				    &c__2, &a[k1 + k1 * a_dim1], lda,
				    &a[l1 + l1 * a_dim1], lda, vec,
				    &c__2, &scaloc, x, &c__2, &xnorm, &ierr);
		    }
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L40: */
			}
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[2];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[k2 + l2 * c_dim1] = x[3];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		      c__[l2 + k1 * c_dim1] = x[2];
		      c__[l1 + k2 * c_dim1] = x[1];
		      c__[l2 + k2 * c_dim1] = x[3];
		    }
		}
	      /* 
	       */
	    L50:
	      ;
	    }
	L60:
	  ;
	}
      /* 
       */
    }
  else
    {
      /* 
       *       Solve    A*X + X*A' = scale*C. 
       * 
       *       The (K,L)th block of X is determined starting from 
       *       bottom-right corner column by column by 
       * 
       *           A(K,K)*X(K,L) + X(K,L)*A(L,L)' = C(K,L) - R(K,L) 
       * 
       *       Where 
       *                     N                     N 
       *           R(K,L) = SUM [A(K,I)*X(I,L)] + SUM [X(K,J)*A(L,J)']. 
       *                   I=K+1                 J=L+1 
       * 
       *       Start column loop (index = L) 
       *       L1 (L2): column index of the first (last) row of X(K,L) 
       * 
       */
      lnext = *n;
      for (l = *n; l >= 1; --l)
	{
	  if (l > lnext)
	    {
	      goto L120;
	    }
	  if (l == 1)
	    {
	      l1 = l;
	      l2 = l;
	    }
	  else
	    {
	      if (a[l + (l - 1) * a_dim1] != 0.)
		{
		  l1 = l - 1;
		  l2 = l;
		  lnext = l - 2;
		}
	      else
		{
		  l1 = l;
		  l2 = l;
		  lnext = l - 1;
		}
	    }
	  /* 
	   *          Start row loop (index = K) 
	   *          K1 (K2): row index of the first (last) row of X(K,L) 
	   * 
	   */
	  knext = l;
	  for (k = l; k >= 1; --k)
	    {
	      if (k > knext)
		{
		  goto L110;
		}
	      if (k == 1)
		{
		  k1 = k;
		  k2 = k;
		}
	      else
		{
		  if (a[k + (k - 1) * a_dim1] != 0.)
		    {
		      k1 = k - 1;
		      k2 = k;
		      knext = k - 2;
		    }
		  else
		    {
		      k1 = k;
		      k2 = k;
		      knext = k - 1;
		    }
		}
	      /* 
	       */
	      if (l1 == l2 && k1 == k2)
		{
		  i__1 = *n - k1;
		  /*Computing MIN 
		   */
		  i__2 = k1 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k1 + 1;
		  suml =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  i__1 = *n - l1;
		  /*Computing MIN 
		   */
		  i__2 = l1 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l1 + 1;
		  sumr =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  scaloc = 1.;
		  /* 
		   */
		  a11 = a[k1 + k1 * a_dim1] + a[l1 + l1 * a_dim1];
		  da11 = Abs (a11);
		  if (da11 <= smin)
		    {
		      a11 = smin;
		      da11 = smin;
		      *info = 1;
		    }
		  db = Abs (vec[0]);
		  if (da11 < 1. && db > 1.)
		    {
		      if (db > bignum * da11)
			{
			  scaloc = 1. / db;
			}
		    }
		  x[0] = vec[0] * scaloc / a11;
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L70: */
			}
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		    }
		  /* 
		   */
		}
	      else if (l1 == l2 && k1 != k2)
		{
		  /* 
		   */
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  suml =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  sumr =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  suml =
		    C2F (ddot) (&i__1,
				&a[k2 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  sumr =
		    C2F (ddot) (&i__1,
				&c__[k2 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  vec[1] = c__[k2 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  d__1 = -a[l1 + l1 * a_dim1];
		  C2F (dlaln2) (&c_false, &c__2, &c__1, &smin, &c_b82,
				&a[k1 + k1 * a_dim1], lda, &c_b82,
				&c_b82, vec, &c__2, &d__1, &c_b83, x,
				&c__2, &scaloc, &xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L80: */
			}
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l1 + k2 * c_dim1] = x[1];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 == k2)
		{
		  /* 
		   */
		  i__1 = *n - k1;
		  /*Computing MIN 
		   */
		  i__2 = k1 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k1 + 1;
		  suml =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  sumr =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__1 = *n - k1;
		  /*Computing MIN 
		   */
		  i__2 = k1 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k1 + 1;
		  suml =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l2 * c_dim1], &c__1);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  sumr =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l2 + Min (i__3, *n) * a_dim1], lda);
		  vec[1] = c__[k1 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  d__1 = -a[k1 + k1 * a_dim1];
		  C2F (dlaln2) (&c_false, &c__2, &c__1, &smin, &c_b82,
				&a[l1 + l1 * a_dim1], lda, &c_b82,
				&c_b82, vec, &c__2, &d__1, &c_b83, x,
				&c__2, &scaloc, &xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L90: */
			}
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[1];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l2 + k1 * c_dim1] = x[1];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 != k2)
		{
		  /* 
		   */
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  suml =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  sumr =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  suml =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l2 * c_dim1], &c__1);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  sumr =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l2 + Min (i__3, *n) * a_dim1], lda);
		  vec[2] = c__[k1 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  suml =
		    C2F (ddot) (&i__1,
				&a[k2 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  sumr =
		    C2F (ddot) (&i__1,
				&c__[k2 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  vec[1] = c__[k2 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  suml =
		    C2F (ddot) (&i__1,
				&a[k2 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l2 * c_dim1], &c__1);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  sumr =
		    C2F (ddot) (&i__1,
				&c__[k2 + Min (i__2, *n) * c_dim1],
				ldc, &a[l2 + Min (i__3, *n) * a_dim1], lda);
		  vec[3] = c__[k2 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  if (k1 == l1)
		    {
		      nsp_ctrlpack_dlaly2 (&c_true, &a[k1 + k1 * a_dim1], lda,
					   vec, &c__2, &scaloc, x, &c__2,
					   &xnorm, &ierr);
		    }
		  else
		    {
		      C2F (dlasy2) (&c_false, &c_true, &c__1, &c__2,
				    &c__2, &a[k1 + k1 * a_dim1], lda,
				    &a[l1 + l1 * a_dim1], lda, vec,
				    &c__2, &scaloc, x, &c__2, &xnorm, &ierr);
		    }
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L100: */
			}
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[2];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[k2 + l2 * c_dim1] = x[3];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		      c__[l2 + k1 * c_dim1] = x[2];
		      c__[l1 + k2 * c_dim1] = x[1];
		      c__[l2 + k2 * c_dim1] = x[3];
		    }
		}
	      /* 
	       */
	    L110:
	      ;
	    }
	L120:
	  ;
	}
      /* 
       */
    }
  /* 
   */
  return 0;
  /* 
   *    End of LYPCTR 
   * 
   */
}				/* lypctr_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * LYPDFR estimates the forward error bound for the computed solution of 
 * the discrete-time matrix Lyapunov equation 
 * 
 *        transpose(op(A))*X*op(A) - X = scale*C 
 * 
 * where op(A) = A or A**T and C is symmetric (C = C**T). A is N-by-N, 
 * the right hand side C and the solution X are N-by-N, and scale is 
 * scale factor, set <= 1 during the solution of the equation to avoid 
 * overflow in X. If the equation is not scaled, scale should be set 
 * equal to 1. 
 * 
 * Arguments 
 * ========= 
 * 
 * TRANA   (input) CHARACTER*1 
 *         Specifies the option op(A): 
 *         = 'N': op(A) = A    (No transpose) 
 *         = 'T': op(A) = A**T (Transpose) 
 *         = 'C': op(A) = A**T (Conjugate transpose = Transpose) 
 * 
 * N       (input) INT 
 *         The order of the matrix A, and the order of the 
 *         matrices C and X. N >= 0. 
 * 
 * A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
 *         The N-by-N matrix A. 
 * 
 * LDA     (input) INT 
 *         The leading dimension of the array A. LDA >= Max(1,N). 
 * 
 * UPLO    (input) CHARACTER*1 
 *         = 'U':  Upper triangle of C is stored; 
 *         = 'L':  Lower triangle of C is stored. 
 * 
 * C       (input) DOUBLE PRECISION array, dimension (LDC,N) 
 *         If UPLO = 'U', the leading N-by-N upper triangular part of C 
 *         contains the upper triangular part of the matrix C. 
 *         If UPLO = 'L', the leading N-by-N lower triangular part of C 
 *         contains the lower triangular part of the matrix C. 
 * 
 * LDC     (input) INT 
 *         The leading dimension of the array C. LDC >= Max(1,N) 
 * 
 * T       (input) DOUBLE PRECISION array, dimension (LDT,N) 
 *         The upper quasi-triangular matrix in Schur canonical 
 *         form from the Schur factorization of A. 
 * 
 * LDT     (input) INT 
 *         The leading dimension of the array T. LDT >= Max(1,N) 
 * 
 * U       (input) DOUBLE PRECISION array, dimension (LDU,N) 
 *         The orthogonal matrix U from the real Schur 
 *         factorization of A. 
 * 
 * LDU     (input) INT 
 *         The leading dimension of the array U. LDU >= Max(1,N) 
 * 
 * X       (input) DOUBLE PRECISION array, dimension (LDX,N) 
 *         The N-by-N solution matrix X. 
 * 
 * LDX     (input) INT 
 *         The leading dimension of the array X. LDX >= Max(1,N) 
 * 
 * SCALE   (input) DOUBLE PRECISION 
 *         The scale factor, scale. 
 * 
 * FERR    (output) DOUBLE PRECISION 
 *         On exit, an estimated forward error bound for the solution X. 
 *         If XTRUE is the true solution, FERR bounds the magnitude 
 *         of the largest entry in (X - XTRUE)  divided by the magnitude 
 *         of the largest entry in X. 
 * 
 * WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK) 
 * 
 * LWORK   (input) INT 
 *         The dimension of the array WORK. 
 *         LWORK >= 7*N*N + 2*N. 
 * 
 * IWORK   (workspace) INT array, dimension (N*N) 
 * 
 * INFO    (output) INT 
 *         = 0: successful exit 
 *         < 0: if INFO = -i, the i-th argument had an illegal value 
 * 
 * Further Details 
 * =============== 
 * 
 * The forward error bound is estimated using a practical error bound 
 * similar to the one proposed in [1]. 
 * 
 * References 
 * ========== 
 * 
 * [1] N.J. Higham, Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * 
 */

int
nsp_ctrlpack_lypdfr (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *t, int *ldt, double *u,
		     int *ldu, double *x, int *ldx, double *scale,
		     double *ferr, double *work, int *lwork, int *iwork,
		     int *info, long int trana_len, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, t_dim1, t_offset, u_dim1, u_offset,
    x_dim1, x_offset, i__1, i__2;
  double d__1, d__2, d__3;

  /* Local variables */
  int idlc, iabs, kase, ixma, ires, ixbs, itmp, iwrk, info2, i__, j;
  int lower;
  double xnorm, scale2;
  int ij;
  char tranat[1];
  int notrna;
  int minwrk;
  double eps, est;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --work;
  --iwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldt < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -11;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -13;
    }
  /* 
   *    Get the machine precision 
   * 
   */
  eps = C2F (dlamch) ("Epsilon", 7L);
  /* 
   *    Compute workspace 
   * 
   */
  minwrk = *n * 7 * *n + (*n << 1);
  if (*lwork < minwrk)
    {
      *info = -17;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("LYPDFR", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  xnorm = C2F (dlansy) ("1", uplo, n, &x[x_offset], ldx, &work[1], 1L, 1L);
  if (xnorm == 0.)
    {
      /* 
       *       Matrix all zero 
       * 
       */
      *ferr = 0.;
      return 0;
    }
  /* 
   *    Workspace usage 
   * 
   */
  idlc = *n * *n;
  itmp = idlc + *n * *n;
  ixma = itmp + *n * *n;
  iabs = ixma + *n * *n;
  ixbs = iabs + *n * *n;
  ires = ixbs + *n * *n;
  iwrk = ires + *n * *n;
  /* 
   *    Compute X*op(A) 
   * 
   */
  C2F (dgemm) ("N", trana, n, n, n, &c_b82, &x[x_offset], ldx,
	       &a[a_offset], lda, &c_b83, &work[ixma + 1], n, 1L, 1L);
  /* 
   */
  if (notrna)
    {
      *(unsigned char *) tranat = 'T';
    }
  else
    {
      *(unsigned char *) tranat = 'N';
    }
  /* 
   *    Form residual matrix R = C + X - op(A')*X*op(A) 
   * 
   */
  C2F (dgemm) (tranat, "N", n, n, n, &c_b82, &a[a_offset], lda,
	       &work[ixma + 1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
  if (lower)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = j; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] =
		*scale * c__[i__ + j * c_dim1] + x[i__ + j * x_dim1] -
		work[itmp + i__ + (j - 1) * *n];
	      /* L10: */
	    }
	  /* L20: */
	}
    }
  else
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = j;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] =
		*scale * c__[i__ + j * c_dim1] + x[i__ + j * x_dim1] -
		work[itmp + i__ + (j - 1) * *n];
	      /* L30: */
	    }
	  /* L40: */
	}
    }
  /* 
   *    Add to Abs(R) a term that takes account of rounding errors in 
   *    forming R: 
   *      Abs(R) := Abs(R) + EPS*(3*abs(C) + 3*abs(X) + 
   *                2*(n+1)*abs(op(A'))*abs(X)*abs(op(A))) 
   *    where EPS is the machine precision 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  work[iabs + i__ + (j - 1) * *n] = (d__1 =
					     a[i__ + j * a_dim1], Abs (d__1));
	  work[ixbs + i__ + (j - 1) * *n] = (d__1 =
					     x[i__ + j * x_dim1], Abs (d__1));
	  /* L50: */
	}
      /* L60: */
    }
  C2F (dgemm) ("N", trana, n, n, n, &c_b82, &work[ixbs + 1], n,
	       &work[iabs + 1], n, &c_b83, &work[ixma + 1], n, 1L, 1L);
  C2F (dgemm) (tranat, "N", n, n, n, &c_b82, &work[iabs + 1], n,
	       &work[ixma + 1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
  if (lower)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = j; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] = (d__1 =
						 work[ires + i__ +
						      (j - 1) * *n],
						 Abs (d__1)) +
		eps * 3. * (*scale *
			    (d__2 =
			     c__[i__ + j * c_dim1], Abs (d__2)) + (d__3 =
								   x[i__ +
								     j *
								     x_dim1],
								   Abs
								   (d__3))) +
		(double) ((*n << 1) + 2) * eps * work[itmp + i__ +
						      (j - 1) * *n];
	      /* L70: */
	    }
	  /* L80: */
	}
    }
  else
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = j;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] = (d__1 =
						 work[ires + i__ +
						      (j - 1) * *n],
						 Abs (d__1)) +
		eps * 3. * (*scale *
			    (d__2 =
			     c__[i__ + j * c_dim1], Abs (d__2)) + (d__3 =
								   x[i__ +
								     j *
								     x_dim1],
								   Abs
								   (d__3))) +
		(double) ((*n << 1) + 2) * eps * work[itmp + i__ +
						      (j - 1) * *n];
	      /* L90: */
	    }
	  /* L100: */
	}
    }
  /* 
   *    Compute forward error bound, using matrix norm estimator 
   * 
   */
  est = 0.;
  kase = 0;
 L110:
  i__1 = *n * (*n + 1) / 2;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] =
			work[ij] * work[ires + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Unpack the lower triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] = work[ij];
		    }
		  /* L120: */
		}
	      /* L130: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] =
			work[ij] * work[ires + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Unpack the upper triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] = work[ij];
		    }
		  /* L140: */
		}
	      /* L150: */
	    }
	}
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      if (kase == 2)
	{
	  /* 
	   *          Solve op(A')*Y*op(A) - Y = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (trana, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale2, &work[iwrk + 1], &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(A)*Z*op(A') - Z = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (tranat, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale2, &work[iwrk + 1], &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n,
		   &u[u_offset], ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Pack the lower triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[ij] = work[itmp + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[ij] =
			work[itmp + i__ + (j - 1) * *n] * work[ires + i__ +
							       (j - 1) * *n];
		    }
		  /* L160: */
		}
	      /* L170: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Pack the upper triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[ij] = work[itmp + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[ij] =
			work[itmp + i__ + (j - 1) * *n] * work[ires + i__ +
							       (j - 1) * *n];
		    }
		  /* L180: */
		}
	      /* L190: */
	    }
	}
      goto L110;
    }
  /* 
   *    Compute the estimate of the forward error 
   * 
   */
  *ferr =
    est * 2. / C2F (dlansy) ("Max", uplo, n, &x[x_offset], ldx,
			     &work[1], 3L, 1L) / scale2;
  if (*ferr > 1.)
    {
      *ferr = 1.;
    }
  /* 
   */
  return 0;
  /* 
   *    End of LYPDFR 
   * 
   */
}				/* lypdfr_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * LYPDRC estimates the reciprocal of the condition number of the 
 * The condition number of the discrete Lyapunov equation is estimated 
 * as 
 * 
 * cond = (norm(Theta)*norm(A) + norm(inv(Omega))*norm(C))/norm(X) 
 * 
 * where Omega and Theta are linear operators defined by 
 * 
 * Omega(Z) = transpose(op(A))*Z*op(A) - Z, 
 * Theta(Z) = inv(Omega(transpose(op(Z))*X*op(A) + 
 *                transpose(op(A))*X*op(Z))). 
 * 
 * The program estimates the quantities 
 * 
 * sepd(op(A),transpose(op(A)) = 1 / norm(inv(Omega)) 
 * 
 * and norm(Theta) using 1-norm condition estimator. 
 * 
 * References 
 * ========== 
 * 
 * [1] N.J. Higham, Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * 
 */

int
nsp_ctrlpack_lypdrc (char *fact, char *trana, int *n, double *a, int *lda,
		     char *uplo, double *c__, int *ldc, double *t, int *ldt,
		     double *u, int *ldu, double *x, int *ldx, double *scale,
		     double *rcond, double *work, int *lwork, int *iwork,
		     int *info, long int fact_len, long int trana_len,
		     long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, t_dim1, t_offset, u_dim1, u_offset,
    x_dim1, x_offset, i__1, i__2;

  /* Local variables */
  int idlc, kase, sdim;
  double sepd;
  int ixma, itmp, iwrk, info2, i__, j;
  double anorm, cnorm;
  int bwork[1], lower;
  double xnorm, scale2;
  int ij;
  int nofact;
  char tranat[1];
  int notrna;
  int minwrk;
  double thnorm;
  int lwa, iwi;
  double est;
  int iwr;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --work;
  --iwork;

  /* Function Body */
  nofact = C2F (lsame) (fact, "N", 1L, 1L);
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!nofact && !C2F (lsame) (fact, "F", 1L, 1L))
    {
      *info = -1;
    }
  else if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
	   && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -2;
    }
  else if (*n < 0)
    {
      *info = -3;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -5;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -6;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -8;
    }
  else if (*ldt < Max (1, *n))
    {
      *info = -10;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -12;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -14;
    }
  /* 
   *    Compute workspace 
   * 
   *Computing MAX 
   */
  i__1 = 1, i__2 = *n * 3;
  minwrk = (*n << 2) * *n + (*n << 1) + Max (i__1, i__2);
  if (*lwork < minwrk)
    {
      *info = -18;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("LYPDRC", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Compute the norms of the matrices A and C 
   * 
   */
  anorm = C2F (dlange) ("1", n, n, &a[a_offset], lda, &work[1], 1L);
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  xnorm = C2F (dlansy) ("1", uplo, n, &x[x_offset], ldx, &work[1], 1L, 1L);
  if (xnorm == 0.)
    {
      /* 
       *       Matrix all zero 
       * 
       */
      *rcond = 0.;
      return 0;
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa = (*n << 2) * *n + (*n << 1);
  idlc = *n * *n;
  itmp = idlc + *n * *n;
  ixma = itmp + *n * *n;
  iwr = ixma + *n * *n;
  iwi = iwr + *n;
  iwrk = iwi + *n;
  /* 
   */
  if (nofact)
    {
      /* 
       *       Compute the Schur factorization of A 
       * 
       */
      C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &t[t_offset], ldt, 4L);
      i__1 = *lwork - iwrk;
      C2F (dgees) ("V", "N", nsp_ctrlpack_voiddummy, n, &t[t_offset], ldt,
		   &sdim, &work[iwr + 1], &work[iwi + 1], &u[u_offset],
		   ldu, &work[iwrk + 1], &i__1, bwork, &info2, 1L, 1L);
      if (info2 > 0)
	{
	  *info = 1;
	  return 0;
	}
      lwa += (int) work[iwrk + 1];
    }
  /* 
   *    Compute X*op(A) 
   * 
   */
  C2F (dgemm) ("N", trana, n, n, n, &c_b82, &x[x_offset], ldx,
	       &a[a_offset], lda, &c_b83, &work[ixma + 1], n, 1L, 1L);
  /* 
   *    Estimate sepd(op(A),transpose(op(A))) 
   * 
   */
  if (notrna)
    {
      *(unsigned char *) tranat = 'T';
    }
  else
    {
      *(unsigned char *) tranat = 'N';
    }
  /* 
   */
  est = 0.;
  kase = 0;
 L10:
  i__1 = *n * (*n + 1) / 2;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      /* 
       *       Unpack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L20: */
		}
	      /* L30: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L40: */
		}
	      /* L50: */
	    }
	}
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      if (kase == 1)
	{
	  /* 
	   *          Solve op(A')*Y*op(A) - Y = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (trana, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale2, &work[iwr + 1], &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(A)*Z*op(A') - Z = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (tranat, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale2, &work[iwr + 1], &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n,
		   &u[u_offset], ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      /* 
       *       Pack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L60: */
		}
	      /* L70: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L80: */
		}
	      /* L90: */
	    }
	}
      goto L10;
    }
  /* 
   */
  sepd = scale2 / 2. / est;
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (sepd == 0.)
    {
      *rcond = 0.;
      return 0;
    }
  /* 
   *    Estimate norm(Theta) 
   * 
   */
  est = 0.;
  kase = 0;
 L100:
  i__1 = *n * *n;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      /* 
       *       Compute RHS = op(W')*X*op(A) + op(A')*X*op(W) 
       * 
       */
      C2F (dsyr2k) (uplo, tranat, n, n, &c_b82, &work[1], n,
		    &work[ixma + 1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dlacpy) (uplo, n, n, &work[itmp + 1], n, &work[1], n, 1L);
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[1], n, &u[u_offset],
		   ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[itmp + 1], n, &c_b83, &work[1], n, 1L, 1L);
      if (kase == 1)
	{
	  /* 
	   *          Solve op(A')*Y*op(A) - Y = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (trana, n, &t[t_offset], ldt, &work[1], n,
			       &scale2, &work[iwr + 1], &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(A)*Z*op(A') - Z = scale2*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (tranat, n, &t[t_offset], ldt, &work[1], n,
			       &scale2, &work[iwr + 1], &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[1], n, &u[u_offset],
		   ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      goto L100;
    }
  /* 
   */
  thnorm = est / scale2;
  /* 
   *    Estimate the reciprocal condition number 
   * 
   */
  *rcond = sepd * xnorm / (cnorm * *scale + sepd * (thnorm * anorm));
  if (*rcond > 1.)
    {
      *rcond = 1.;
    }
  /* 
   */
  work[1] = (double) lwa;
  return 0;
  /* 
   *    End of LYPDRC 
   * 
   */
}				/* lypdrc_ */
/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * LYPDSL solves the discrete-time matrix Lyapunov equation 
 *         On exit, if INFO = 0, WORK(1) contains the optimal LWORK. 
 * 
 * LWORK   (input) INT 
 *         The dimension of the array WORK. 
 *         LWORK >= 7*N*N + 2*N + Max(1,3*N). 
 *         For good performance, LWORK must generally be larger. 
 * 
 * IWORK   (workspace) INT array, dimension (N*N) 
 * 
 * INFO    (output) INT 
 *         = 0: successful exit 
 *         < 0: if INFO = -i, the i-th argument had an illegal value 
 *         = 1: the matrix A cannot be reduced to Schur canonical form 
 *         = 2: A has almost reciprocal eigenvalues; perturbed 
 *               values were used to solve the equation (but the 
 *               matrix A is unchanged). 
 * 
 * Further Details 
 * =============== 
 * 
 * The discrete-time matrix Lyapunov equation is solved by the Barraud- 
 * Kitagawa algorithm [1], [2]. 
 * 
 * The condition number of the equation is estimated using 1-norm 
 * condition estimator. 
 * 
 * The forward error bound is estimated using the practical error bound 
 * proposed in [3]. 
 * 
 * References 
 * ========== 
 * 
 *                                                   T 
 * [1] A.Y. Barraud. A numerical algorithm to solve A XA - X = Q. 
 *     IEEE Trans. Automat. Control, vol. AC-22, pp. 883-885, 1977. 
 * [2] G. Kitagawa. An algorithm for solving the matrix equation X = 
 *        T 
 *     FXF  + S. Internat. J. Control, vol. 25, pp. 745-753, 1977. 
 * [3] N.J. Higham, Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, 1993, pp. 124-136. 
 * 
 */

int
nsp_ctrlpack_lypdsl (char *fact, char *trana, int *n, double *a, int *lda,
		     char *uplo, double *c__, int *ldc, double *t, int *ldt,
		     double *u, int *ldu, double *wr, double *wi, double *x,
		     int *ldx, double *scale, double *rcond, double *ferr,
		     double *work, int *lwork, int *iwork, int *info,
		     long int fact_len, long int trana_len, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, t_dim1, t_offset, u_dim1, u_offset,
    x_dim1, x_offset, i__1, i__2;

  /* Local variables */
  int sdim, info2;
  double cnorm;
  int bwork[1], lower;
  int nofact;
  int lwamax;
  int notrna;
  int minwrk;
  int lwa;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  --wr;
  --wi;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --work;
  --iwork;

  /* Function Body */
  nofact = C2F (lsame) (fact, "N", 1L, 1L);
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!nofact && !C2F (lsame) (fact, "F", 1L, 1L))
    {
      *info = -1;
    }
  else if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
	   && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -2;
    }
  else if (*n < 0)
    {
      *info = -3;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -5;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -6;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -8;
    }
  else if (*ldt < Max (1, *n))
    {
      *info = -10;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -12;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -16;
    }
  /* 
   *    Compute workspace 
   * 
   *Computing MAX 
   */
  i__1 = 1, i__2 = *n * 3;
  minwrk = *n * 7 * *n + (*n << 1) + Max (i__1, i__2);
  if (*lwork < minwrk)
    {
      *info = -21;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("LYPDSL", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  if (cnorm == 0.)
    {
      /* 
       *       Matrix all zero. Return zero solution 
       * 
       */
      C2F (dlaset) ("F", n, n, &c_b83, &c_b83, &x[x_offset], ldx, 1L);
      *scale = 1.;
      *rcond = 0.;
      *ferr = 0.;
      return 0;
    }
  /* 
   */
  lwa = 0;
  /* 
   */
  if (nofact)
    {
      /* 
       *       Compute the Schur factorization of A 
       * 
       */
      C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &t[t_offset], ldt, 4L);
      C2F (dgees) ("V", "N", nsp_ctrlpack_voiddummy, n, &t[t_offset], ldt,
		   &sdim, &wr[1], &wi[1], &u[u_offset], ldu, &work[1],
		   lwork, bwork, &info2, 1L, 1L);
      if (info2 > 0)
	{
	  *info = 1;
	  return 0;
	}
      lwa = (int) work[1];
    }
  lwamax = lwa;
  /* 
   *    Transform the right-hand side: C := U'*C*U. 
   *    Form TEMP = C*U then X = U'*TEMP 
   * 
   */
  C2F (dsymm) ("L", uplo, n, n, &c_b82, &c__[c_offset], ldc,
	       &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
  C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu, &work[1],
	       n, &c_b83, &x[x_offset], ldx, 1L, 1L);
  /* 
   *    Solve the quasi-triangular discrete-time Lyapunov equation. 
   *    The answer overwrites the right-hand side 
   * 
   */
  nsp_ctrlpack_lypdtr (trana, n, &t[t_offset], ldt, &x[x_offset], ldx, scale,
		       &work[1], &info2, 1L);
  if (info2 > 0)
    {
      *info = 2;
    }
  /* 
   *    Transform back to obtain the solution: X := U*X*U'. 
   *    Form TEMP = U*X then X = TEMP*U' 
   * 
   */
  C2F (dsymm) ("R", uplo, n, n, &c_b82, &x[x_offset], ldx,
	       &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
  C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n, &u[u_offset],
	       ldu, &c_b83, &x[x_offset], ldx, 1L, 1L);
  /* 
   *    Estimate the reciprocal of the condition number 
   * 
   */
  nsp_ctrlpack_lypdrc ("F", trana, n, &a[a_offset], lda, uplo, &c__[c_offset],
		       ldc, &t[t_offset], ldt, &u[u_offset], ldu,
		       &x[x_offset], ldx, scale, rcond, &work[1], lwork,
		       &iwork[1], &info2, 1L, 1L, 1L);
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (*rcond == 0.)
    {
      *ferr = 1.;
      return 0;
    }
  lwa = (int) work[1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Estimate the bound on the forward error 
   * 
   */
  nsp_ctrlpack_lypdfr (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &t[t_offset], ldt, &u[u_offset], ldu, &x[x_offset],
		       ldx, scale, ferr, &work[1], lwork, &iwork[1], &info2,
		       1L, 1L);
  lwa = *n * 7 * *n + (*n << 1);
  lwamax = Max (lwa, lwamax);
  /* 
   */
  work[1] = (double) lwamax;
  return 0;
  /* 
   *    End of LYPDSL 
   * 
   */
}				/* lypdsl_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * LYPDTR solves the discrete-time matrix Lyapunov equation 
 * 
 *        transpose(op(A))*X*op(A) - X = scale*C 
 * 
 * where op(A) = A or A**T,  A is upper quasi-triangular and C is 
 * symmetric (C = C**T). A is N-by-N, the right hand side C and the 
 * solution X are N-by-N, and scale is an output scale factor, 
 * set <= 1 to avoid overflow in X. 
 * 
 * A must be in Schur canonical form (as returned by DHSEQR), that is, 
 * block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; 
 * each 2-by-2 diagonal block has its diagonal elements equal and its 
 * off-diagonal elements of opposite sign. 
 * 
 * Arguments 
 * ========= 
 * 
 * TRANA   (input) CHARACTER*1 
 *         Specifies the option op(A): 
 *         = 'N': op(A) = A    (No transpose) 
 *         = 'T': op(A) = A**T (Transpose) 
 *         = 'C': op(A) = A**H (Conjugate transpose = Transpose) 
 * 
 * N       (input) INT 
 *         The order of the matrix A, and the order of the 
 *         matrices X and C. N >= 0. 
 * 
 * A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
 *         The upper quasi-triangular matrix A, in Schur canonical form. 
 * 
 * LDA     (input) INT 
 *         The leading dimension of the array A. LDA >= Max(1,N). 
 * 
 * C       (input/output) DOUBLE PRECISION array, dimension (LDC,N) 
 *         On entry, the symmetric N-by-N right hand side matrix C. 
 *         On exit, C is overwritten by the solution matrix X. 
 * 
 * LDC     (input) INT 
 *         The leading dimension of the array C. LDC >= Max(1,N) 
 * 
 * SCALE   (output) DOUBLE PRECISION 
 *         The scale factor, scale, set <= 1 to avoid overflow in X. 
 * 
 * WORK    (workspace) DOUBLE PRECISION array, dimension (N,2) 
 * 
 * INFO    (output) INT 
 *         = 0: successful exit 
 *         < 0: if INFO = -i, the i-th argument had an illegal value 
 *         = 1: A has almost reciprocal eigenvalues; perturbed 
 *               values were used to solve the equation (but the 
 *               matrix A is unchanged). 
 */

int
nsp_ctrlpack_lypdtr (char *trana, int *n, double *a, int *lda, double *c__,
		     int *ldc, double *scale, double *work, int *info,
		     long int trana_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, work_dim1, work_offset, i__1, i__2,
    i__3;
  double d__1, d__2;

  /* Local variables */
  int ierr;
  double smin, suml, sumr;
  int j, k, l;
  double x[4] /* was [2][2] */ ;
  int knext, lnext, k1, k2, l1, l2;
  double xnorm;
  double a11, db;
  double p11, p12, p21, p22;
  double scaloc;
  double bignum;
  int notrna;
  double smlnum, da11, vec[4] /* was [2][2] */ , dum[1], eps;

  /* Parameter adjustments */
  work_dim1 = *n;
  work_offset = work_dim1 + 1;
  work -= work_offset;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -6;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("LYPDTR", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Set constants to control overflow 
   * 
   */
  eps = C2F (dlamch) ("P", 1L);
  smlnum = C2F (dlamch) ("S", 1L);
  bignum = 1. / smlnum;
  C2F (dlabad) (&smlnum, &bignum);
  smlnum = smlnum * (double) (*n * *n) / eps;
  bignum = 1. / smlnum;
  /* 
   *Computing MAX 
   */
  d__1 = smlnum, d__2 =
    eps * C2F (dlange) ("M", n, n, &a[a_offset], lda, dum, 1L);
  smin = Max (d__1, d__2);
  /* 
   */
  *scale = 1.;
  /* 
   */
  if (notrna)
    {
      /* 
       *       Solve    A'*X*A - X = scale*C. 
       * 
       *       The (K,L)th block of X is determined starting from 
       *       upper-left corner column by column by 
       * 
       *         A(K,K)'*X(K,L)*A(L,L) - X(K,L) = C(K,L) - R(K,L) 
       * 
       *       where 
       * 
       *                   K           L-1 
       *         R(K,L) = SUM {A(I,K)'*SUM [X(I,J)*A(J,L)]} + 
       *                  I=1          J=1 
       * 
       *                   K-1 
       *                  {SUM [A(I,K)'*X(I,L)]}*A(L,L) 
       *                   I=1 
       * 
       *       Start column loop (index = L) 
       *       L1 (L2): column index of the first (last) row of X(K,L) 
       * 
       */
      lnext = 1;
      i__1 = *n;
      for (l = 1; l <= i__1; ++l)
	{
	  if (l < lnext)
	    {
	      goto L60;
	    }
	  if (l == *n)
	    {
	      l1 = l;
	      l2 = l;
	    }
	  else
	    {
	      if (a[l + 1 + l * a_dim1] != 0.)
		{
		  l1 = l;
		  l2 = l + 1;
		  lnext = l + 2;
		}
	      else
		{
		  l1 = l;
		  l2 = l;
		  lnext = l + 1;
		}
	    }
	  /* 
	   *          Start row loop (index = K) 
	   *          K1 (K2): row index of the first (last) row of X(K,L) 
	   * 
	   */
	  C2F (dscal) (&l1, &c_b83, &work[work_dim1 + 1], &c__1);
	  C2F (dscal) (&l1, &c_b83, &work[(work_dim1 << 1) + 1], &c__1);
	  i__2 = l1 - 1;
	  C2F (dsymv) ("L", &i__2, &c_b82, &c__[c_offset], ldc,
		       &a[l1 * a_dim1 + 1], &c__1, &c_b83,
		       &work[work_dim1 + 1], &c__1, 1L);
	  i__2 = l1 - 1;
	  C2F (dsymv) ("L", &i__2, &c_b82, &c__[c_offset], ldc,
		       &a[l2 * a_dim1 + 1], &c__1, &c_b83,
		       &work[(work_dim1 << 1) + 1], &c__1, 1L);
	  /* 
	   */
	  knext = l;
	  i__2 = *n;
	  for (k = l; k <= i__2; ++k)
	    {
	      if (k < knext)
		{
		  goto L50;
		}
	      if (k == *n)
		{
		  k1 = k;
		  k2 = k;
		}
	      else
		{
		  if (a[k + 1 + k * a_dim1] != 0.)
		    {
		      k1 = k;
		      k2 = k + 1;
		      knext = k + 2;
		    }
		  else
		    {
		      k1 = k;
		      k2 = k;
		      knext = k + 1;
		    }
		}
	      /* 
	       */
	      if (l1 == l2 && k1 == k2)
		{
		  i__3 = l1 - 1;
		  work[k1 + work_dim1] =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  i__3 = k1 - 1;
		  p11 =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  /* 
		   */
		  suml =
		    C2F (ddot) (&k1, &a[k1 * a_dim1 + 1], &c__1,
				&work[work_dim1 + 1], &c__1);
		  sumr = p11 * a[l1 + l1 * a_dim1];
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  scaloc = 1.;
		  /* 
		   */
		  a11 = a[k1 + k1 * a_dim1] * a[l1 + l1 * a_dim1] - 1.;
		  da11 = Abs (a11);
		  if (da11 <= smin)
		    {
		      a11 = smin;
		      da11 = smin;
		      *info = 1;
		    }
		  db = Abs (vec[0]);
		  if (da11 < 1. && db > 1.)
		    {
		      if (db > bignum * da11)
			{
			  scaloc = 1. / db;
			}
		    }
		  x[0] = vec[0] * scaloc / a11;
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L10: */
			}
		      C2F (dscal) (n, &scaloc, &work[work_dim1 + 1], &c__1);
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		    }
		  /* 
		   */
		}
	      else if (l1 == l2 && k1 != k2)
		{
		  /* 
		   */
		  i__3 = l1 - 1;
		  work[k1 + work_dim1] =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  work[k2 + work_dim1] =
		    C2F (ddot) (&i__3, &c__[k2 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  i__3 = k1 - 1;
		  p11 =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  i__3 = k1 - 1;
		  p21 =
		    C2F (ddot) (&i__3, &a[k2 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  /* 
		   */
		  suml =
		    C2F (ddot) (&k2, &a[k1 * a_dim1 + 1], &c__1,
				&work[work_dim1 + 1], &c__1);
		  sumr = p11 * a[l1 + l1 * a_dim1];
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  suml =
		    C2F (ddot) (&k2, &a[k2 * a_dim1 + 1], &c__1,
				&work[work_dim1 + 1], &c__1);
		  sumr = p21 * a[l1 + l1 * a_dim1];
		  vec[1] = c__[k2 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  nsp_ctrlpack_dlasd2 (&c_true, &c_false, &c__1, &c__2, &c__1,
				       &a[k1 + k1 * a_dim1], lda,
				       &a[l1 + l1 * a_dim1], lda, vec, &c__2,
				       &scaloc, x, &c__2, &xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L20: */
			}
		      C2F (dscal) (n, &scaloc, &work[work_dim1 + 1], &c__1);
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l1 + k2 * c_dim1] = x[1];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 == k2)
		{
		  /* 
		   */
		  i__3 = l1 - 1;
		  work[k1 + work_dim1] =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  work[k1 + (work_dim1 << 1)] =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l2 * a_dim1 + 1], &c__1);
		  i__3 = k1 - 1;
		  p11 =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  i__3 = k1 - 1;
		  p12 =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l2 * c_dim1 + 1], &c__1);
		  /* 
		   */
		  suml =
		    C2F (ddot) (&k1, &a[k1 * a_dim1 + 1], &c__1,
				&work[work_dim1 + 1], &c__1);
		  sumr =
		    p11 * a[l1 + l1 * a_dim1] + p12 * a[l2 + l1 * a_dim1];
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  suml =
		    C2F (ddot) (&k1, &a[k1 * a_dim1 + 1], &c__1,
				&work[(work_dim1 << 1) + 1], &c__1);
		  sumr =
		    p11 * a[l1 + l2 * a_dim1] + p12 * a[l2 + l2 * a_dim1];
		  vec[2] = c__[k1 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  nsp_ctrlpack_dlasd2 (&c_true, &c_false, &c__1, &c__1, &c__2,
				       &a[k1 + k1 * a_dim1], lda,
				       &a[l1 + l1 * a_dim1], lda, vec, &c__2,
				       &scaloc, x, &c__2, &xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L30: */
			}
		      C2F (dscal) (n, &scaloc, &work[work_dim1 + 1], &c__1);
		      C2F (dscal) (n, &scaloc,
				   &work[(work_dim1 << 1) + 1], &c__1);
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[2];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l2 + k1 * c_dim1] = x[2];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 != k2)
		{
		  /* 
		   */
		  i__3 = l1 - 1;
		  work[k1 + work_dim1] =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  work[k2 + work_dim1] =
		    C2F (ddot) (&i__3, &c__[k2 + c_dim1], ldc,
				&a[l1 * a_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  work[k1 + (work_dim1 << 1)] =
		    C2F (ddot) (&i__3, &c__[k1 + c_dim1], ldc,
				&a[l2 * a_dim1 + 1], &c__1);
		  i__3 = l1 - 1;
		  work[k2 + (work_dim1 << 1)] =
		    C2F (ddot) (&i__3, &c__[k2 + c_dim1], ldc,
				&a[l2 * a_dim1 + 1], &c__1);
		  i__3 = k1 - 1;
		  p11 =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  i__3 = k1 - 1;
		  p12 =
		    C2F (ddot) (&i__3, &a[k1 * a_dim1 + 1], &c__1,
				&c__[l2 * c_dim1 + 1], &c__1);
		  i__3 = k1 - 1;
		  p21 =
		    C2F (ddot) (&i__3, &a[k2 * a_dim1 + 1], &c__1,
				&c__[l1 * c_dim1 + 1], &c__1);
		  i__3 = k1 - 1;
		  p22 =
		    C2F (ddot) (&i__3, &a[k2 * a_dim1 + 1], &c__1,
				&c__[l2 * c_dim1 + 1], &c__1);
		  /* 
		   */
		  suml =
		    C2F (ddot) (&k2, &a[k1 * a_dim1 + 1], &c__1,
				&work[work_dim1 + 1], &c__1);
		  sumr =
		    p11 * a[l1 + l1 * a_dim1] + p12 * a[l2 + l1 * a_dim1];
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  suml =
		    C2F (ddot) (&k2, &a[k1 * a_dim1 + 1], &c__1,
				&work[(work_dim1 << 1) + 1], &c__1);
		  sumr =
		    p11 * a[l1 + l2 * a_dim1] + p12 * a[l2 + l2 * a_dim1];
		  vec[2] = c__[k1 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  suml =
		    C2F (ddot) (&k2, &a[k2 * a_dim1 + 1], &c__1,
				&work[work_dim1 + 1], &c__1);
		  sumr =
		    p21 * a[l1 + l1 * a_dim1] + p22 * a[l2 + l1 * a_dim1];
		  vec[1] = c__[k2 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  suml =
		    C2F (ddot) (&k2, &a[k2 * a_dim1 + 1], &c__1,
				&work[(work_dim1 << 1) + 1], &c__1);
		  sumr =
		    p21 * a[l1 + l2 * a_dim1] + p22 * a[l2 + l2 * a_dim1];
		  vec[3] = c__[k2 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  if (k1 == l1)
		    {
		      nsp_ctrlpack_dlald2 (&c_false, &a[k1 + k1 * a_dim1],
					   lda, vec, &c__2, &scaloc, x, &c__2,
					   &xnorm, &ierr);
		    }
		  else
		    {
		      nsp_ctrlpack_dlasd2 (&c_true, &c_false, &c__1, &c__2,
					   &c__2, &a[k1 + k1 * a_dim1], lda,
					   &a[l1 + l1 * a_dim1], lda, vec,
					   &c__2, &scaloc, x, &c__2, &xnorm,
					   &ierr);
		    }
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__3 = *n;
		      for (j = 1; j <= i__3; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L40: */
			}
		      C2F (dscal) (n, &scaloc, &work[work_dim1 + 1], &c__1);
		      C2F (dscal) (n, &scaloc,
				   &work[(work_dim1 << 1) + 1], &c__1);
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[2];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[k2 + l2 * c_dim1] = x[3];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		      c__[l2 + k1 * c_dim1] = x[2];
		      c__[l1 + k2 * c_dim1] = x[1];
		      c__[l2 + k2 * c_dim1] = x[3];
		    }
		}
	      /* 
	       */
	    L50:
	      ;
	    }
	L60:
	  ;
	}
      /* 
       */
    }
  else
    {
      /* 
       *       Solve    A*X*A' - X = scale*C. 
       * 
       *       The (K,L)th block of X is determined starting from 
       *       bottom-right corner column by column by 
       * 
       *           A(K,K)*X(K,L)*A(L,L)' = C(K,L) - R(K,L) 
       * 
       *       where 
       * 
       *                   N            N 
       *         R(K,L) = SUM {A(K,I)* SUM [X(I,J)*A(L,J)']} + 
       *                  I=K         J=L+1 
       * 
       *                     N 
       *                  { SUM [A(K,J)*X(J,L)]}*A(L,L)' 
       *                   J=K+1 
       * 
       *       Start column loop (index = L) 
       *       L1 (L2): column index of the first (last) row of X(K,L) 
       * 
       */
      lnext = *n;
      for (l = *n; l >= 1; --l)
	{
	  if (l > lnext)
	    {
	      goto L120;
	    }
	  if (l == 1)
	    {
	      l1 = l;
	      l2 = l;
	    }
	  else
	    {
	      if (a[l + (l - 1) * a_dim1] != 0.)
		{
		  l1 = l - 1;
		  l2 = l;
		  lnext = l - 2;
		}
	      else
		{
		  l1 = l;
		  l2 = l;
		  lnext = l - 1;
		}
	    }
	  /* 
	   *          Start row loop (index = K) 
	   *          K1 (K2): row index of the first (last) row of X(K,L) 
	   * 
	   */
	  i__1 = *n - l1 + 1;
	  C2F (dscal) (&i__1, &c_b83, &work[l1 + work_dim1], &c__1);
	  i__1 = *n - l1 + 1;
	  C2F (dscal) (&i__1, &c_b83, &work[l1 + (work_dim1 << 1)], &c__1);
	  i__1 = *n - l2;
	  C2F (dsymv) ("U", &i__1, &c_b82,
		       &c__[l2 + 1 + (l2 + 1) * c_dim1], ldc,
		       &a[l1 + (l2 + 1) * a_dim1], lda, &c_b83,
		       &work[l2 + 1 + work_dim1], &c__1, 1L);
	  i__1 = *n - l2;
	  C2F (dsymv) ("U", &i__1, &c_b82,
		       &c__[l2 + 1 + (l2 + 1) * c_dim1], ldc,
		       &a[l2 + (l2 + 1) * a_dim1], lda, &c_b83,
		       &work[l2 + 1 + (work_dim1 << 1)], &c__1, 1L);
	  /* 
	   */
	  knext = l;
	  for (k = l; k >= 1; --k)
	    {
	      if (k > knext)
		{
		  goto L110;
		}
	      if (k == 1)
		{
		  k1 = k;
		  k2 = k;
		}
	      else
		{
		  if (a[k + (k - 1) * a_dim1] != 0.)
		    {
		      k1 = k - 1;
		      k2 = k;
		      knext = k - 2;
		    }
		  else
		    {
		      k1 = k;
		      k2 = k;
		      knext = k - 1;
		    }
		}
	      /* 
	       */
	      if (l1 == l2 && k1 == k2)
		{
		  i__1 = *n - l1;
		  /*Computing MIN 
		   */
		  i__2 = l1 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l1 + 1;
		  work[k1 + work_dim1] =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  i__1 = *n - k1;
		  /*Computing MIN 
		   */
		  i__2 = k1 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k1 + 1;
		  p11 =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  /* 
		   */
		  i__1 = *n - k1 + 1;
		  suml =
		    C2F (ddot) (&i__1, &a[k1 + k1 * a_dim1], lda,
				&work[k1 + work_dim1], &c__1);
		  sumr = p11 * a[l1 + l1 * a_dim1];
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  scaloc = 1.;
		  /* 
		   */
		  a11 = a[k1 + k1 * a_dim1] * a[l1 + l1 * a_dim1] - 1.;
		  da11 = Abs (a11);
		  if (da11 <= smin)
		    {
		      a11 = smin;
		      da11 = smin;
		      *info = 1;
		    }
		  db = Abs (vec[0]);
		  if (da11 < 1. && db > 1.)
		    {
		      if (db > bignum * da11)
			{
			  scaloc = 1. / db;
			}
		    }
		  x[0] = vec[0] * scaloc / a11;
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L70: */
			}
		      C2F (dscal) (n, &scaloc, &work[work_dim1 + 1], &c__1);
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		    }
		  /* 
		   */
		}
	      else if (l1 == l2 && k1 != k2)
		{
		  /* 
		   */
		  i__1 = *n - l1;
		  /*Computing MIN 
		   */
		  i__2 = l1 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l1 + 1;
		  work[k1 + work_dim1] =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  i__1 = *n - l1;
		  /*Computing MIN 
		   */
		  i__2 = l1 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l1 + 1;
		  work[k2 + work_dim1] =
		    C2F (ddot) (&i__1,
				&c__[k2 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  p11 =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  p21 =
		    C2F (ddot) (&i__1,
				&a[k2 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  /* 
		   */
		  i__1 = *n - k1 + 1;
		  suml =
		    C2F (ddot) (&i__1, &a[k1 + k1 * a_dim1], lda,
				&work[k1 + work_dim1], &c__1);
		  sumr = p11 * a[l1 + l1 * a_dim1];
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__1 = *n - k1 + 1;
		  suml =
		    C2F (ddot) (&i__1, &a[k2 + k1 * a_dim1], lda,
				&work[k1 + work_dim1], &c__1);
		  sumr = p21 * a[l1 + l1 * a_dim1];
		  vec[1] = c__[k2 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  nsp_ctrlpack_dlasd2 (&c_false, &c_true, &c__1, &c__2, &c__1,
				       &a[k1 + k1 * a_dim1], lda,
				       &a[l1 + l1 * a_dim1], lda, vec, &c__2,
				       &scaloc, x, &c__2, &xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L80: */
			}
		      C2F (dscal) (n, &scaloc, &work[work_dim1 + 1], &c__1);
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l1 + k2 * c_dim1] = x[1];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 == k2)
		{
		  /* 
		   */
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  work[k1 + work_dim1] =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  work[k1 + (work_dim1 << 1)] =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l2 + Min (i__3, *n) * a_dim1], lda);
		  i__1 = *n - k1;
		  /*Computing MIN 
		   */
		  i__2 = k1 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k1 + 1;
		  p11 =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  i__1 = *n - k1;
		  /*Computing MIN 
		   */
		  i__2 = k1 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k1 + 1;
		  p12 =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l2 * c_dim1], &c__1);
		  /* 
		   */
		  i__1 = *n - k1 + 1;
		  suml =
		    C2F (ddot) (&i__1, &a[k1 + k1 * a_dim1], lda,
				&work[k1 + work_dim1], &c__1);
		  sumr =
		    p11 * a[l1 + l1 * a_dim1] + p12 * a[l1 + l2 * a_dim1];
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__1 = *n - k1 + 1;
		  suml =
		    C2F (ddot) (&i__1, &a[k1 + k1 * a_dim1], lda,
				&work[k1 + (work_dim1 << 1)], &c__1);
		  sumr =
		    p11 * a[l2 + l1 * a_dim1] + p12 * a[l2 + l2 * a_dim1];
		  vec[2] = c__[k1 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  nsp_ctrlpack_dlasd2 (&c_false, &c_true, &c__1, &c__1, &c__2,
				       &a[k1 + k1 * a_dim1], lda,
				       &a[l1 + l1 * a_dim1], lda, vec, &c__2,
				       &scaloc, x, &c__2, &xnorm, &ierr);
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L90: */
			}
		      C2F (dscal) (n, &scaloc, &work[work_dim1 + 1], &c__1);
		      C2F (dscal) (n, &scaloc,
				   &work[(work_dim1 << 1) + 1], &c__1);
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[2];
		  c__[l1 + k1 * c_dim1] = x[0];
		  c__[l2 + k1 * c_dim1] = x[2];
		  /* 
		   */
		}
	      else if (l1 != l2 && k1 != k2)
		{
		  /* 
		   */
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  work[k1 + work_dim1] =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  work[k2 + work_dim1] =
		    C2F (ddot) (&i__1,
				&c__[k2 + Min (i__2, *n) * c_dim1],
				ldc, &a[l1 + Min (i__3, *n) * a_dim1], lda);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  work[k1 + (work_dim1 << 1)] =
		    C2F (ddot) (&i__1,
				&c__[k1 + Min (i__2, *n) * c_dim1],
				ldc, &a[l2 + Min (i__3, *n) * a_dim1], lda);
		  i__1 = *n - l2;
		  /*Computing MIN 
		   */
		  i__2 = l2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = l2 + 1;
		  work[k2 + (work_dim1 << 1)] =
		    C2F (ddot) (&i__1,
				&c__[k2 + Min (i__2, *n) * c_dim1],
				ldc, &a[l2 + Min (i__3, *n) * a_dim1], lda);
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  p11 =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  p12 =
		    C2F (ddot) (&i__1,
				&a[k1 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l2 * c_dim1], &c__1);
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  p21 =
		    C2F (ddot) (&i__1,
				&a[k2 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l1 * c_dim1], &c__1);
		  i__1 = *n - k2;
		  /*Computing MIN 
		   */
		  i__2 = k2 + 1;
		  /*Computing MIN 
		   */
		  i__3 = k2 + 1;
		  p22 =
		    C2F (ddot) (&i__1,
				&a[k2 + Min (i__2, *n) * a_dim1], lda,
				&c__[Min (i__3, *n) + l2 * c_dim1], &c__1);
		  /* 
		   */
		  i__1 = *n - k1 + 1;
		  suml =
		    C2F (ddot) (&i__1, &a[k1 + k1 * a_dim1], lda,
				&work[k1 + work_dim1], &c__1);
		  sumr =
		    p11 * a[l1 + l1 * a_dim1] + p12 * a[l1 + l2 * a_dim1];
		  vec[0] = c__[k1 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__1 = *n - k1 + 1;
		  suml =
		    C2F (ddot) (&i__1, &a[k1 + k1 * a_dim1], lda,
				&work[k1 + (work_dim1 << 1)], &c__1);
		  sumr =
		    p11 * a[l2 + l1 * a_dim1] + p12 * a[l2 + l2 * a_dim1];
		  vec[2] = c__[k1 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__1 = *n - k1 + 1;
		  suml =
		    C2F (ddot) (&i__1, &a[k2 + k1 * a_dim1], lda,
				&work[k1 + work_dim1], &c__1);
		  sumr =
		    p21 * a[l1 + l1 * a_dim1] + p22 * a[l1 + l2 * a_dim1];
		  vec[1] = c__[k2 + l1 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  i__1 = *n - k1 + 1;
		  suml =
		    C2F (ddot) (&i__1, &a[k2 + k1 * a_dim1], lda,
				&work[k1 + (work_dim1 << 1)], &c__1);
		  sumr =
		    p21 * a[l2 + l1 * a_dim1] + p22 * a[l2 + l2 * a_dim1];
		  vec[3] = c__[k2 + l2 * c_dim1] - (suml + sumr);
		  /* 
		   */
		  if (k1 == l1)
		    {
		      nsp_ctrlpack_dlald2 (&c_true, &a[k1 + k1 * a_dim1], lda,
					   vec, &c__2, &scaloc, x, &c__2,
					   &xnorm, &ierr);
		    }
		  else
		    {
		      nsp_ctrlpack_dlasd2 (&c_false, &c_true, &c__1, &c__2,
					   &c__2, &a[k1 + k1 * a_dim1], lda,
					   &a[l1 + l1 * a_dim1], lda, vec,
					   &c__2, &scaloc, x, &c__2, &xnorm,
					   &ierr);
		    }
		  if (ierr != 0)
		    {
		      *info = 1;
		    }
		  /* 
		   */
		  if (scaloc != 1.)
		    {
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (n, &scaloc,
				       &c__[j * c_dim1 + 1], &c__1);
			  /* L100: */
			}
		      C2F (dscal) (n, &scaloc, &work[work_dim1 + 1], &c__1);
		      C2F (dscal) (n, &scaloc,
				   &work[(work_dim1 << 1) + 1], &c__1);
		      *scale *= scaloc;
		    }
		  c__[k1 + l1 * c_dim1] = x[0];
		  c__[k1 + l2 * c_dim1] = x[2];
		  c__[k2 + l1 * c_dim1] = x[1];
		  c__[k2 + l2 * c_dim1] = x[3];
		  if (k1 != l1)
		    {
		      c__[l1 + k1 * c_dim1] = x[0];
		      c__[l2 + k1 * c_dim1] = x[2];
		      c__[l1 + k2 * c_dim1] = x[1];
		      c__[l2 + k2 * c_dim1] = x[3];
		    }
		}
	      /* 
	       */
	    L110:
	      ;
	    }
	L120:
	  ;
	}
      /* 
       */
    }
  /* 
   */
  return 0;
  /* 
   *    End of LYPDTR 
   * 
   */
}				/* lypdtr_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * RICCFR estimates the forward error bound for the computed solution of 
 * the matrix algebraic Riccati equation 
 * 
 * transpose(op(A))*X + X*op(A) + C - X*D*X = 0 
 * 
 * where op(A) = A or A**T and C, D are symmetric (C = C**T, D = D**T). 
 * The matrices A, C, D and X are N-by-N. 
 * 
 * Arguments 
 * ========= 
 * 
 * TRANA   (input) CHARACTER*1 
 *         Specifies the option op(A): 
 *         = 'N': op(A) = A    (No transpose) 
 *         = 'T': op(A) = A**T (Transpose) 
 *         = 'C': op(A) = A**T (Conjugate transpose = Transpose) 
 * 
 * N       (input) INT 
 *         The order of the matrix A, and the order of the 
 *         matrices C, D and X. N >= 0. 
 * 
 * A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
 *         The N-by-N matrix A. 
 * 
 * LDA     (input) INT 
 *         The leading dimension of the array A. LDA >= Max(1,N). 
 * 
 * UPLO    (input) CHARACTER*1 
 *         = 'U':  Upper triangles of C and D are stored; 
 *         = 'L':  Lower triangles of C and D are stored. 
 * 
 * C       (input) DOUBLE PRECISION array, dimension (LDC,N) 
 *         If UPLO = 'U', the leading N-by-N upper triangular part of C 
 *         contains the upper triangular part of the matrix C. 
 *         If UPLO = 'L', the leading N-by-N lower triangular part of C 
 *         contains the lower triangular part of the matrix C. 
 * 
 * LDC     (input) INT 
 *         The leading dimension of the array C. LDC >= Max(1,N). 
 * 
 * D       (input) DOUBLE PRECISION array, dimension (LDD,N) 
 *         If UPLO = 'U', the leading N-by-N upper triangular part of D 
 *         contains the upper triangular part of the matrix D. 
 *         If UPLO = 'L', the leading N-by-N lower triangular part of D 
 *         contains the lower triangular part of the matrix D. 
 * 
 * LDD     (input) INT 
 *         The leading dimension of the array D. LDD >= Max(1,N). 
 * 
 * X       (input) DOUBLE PRECISION array, dimension (LDX,N) 
 *         The N-by-N solution matrix X. 
 * 
 * LDX     (input) INT 
 *         The leading dimension of the array X. LDX >= Max(1,N). 
 * 
 * T       (input) DOUBLE PRECISION array, dimension (LDT,N) 
 *         The upper quasi-triangular matrix in Schur canonical form 
 *         from the Schur factorization of the matrix Ac = A - D*X 
 *         (if TRANA = 'N') or Ac = A - X*D (if TRANA = 'T' or 'C'). 
 * 
 * LDT     (input) INT 
 *         The leading dimension of the array T. LDT >= Max(1,N) 
 * 
 * U       (input) DOUBLE PRECISION array, dimension (LDU,N) 
 *         The orthogonal N-by-N matrix from the real Schur 
 *         factorization of the matrix Ac = A - D*X (if TRANA = 'N') 
 *         or Ac = A - X*D (if TRANA = 'T' or 'C'). 
 * 
 * LDU     (input) INT 
 *         The leading dimension of the array U. LDU >= Max(1,N) 
 * 
 * FERR    (output) DOUBLE PRECISION 
 *         The estimated forward error bound for the solution X. 
 *         If XTRUE is the true solution, FERR bounds the magnitude 
 *         of the largest entry in (X - XTRUE) divided by the magnitude 
 *         of the largest entry in X. 
 * 
 * WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK) 
 * 
 * LWORK   INT 
 *         The dimension of the array WORK. LWORK >= 7*N*N 
 * 
 * IWORK   (workspace) INT array, dimension (N*N) 
 * 
 * INFO    INT 
 *         = 0: successful exit 
 *         < 0: if INFO = -i, the i-th argument had an illegal value 
 * 
 * Further details 
 * =============== 
 * 
 * The forward error bound is estimated using a practical error bound 
 * similar to the one proposed in [1]. 
 * 
 * References 
 * ========== 
 * 
 * [1] N.J. Higham. Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * [2] P.Hr. Petkov, M.M. Konstantinov, and V. Mehrmann. DGRSVX and 
 *     DMSRIC: Fortan 77 subroutines for solving continuous-time matrix 
 *     algebraic Riccati equations with condition and accuracy 
 *     estimates. Preprint SFB393/98-16, Fak. f. Mathematik, Tech. Univ. 
 *     Chemnitz, May 1998. 
 */


int
nsp_ctrlpack_riccfr (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *d__, int *ldd, double *x,
		     int *ldx, double *t, int *ldt, double *u, int *ldu,
		     double *ferr, double *work, int *lwork, int *iwork,
		     int *info, long int trana_len, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, d_dim1, d_offset, t_dim1, t_offset,
    u_dim1, u_offset, x_dim1, x_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int idlc, iabs, idbs, kase, ires, ixbs, itmp, info2, i__, j;
  double scale;
  int lower;
  double xnorm;
  int ij;
  char tranat[1];
  int notrna;
  int minwrk;
  double eps, est;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  --work;
  --iwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldd < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -11;
    }
  else if (*ldt < Max (1, *n))
    {
      *info = -13;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -15;
    }
  /* 
   *    Get the machine precision 
   * 
   */
  eps = C2F (dlamch) ("Epsilon", 7L);
  /* 
   *    Compute workspace 
   * 
   */
  minwrk = *n * 7 * *n;
  if (*lwork < minwrk)
    {
      *info = -18;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("RICCFR", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  xnorm = C2F (dlansy) ("1", uplo, n, &x[x_offset], ldx, &work[1], 1L, 1L);
  if (xnorm == 0.)
    {
      *ferr = 0.;
      return 0;
    }
  /* 
   *    Workspace usage 
   * 
   */
  idlc = *n * *n;
  itmp = idlc + *n * *n;
  iabs = itmp + *n * *n;
  idbs = iabs + *n * *n;
  ixbs = idbs + *n * *n;
  ires = ixbs + *n * *n;
  /* 
   */
  if (notrna)
    {
      *(unsigned char *) tranat = 'T';
    }
  else
    {
      *(unsigned char *) tranat = 'N';
    }
  /* 
   *    Form residual matrix R = transpose(op(A))*X + X*op(A) + C - X*D*X 
   * 
   */
  C2F (dlacpy) (uplo, n, n, &c__[c_offset], ldc, &work[ires + 1], n, 1L);
  C2F (dsyr2k) (uplo, tranat, n, n, &c_b82, &a[a_offset], lda,
		&x[x_offset], ldx, &c_b82, &work[ires + 1], n, 1L, 1L);
  C2F (dsymm) ("R", uplo, n, n, &c_b82, &d__[d_offset], ldd,
	       &x[x_offset], ldx, &c_b83, &work[itmp + 1], n, 1L, 1L);
  C2F (dsymm) ("R", uplo, n, n, &c_b79, &x[x_offset], ldx,
	       &work[itmp + 1], n, &c_b82, &work[ires + 1], n, 1L, 1L);
  /* 
   *    Add to Abs(R) a term that takes account of rounding errors in 
   *    forming R: 
   *      Abs(R) := Abs(R) + EPS*(4*abs(C) + (n+4)*(Abs(op(A'))*abs(X) + 
   *                  Abs(X)*abs(op(A))) + 2*(n+1)*abs(X)*abs(D)*abs(X)) 
   *    where EPS is the machine precision 
   * 
   */
  ij = 0;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ++ij;
	  work[iabs + ij] = (d__1 = a[i__ + j * a_dim1], Abs (d__1));
	  work[ixbs + ij] = (d__1 = x[i__ + j * x_dim1], Abs (d__1));
	  /* L10: */
	}
      /* L20: */
    }
  if (lower)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = j; i__ <= i__2; ++i__)
	    {
	      work[itmp + i__ + (j - 1) * *n] = (d__1 =
						 c__[i__ + j * c_dim1],
						 Abs (d__1));
	      work[idbs + i__ + (j - 1) * *n] = (d__1 =
						 d__[i__ + j * d_dim1],
						 Abs (d__1));
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
	  i__2 = j;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      work[itmp + i__ + (j - 1) * *n] = (d__1 =
						 c__[i__ + j * c_dim1],
						 Abs (d__1));
	      work[idbs + i__ + (j - 1) * *n] = (d__1 =
						 d__[i__ + j * d_dim1],
						 Abs (d__1));
	      /* L50: */
	    }
	  /* L60: */
	}
    }
  d__1 = (double) (*n + 4) * eps;
  d__2 = eps * 4.;
  C2F (dsyr2k) (uplo, tranat, n, n, &d__1, &work[iabs + 1], n,
		&work[ixbs + 1], n, &d__2, &work[itmp + 1], n, 1L, 1L);
  C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[idbs + 1], n,
	       &work[ixbs + 1], n, &c_b83, &work[idlc + 1], n, 1L, 1L);
  d__1 = (double) ((*n << 1) + 2) * eps;
  C2F (dsymm) ("R", uplo, n, n, &d__1, &work[ixbs + 1], n,
	       &work[idlc + 1], n, &c_b82, &work[itmp + 1], n, 1L, 1L);
  if (lower)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = j; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] = (d__1 =
						 work[ires + i__ +
						      (j - 1) * *n],
						 Abs (d__1)) + work[itmp +
								    i__ + (j -
									   1)
								    * *n];
	      /* L70: */
	    }
	  /* L80: */
	}
    }
  else
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = j;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] = (d__1 =
						 work[ires + i__ +
						      (j - 1) * *n],
						 Abs (d__1)) + work[itmp +
								    i__ + (j -
									   1)
								    * *n];
	      /* L90: */
	    }
	  /* L100: */
	}
    }
  /* 
   *    Compute forward error bound, using matrix norm estimator 
   * 
   */
  est = 0.;
  kase = 0;
 L110:
  i__1 = *n * (*n + 1) / 2;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] =
			work[ij] * work[ires + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Unpack the lower triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] = work[ij];
		    }
		  /* L120: */
		}
	      /* L130: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] =
			work[ij] * work[ires + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Unpack the upper triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] = work[ij];
		    }
		  /* L140: */
		}
	      /* L150: */
	    }
	}
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      if (kase == 2)
	{
	  /* 
	   *          Solve op(A')*Y + Y*op(A) = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (trana, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(A)*Z + Z*op(A') = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (tranat, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n,
		   &u[u_offset], ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Pack the lower triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[ij] = work[itmp + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[ij] =
			work[itmp + i__ + (j - 1) * *n] * work[ires + i__ +
							       (j - 1) * *n];
		    }
		  /* L160: */
		}
	      /* L170: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Pack the upper triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[ij] = work[itmp + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[ij] =
			work[itmp + i__ + (j - 1) * *n] * work[ires + i__ +
							       (j - 1) * *n];
		    }
		  /* L180: */
		}
	      /* L190: */
	    }
	}
      goto L110;
    }
  /* 
   *    Compute the estimate of the forward error 
   * 
   */
  *ferr =
    est * 2. / C2F (dlansy) ("Max", uplo, n, &x[x_offset], ldx,
			     &work[1], 3L, 1L) / scale;
  if (*ferr > 1.)
    {
      *ferr = 1.;
    }
  /* 
   */
  return 0;
  /* 
   *    End of RICCFR 
   * 
   */
}				/* riccfr_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * RICCMF solves the matrix algebraic Riccati equation 
 * =============== 
 * 
 * The matrix Riccati equation is solved by the inverse free method 
 * proposed in [1]. 
 * 
 * The condition number of the equation is estimated using 1-norm 
 * estimator. 
 * 
 * The forward error bound is estimated using a practical error bound 
 * similar to the one proposed in [2]. 
 * 
 * References 
 * ========== 
 * 
 * [1] Z. Bai and Q. Qian. Inverse free parallel method for the 
 *     numerical solution of algebraic Riccati equations. In J.G. Lewis, 
 *     editor, Proc. Fifth SIAM Conf. on Appl. Lin. Algebra, Snowbird, 
 *     UT, June 1994, pp. 167-171. SIAM, Philadelphia, PA, 1994. 
 * [2] N.J. Higham. Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * [3] P.Hr. Petkov, M.M. Konstantinov, and V. Mehrmann. DGRSVX and 
 *     DMSRIC: Fortan 77 subroutines for solving continuous-time matrix 
 *     algebraic Riccati equations with condition and accuracy 
 *     estimates. Preprint SFB393/98-16, Fak. f. Mathematik, Tech. Univ. 
 *     Chemnitz, May 1998. 
 */

int
nsp_ctrlpack_riccmf (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *d__, int *ldd, double *x,
		     int *ldx, double *wr, double *wi, double *rcond,
		     double *ferr, double *work, int *lwork, int *iwork,
		     int *info, long int trana_len, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, d_dim1, d_offset, x_dim1, x_offset,
    i__1, i__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int iscl, itau, iter;
  double temp;
  int iwrk, info2, i__, j;
  char equed[1];
  double cnorm, dnorm;
  int lower;
  double rnorm=0;
  int n2, n4;
  double cnorm2, dnorm2;
  int ia, ib, ic, ij;
  int iq, ir, is;
  int iu, iv;
  int lwamax;
  int notrna;
  double rdnorm=0;
  int ij1, ij2;
  int minwrk;
  int iaf, ibr, ifr, lwa;
  double eps, tol;
  int lwa0;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --wr;
  --wi;
  --work;
  --iwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldd < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -11;
    }
  /* 
   *    Set tol 
   * 
   */
  eps = C2F (dlamch) ("Epsilon", 7L);
  tol = (double) (*n) * 10. * eps;
  /* 
   *    Compute workspace 
   * 
   *Computing MAX 
   */
  i__1 = 1, i__2 = *n << 1;
  minwrk = *n * 28 * *n + (*n << 1) + Max (i__1, i__2);
  if (*lwork < minwrk)
    {
      *info = -17;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("RICCMF", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Compute the norms of the matrices C and D 
   * 
   */
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  dnorm = C2F (dlansy) ("1", uplo, n, &d__[d_offset], ldd, &work[1], 1L, 1L);
  /* 
   */
  n2 = *n << 1;
  n4 = *n << 2;
  /* 
   *    Construct the Hamiltonian matrix 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = (j - 1) * n2 + i__;
	  if (notrna)
	    {
	      work[ij] = a[i__ + j * a_dim1];
	    }
	  else
	    {
	      work[ij] = a[j + i__ * a_dim1];
	    }
	  ij = (j - 1) * n2 + *n + i__;
	  if (!lower)
	    {
	      if (i__ <= j)
		{
		  work[ij] = -c__[i__ + j * c_dim1];
		}
	      else
		{
		  work[ij] = -c__[j + i__ * c_dim1];
		}
	    }
	  else
	    {
	      if (i__ >= j)
		{
		  work[ij] = -c__[i__ + j * c_dim1];
		}
	      else
		{
		  work[ij] = -c__[j + i__ * c_dim1];
		}
	    }
	  ij = (*n + j - 1) * n2 + i__;
	  if (!lower)
	    {
	      if (i__ <= j)
		{
		  work[ij] = -d__[i__ + j * d_dim1];
		}
	      else
		{
		  work[ij] = -d__[j + i__ * d_dim1];
		}
	    }
	  else
	    {
	      if (i__ >= j)
		{
		  work[ij] = -d__[i__ + j * d_dim1];
		}
	      else
		{
		  work[ij] = -d__[j + i__ * d_dim1];
		}
	    }
	  ij = (*n + j - 1) * n2 + *n + i__;
	  if (notrna)
	    {
	      work[ij] = -a[j + i__ * a_dim1];
	    }
	  else
	    {
	      work[ij] = -a[i__ + j * a_dim1];
	    }
	  /* L10: */
	}
      /* L20: */
    }
  /* 
   *    Scale the Hamiltonian matrix 
   * 
   */
  cnorm2 = sqrt (cnorm);
  dnorm2 = sqrt (dnorm);
  iscl = 0;
  if (cnorm2 > dnorm2 && dnorm2 > 0.)
    {
      C2F (dlascl) ("G", &c__0, &c__0, &cnorm2, &dnorm2, n, n,
		    &work[*n + 1], &n2, &info2, 1L);
      C2F (dlascl) ("G", &c__0, &c__0, &dnorm2, &cnorm2, n, n,
		    &work[n2 * *n + 1], &n2, &info2, 1L);
      iscl = 1;
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa0 = *n * 28 * *n + (*n << 1);
  lwamax = 0;
  ia = n2 * n2;
  ir = ia + n2 * n2;
  is = ir + n4 * n2;
  iq = is + n2 * n2;
  itau = iq + n4 * n2;
  iwrk = itau + n2;
  /* 
   *    Compute B0 and -A0 
   * 
   */
  i__1 = n2;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = n2;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij1 = (j - 1) * n2 + i__;
	  ij2 = ia + (j - 1) * n2 + i__;
	  temp = work[ij1];
	  if (i__ == j)
	    {
	      work[ij1] = temp + 1.;
	      work[ij2] = temp - 1.;
	    }
	  else
	    {
	      work[ij2] = temp;
	    }
	  /* L30: */
	}
      /* L40: */
    }
  C2F (dlacpy) ("F", &n2, &n2, &work[1], &n2, &work[ir + 1], &n4, 1L);
  C2F (dlacpy) ("F", &n2, &n2, &work[ia + 1], &n2, &work[ir + n2 + 1],
		&n4, 1L);
  /* 
   *    Main iteration loop 
   * 
   */
  for (iter = 1; iter <= 50; ++iter)
    {
      /* 
       *                           [ Bj] 
       *       QR decomposition of [   ] 
       *                           [-Aj] 
       * 
       */
      i__1 = *lwork - iwrk;
      C2F (dgeqrf) (&n4, &n2, &work[ir + 1], &n4, &work[itau + 1],
		    &work[iwrk + 1], &i__1, &info2);
      lwa = lwa0 + (int) work[iwrk + 1];
      lwamax = Max (lwa, lwamax);
      /* 
       *       Make the diagonal elements of Rj positive 
       * 
       */
      i__1 = n2;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (work[ir + (i__ - 1) * n4 + i__] < 0.)
	    {
	      i__2 = n2 - i__ + 1;
	      C2F (dscal) (&i__2, &c_b79,
			   &work[ir + (i__ - 1) * n4 + i__], &n4);
	    }
	  /* L50: */
	}
      if (iter > 1)
	{
	  /* 
	   *          Compute Rj+1 - Rj 
	   * 
	   */
	  i__1 = n2;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ij1 = ir + (j - 1) * n4 + i__;
		  ij2 = is + (j - 1) * n2 + i__;
		  work[ij2] = work[ij1] - work[ij2];
		  /* L60: */
		}
	      /* L70: */
	    }
	  rdnorm =
	    C2F (dlange) ("1", &n2, &n2, &work[is + 1], &n2,
			  &work[iwrk + 1], 1L);
	}
      /* 
       *       Save Rj for future use 
       * 
       */
      C2F (dlacpy) ("U", &n2, &n2, &work[ir + 1], &n4, &work[is + 1],
		    &n2, 1L);
      if (iter == 1)
	{
	  i__1 = n2 - 1;
	  i__2 = n2 - 1;
	  C2F (dlaset) ("L", &i__1, &i__2, &c_b83, &c_b83,
			&work[is + 2], &n2, 1L);
	}
      /* 
       *       Generate the matrices Q12 and Q22 
       * 
       */
      C2F (dlaset) ("F", &n2, &n2, &c_b83, &c_b83, &work[iq + 1], &n4, 1L);
      C2F (dlaset) ("F", &n2, &n2, &c_b83, &c_b82, &work[iq + n2 + 1],
		    &n4, 1L);
      i__1 = *lwork - iwrk;
      C2F (dormqr) ("L", "N", &n4, &n2, &n2, &work[ir + 1], &n4,
		    &work[itau + 1], &work[iq + 1], &n4,
		    &work[iwrk + 1], &i__1, &info2, 1L, 1L);
      lwa = lwa0 + (int) work[iwrk + 1];
      lwamax = Max (lwa, lwamax);
      /* 
       *       Compute Bj and -Aj 
       * 
       */
      C2F (dgemm) ("T", "N", &n2, &n2, &n2, &c_b82, &work[iq + n2 + 1],
		   &n4, &work[1], &n2, &c_b83, &work[ir + 1], &n4, 1L, 1L);
      C2F (dgemm) ("T", "N", &n2, &n2, &n2, &c_b82, &work[iq + 1], &n4,
		   &work[ia + 1], &n2, &c_b83, &work[ir + n2 + 1], &n4,
		   1L, 1L);
      C2F (dlacpy) ("F", &n2, &n2, &work[ir + 1], &n4, &work[1], &n2, 1L);
      C2F (dlacpy) ("F", &n2, &n2, &work[ir + n2 + 1], &n4,
		    &work[ia + 1], &n2, 1L);
      /* 
       *       Test for convergence 
       * 
       */
      if (iter > 1 && rdnorm <= tol * rnorm)
	{
	  goto L90;
	}
      rnorm =
	C2F (dlange) ("1", &n2, &n2, &work[is + 1], &n2, &work[iwrk + 1], 1L);
      /* L80: */
    }
  *info = 1;
 L90:
  lwa0 = *n * 10 * *n + (*n << 1);
  iq = ia + n2 * n2;
  itau = iq + n2 * *n;
  iwrk = itau + n2;
  /* 
   *    Compute Ap + Bp 
   * 
   */
  i__1 = n2 * n2;
  C2F (dscal) (&i__1, &c_b79, &work[ia + 1], &c__1);
  i__1 = n2 * n2;
  C2F (daxpy) (&i__1, &c_b82, &work[ia + 1], &c__1, &work[1], &c__1);
  /* 
   *    QR decomposition with column pivoting of Ap 
   * 
   */
  i__1 = n2;
  for (j = 1; j <= i__1; ++j)
    {
      iwork[j] = 0;
      /* L100: */
    }
  i__1 = *lwork - iwrk;
  C2F (dgeqp3) (&n2, &n2, &work[ia + 1], &n2, &iwork[1],
		&work[itau + 1], &work[iwrk + 1], &i__1, &info2);
  lwa = lwa0 + (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *              T 
   *    Compute Q1 (Ap + Bp) 
   * 
   */
  i__1 = *lwork - iwrk;
  C2F (dormqr) ("L", "T", &n2, &n2, &n2, &work[ia + 1], &n2,
		&work[itau + 1], &work[1], &n2, &work[iwrk + 1], &i__1,
		&info2, 1L, 1L);
  lwa = lwa0 + (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *                          T 
   *    RQ decomposition of Q1 (Ap + Bp) 
   * 
   */
  i__1 = *lwork - iwrk;
  C2F (dgerqf) (&n2, &n2, &work[1], &n2, &work[itau + 1],
		&work[iwrk + 1], &i__1, &info2);
  lwa = lwa0 + (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Generate Q11 and Q21 
   * 
   */
  C2F (dlaset) ("F", n, n, &c_b83, &c_b82, &work[iq + 1], &n2, 1L);
  C2F (dlaset) ("F", n, n, &c_b83, &c_b83, &work[iq + *n + 1], &n2, 1L);
  i__1 = *lwork - iwrk;
  C2F (dormrq) ("L", "T", &n2, n, &n2, &work[1], &n2, &work[itau + 1],
		&work[iq + 1], &n2, &work[iwrk + 1], &i__1, &info2, 1L, 1L);
  lwa = lwa0 + (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Store the matrices Q11 and Q21 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = (j - 1) * *n + i__;
	  iv = iq + (i__ - 1) * n2 + j;
	  work[ij] = work[iv];
	  ij = (j - 1) * *n + (*n << 1) * *n + i__;
	  iv = iq + (i__ - 1) * n2 + *n + j;
	  work[ij] = work[iv];
	  /* L110: */
	}
      /* L120: */
    }
  /* 
   *    Workspace usage 
   * 
   */
  iaf = *n * *n;
  ib = iaf + *n * *n;
  ir = ib + *n * *n;
  ic = ir + *n;
  ifr = ic + *n;
  ibr = ifr + *n;
  iwrk = ibr + *n;
  /* 
   *    Compute the solution matrix X 
   * 
   */
  C2F (dgesvx) ("E", "N", n, n, &work[1], n, &work[iaf + 1], n,
		&iwork[1], equed, &work[ir + 1], &work[ic + 1],
		&work[ib + 1], n, &x[x_offset], ldx, rcond,
		&work[ifr + 1], &work[ibr + 1], &work[iwrk + 1],
		&iwork[*n + 1], &info2, 1L, 1L, 1L);
  if (info2 > 0)
    {
      *info = 2;
      return 0;
    }
  /* 
   *    Symmetrize the solution 
   * 
   */
  if (*n > 1)
    {
      i__1 = *n - 1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  i__2 = *n;
	  for (j = i__ + 1; j <= i__2; ++j)
	    {
	      temp = (x[i__ + j * x_dim1] + x[j + i__ * x_dim1]) / 2.;
	      x[i__ + j * x_dim1] = temp;
	      x[j + i__ * x_dim1] = temp;
	      /* L130: */
	    }
	  /* L140: */
	}
    }
  /* 
   *    Undo scaling for the solution matrix 
   * 
   */
  if (iscl == 1)
    {
      C2F (dlascl) ("G", &c__0, &c__0, &dnorm2, &cnorm2, n, n,
		    &x[x_offset], ldx, &info2, 1L);
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa = (*n << 1) * *n;
  iu = *n * *n;
  iwrk = iu + *n * *n;
  /* 
   *    Estimate the reciprocal condition number 
   * 
   */
  i__1 = *lwork - iwrk;
  nsp_ctrlpack_riccrc (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &d__[d_offset], ldd, &x[x_offset], ldx, rcond,
		       &work[1], n, &work[iu + 1], n, &wr[1], &wi[1],
		       &work[iwrk + 1], &i__1, &iwork[1], &info2, 1L, 1L);
  if (info2 > 0)
    {
      *info = 3;
      return 0;
    }
  lwa += (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (*rcond == 0.)
    {
      *ferr = 1.;
      work[1] = (double) lwamax;
      return 0;
    }
  /* 
   *    Estimate the bound on the forward error 
   * 
   */
  i__1 = *lwork - iwrk;
  nsp_ctrlpack_riccfr (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &d__[d_offset], ldd, &x[x_offset], ldx, &work[1], n,
		       &work[iu + 1], n, ferr, &work[iwrk + 1], &i__1,
		       &iwork[1], &info2, 1L, 1L);
  lwa = *n * 9 * *n;
  lwamax = Max (lwa, lwamax);
  work[1] = (double) lwamax;
  return 0;
  /* 
   *    End of RICCMF 
   * 
   */
}				/* riccmf_ */


/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * RICCMS solves the matrix algebraic Riccati equation 
 *              error bounds could not be computed 
 *         = 4: the matrix A-D*X (or A-X*D) can not be reduced to Schur 
 *              canonical form and condition number estimate and 
 *              forward error estimate have not been computed. 
 * 
 * Further Details 
 * =============== 
 * 
 * The Riccati equation is solved by the matrix sign function approach 
 * [1], [2] implementing a scaling which enhances the numerical 
 * stability [4]. 
 * 
 * The condition number of the equation is estimated using 1-norm 
 * condition estimator. 
 * 
 * The forward error bound is estimated using a practical error bound 
 * similar to the one proposed in [3]. 
 * 
 * References 
 * ========== 
 * 
 * [1] Z. Bai, J. Demmel, J. Dongarra, A. Petitet, H. Robinson, and 
 *     K. Stanley. The spectral decomposition of nonsymmetric matrices 
 *     on distributed memory parallel computers. SIAM J. Sci. Comput., 
 *     vol. 18, pp. 1446-1461, 1997. 
 * [2] R. Byers, C. He, and V. Mehrmann. The matrix sign function method 
 *     and the computation of invariant subspaces. SIAM J. Matrix Anal. 
 *     Appl., vol. 18, pp. 615-632, 1997. 
 * [3] N.J. Higham. Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * [4] P.Hr. Petkov, M.M. Konstantinov, and V. Mehrmann. DGRSVX and 
 *     DMSRIC: Fortan 77 subroutines for solving continuous-time matrix 
 *     algebraic Riccati equations with condition and accuracy 
 *     estimates. Preprint SFB393/98-16, Fak. f. Mathematik, Tech. Univ. 
 *     Chemnitz, May 1998. 
 */

int
nsp_ctrlpack_riccms (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *d__, int *ldd, double *x,
		     int *ldx, double *wr, double *wi, double *rcond,
		     double *ferr, double *work, int *lwork, int *iwork,
		     int *info, long int trana_len, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, d_dim1, d_offset, x_dim1, x_offset,
    i__1, i__2;
  double d__1;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int iscl, itau, iter;
  double conv, temp;
  int iwrk, info2, i__, j;
  double scale;
  char equed[1];
  double cnorm, dnorm;
  double hnorm;
  int lower;
  int n2;
  double cnorm2, dnorm2;
  int ib, ic, ij, ji;
  int ir, iu, iv;
  int lwamax;
  double hinnrm;
  int notrna;
  int ij1, ij2;
  int minwrk;
  int iaf, ibr, ifr, lwa;
  double eps, tol;
  int ivs;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --wr;
  --wi;
  --work;
  --iwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldd < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -11;
    }
  /* 
   *    Set tol 
   * 
   */
  eps = C2F (dlamch) ("Epsilon", 7L);
  tol = (double) (*n) * 10. * eps;
  /* 
   *    Compute workspace 
   * 
   */
  minwrk = *n * 9 * *n + *n * 7 + 1;
  if (*lwork < minwrk)
    {
      *info = -17;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("RICCMS", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Compute the norms of the matrices C and D 
   * 
   */
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  dnorm = C2F (dlansy) ("1", uplo, n, &d__[d_offset], ldd, &work[1], 1L, 1L);
  /* 
   */
  n2 = *n << 1;
  /* 
   *    Construct the block-permuted Hamiltonian matrix 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  if (!lower)
	    {
	      ij = (*n + j - 1) * n2 + i__;
	      if (notrna)
		{
		  work[ij] = -a[j + i__ * a_dim1];
		}
	      else
		{
		  work[ij] = -a[i__ + j * a_dim1];
		}
	    }
	  else
	    {
	      ij = (j - 1) * n2 + *n + i__;
	      if (notrna)
		{
		  work[ij] = -a[i__ + j * a_dim1];
		}
	      else
		{
		  work[ij] = -a[j + i__ * a_dim1];
		}
	    }
	  ij = (j - 1) * n2 + i__;
	  if (!lower)
	    {
	      if (i__ <= j)
		{
		  work[ij] = -c__[i__ + j * c_dim1];
		}
	    }
	  else
	    {
	      if (i__ >= j)
		{
		  work[ij] = -c__[i__ + j * c_dim1];
		}
	    }
	  ij = (*n + j - 1) * n2 + *n + i__;
	  if (!lower)
	    {
	      if (i__ <= j)
		{
		  work[ij] = d__[i__ + j * d_dim1];
		}
	    }
	  else
	    {
	      if (i__ >= j)
		{
		  work[ij] = d__[i__ + j * d_dim1];
		}
	    }
	  /* L10: */
	}
      /* L20: */
    }
  /* 
   *    Block-scaling 
   * 
   */
  cnorm2 = sqrt (cnorm);
  dnorm2 = sqrt (dnorm);
  iscl = 0;
  if (cnorm2 > dnorm2 && dnorm2 > 0.)
    {
      C2F (dlascl) (uplo, &c__0, &c__0, &cnorm2, &dnorm2, n, n,
		    &work[1], &n2, &info2, 1L);
      C2F (dlascl) (uplo, &c__0, &c__0, &dnorm2, &cnorm2, n, n,
		    &work[n2 * *n + *n + 1], &n2, &info2, 1L);
      iscl = 1;
    }
  /* 
   *    Workspace usage 
   * 
   */
  ivs = n2 * n2;
  itau = ivs + n2 * n2;
  iwrk = itau + n2;
  /* 
   *    Compute the matrix sign function 
   * 
   */
  i__1 = n2 * n2;
  C2F (dcopy) (&i__1, &work[1], &c__1, &work[ivs + 1], &c__1);
  lwamax = 0;
  /* 
   */
  for (iter = 1; iter <= 50; ++iter)
    {
      /* 
       *       Store the norm of the Hamiltonian matrix 
       * 
       */
      hnorm = C2F (dlansy) ("F", uplo, &n2, &work[1], &n2, &work[1], 1L, 1L);
      /* 
       *       Compute the inverse of the block-permuted Hamiltonian matrix 
       * 
       */
      i__1 = *lwork - iwrk;
      C2F (dsytrf) (uplo, &n2, &work[ivs + 1], &n2, &iwork[1],
		    &work[iwrk + 1], &i__1, &info2, 1L);
      if (info2 > 0)
	{
	  *info = 1;
	  return 0;
	}
      lwa = (int) work[iwrk + 1];
      lwamax = Max (lwa, lwamax);
      C2F (dsytri) (uplo, &n2, &work[ivs + 1], &n2, &iwork[1],
		    &work[iwrk + 1], &info2, 1L);
      /* 
       *       Block-permutation of the inverse matrix 
       * 
       */
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      ij1 = ivs + (j - 1) * n2 + i__;
	      ij2 = ivs + (*n + j - 1) * n2 + *n + i__;
	      if (!lower)
		{
		  if (i__ <= j)
		    {
		      temp = work[ij1];
		      work[ij1] = -work[ij2];
		      work[ij2] = -temp;
		    }
		}
	      else
		{
		  if (i__ >= j)
		    {
		      temp = work[ij1];
		      work[ij1] = -work[ij2];
		      work[ij2] = -temp;
		    }
		}
	      if (!lower)
		{
		  if (i__ < j)
		    {
		      ij1 = ivs + (*n + j - 1) * n2 + i__;
		      ij2 = ivs + (*n + i__ - 1) * n2 + j;
		      temp = work[ij1];
		      work[ij1] = work[ij2];
		      work[ij2] = temp;
		    }
		}
	      else
		{
		  if (i__ > j)
		    {
		      ij1 = ivs + (j - 1) * n2 + *n + i__;
		      ij2 = ivs + (i__ - 1) * n2 + *n + j;
		      temp = work[ij1];
		      work[ij1] = work[ij2];
		      work[ij2] = temp;
		    }
		}
	      /* L30: */
	    }
	  /* L40: */
	}
      /* 
       *       Scale the Hamiltonian matrix and its inverse 
       * 
       */
      hinnrm =
	C2F (dlansy) ("F", uplo, &n2, &work[ivs + 1], &n2, &work[1], 1L, 1L);
      scale = sqrt (hinnrm / hnorm);
      i__1 = n2 * n2;
      d__1 = 1. / scale;
      C2F (dscal) (&i__1, &d__1, &work[ivs + 1], &c__1);
      /* 
       *       Compute the next iteration 
       * 
       */
      i__1 = n2 * n2;
      C2F (daxpy) (&i__1, &scale, &work[1], &c__1, &work[ivs + 1], &c__1);
      i__1 = n2 * n2;
      C2F (dscal) (&i__1, &c_b806, &work[ivs + 1], &c__1);
      i__1 = n2 * n2;
      C2F (daxpy) (&i__1, &c_b79, &work[ivs + 1], &c__1, &work[1], &c__1);
      /* 
       *       Test for convergence 
       * 
       */
      conv = C2F (dlansy) ("F", uplo, &n2, &work[1], &n2, &work[1], 1L, 1L);
      if (conv <= tol * hnorm)
	{
	  goto L60;
	}
      i__1 = n2 * n2;
      C2F (dcopy) (&i__1, &work[ivs + 1], &c__1, &work[1], &c__1);
      /* L50: */
    }
  if (conv > tol * hnorm)
    {
      *info = 2;
    }
 L60:
  i__1 = n2;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = n2;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = ivs + (j - 1) * n2 + i__;
	  ji = ivs + (i__ - 1) * n2 + j;
	  if (!lower)
	    {
	      if (i__ < j)
		{
		  work[ji] = work[ij];
		}
	    }
	  else
	    {
	      if (i__ > j)
		{
		  work[ji] = work[ij];
		}
	    }
	  /* L70: */
	}
      /* L80: */
    }
  /* 
   *    Back block-permutation 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij1 = ivs + (j - 1) * n2 + i__;
	  ij2 = ivs + (j - 1) * n2 + *n + i__;
	  temp = work[ij1];
	  work[ij1] = -work[ij2];
	  work[ij2] = temp;
	  ij1 = ivs + (*n + j - 1) * n2 + i__;
	  ij2 = ivs + (*n + j - 1) * n2 + *n + i__;
	  temp = work[ij1];
	  work[ij1] = -work[ij2];
	  work[ij2] = temp;
	  /* L90: */
	}
      /* L100: */
    }
  /* 
   *    Compute the QR decomposition of the projector onto the stable 
   *    invariant subspace 
   * 
   */
  C2F (dlaset) ("F", &n2, &n2, &c_b83, &c_b82, &work[1], &n2, 1L);
  i__1 = n2 * n2;
  C2F (daxpy) (&i__1, &c_b79, &work[ivs + 1], &c__1, &work[1], &c__1);
  i__1 = n2 * n2;
  C2F (dscal) (&i__1, &c_b806, &work[1], &c__1);
  i__1 = n2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      iwork[i__] = 0;
      /* L110: */
    }
  i__1 = *lwork - iwrk;
  C2F (dgeqp3) (&n2, &n2, &work[1], &n2, &iwork[1], &work[itau + 1],
		&work[iwrk + 1], &i__1, &info2);
  lwa = (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Accumulate the orthogonal transformations 
   * 
   */
  C2F (dlaset) ("F", &n2, n, &c_b83, &c_b82, &work[ivs + 1], &n2, 1L);
  i__1 = *lwork - iwrk;
  C2F (dormqr) ("L", "N", &n2, n, n, &work[1], &n2, &work[itau + 1],
		&work[ivs + 1], &n2, &work[iwrk + 1], &i__1, &info2, 1L, 1L);
  lwa = (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Store the matrices V11 and V21 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = (j - 1) * *n + i__;
	  iv = (i__ - 1) * n2 + ivs + j;
	  work[ij] = work[iv];
	  ij = (j - 1) * *n + (*n << 1) * *n + i__;
	  iv = (i__ - 1) * n2 + ivs + *n + j;
	  work[ij] = work[iv];
	  /* L120: */
	}
      /* L130: */
    }
  /* 
   *    Workspace usage 
   * 
   */
  iaf = *n * *n;
  ib = iaf + *n * *n;
  ir = ib + *n * *n;
  ic = ir + *n;
  ifr = ic + *n;
  ibr = ifr + *n;
  iwrk = ibr + *n;
  /* 
   *    Compute the solution matrix X 
   * 
   */
  C2F (dgesvx) ("E", "N", n, n, &work[1], n, &work[iaf + 1], n,
		&iwork[1], equed, &work[ir + 1], &work[ic + 1],
		&work[ib + 1], n, &x[x_offset], ldx, rcond,
		&work[ifr + 1], &work[ibr + 1], &work[iwrk + 1],
		&iwork[*n + 1], &info2, 1L, 1L, 1L);
  if (info2 > 0)
    {
      *info = 3;
      return 0;
    }
  /* 
   *    Symmetrize the solution 
   * 
   */
  if (*n > 1)
    {
      i__1 = *n - 1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  i__2 = *n;
	  for (j = i__ + 1; j <= i__2; ++j)
	    {
	      temp = (x[i__ + j * x_dim1] + x[j + i__ * x_dim1]) / 2.;
	      x[i__ + j * x_dim1] = temp;
	      x[j + i__ * x_dim1] = temp;
	      /* L140: */
	    }
	  /* L150: */
	}
    }
  /* 
   *    Undo scaling for the solution matrix 
   * 
   */
  if (iscl == 1)
    {
      C2F (dlascl) ("G", &c__0, &c__0, &dnorm2, &cnorm2, n, n,
		    &x[x_offset], ldx, &info2, 1L);
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa = (*n << 1) * *n;
  iu = *n * *n;
  iwrk = iu + *n * *n;
  /* 
   *    Estimate the reciprocal condition number 
   * 
   */
  i__1 = *lwork - iwrk;
  nsp_ctrlpack_riccrc (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &d__[d_offset], ldd, &x[x_offset], ldx, rcond,
		       &work[1], n, &work[iu + 1], n, &wr[1], &wi[1],
		       &work[iwrk + 1], &i__1, &iwork[1], &info2, 1L, 1L);
  if (info2 > 0)
    {
      *info = 5;
      return 0;
    }
  lwa += (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (*rcond == 0.)
    {
      *ferr = 1.;
      work[1] = (double) lwamax;
      return 0;
    }
  /* 
   *    Estimate the bound on the forward error 
   * 
   */
  i__1 = *lwork - iwrk;
  nsp_ctrlpack_riccfr (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &d__[d_offset], ldd, &x[x_offset], ldx, &work[1], n,
		       &work[iu + 1], n, ferr, &work[iwrk + 1], &i__1,
		       &iwork[1], &info2, 1L, 1L);
  lwa = *n * 9 * *n;
  lwamax = Max (lwa, lwamax);
  work[1] = (double) lwamax;
  return 0;
  /* 
   *    End of RICCMS 
   * 
   */
}				/* riccms_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * RICCRC estimates the reciprocal of the condition number of the matrix 
 * Omega(Z) = transpose(op(Ac))*Z + Z*op(Ac), 
 * Theta(Z) = inv(Omega(transpose(op(Z))*X + X*op(Z))), 
 * Pi(Z) = inv(Omega(X*Z*X)) 
 * 
 * and Ac = A - D*X (if TRANA = 'N') or Ac = A - X*D (if TRANA = 'T' or 
 * 'C'). 
 * 
 * The program estimates the quantities 
 * 
 * sep(op(Ac),-transpose(op(Ac)) = 1 / norm(inv(Omega)), 
 * 
 * norm(Theta) and norm(Pi) using 1-norm condition estimator. 
 * 
 * References 
 * ========== 
 * 
 * [1] A.R. Ghavimi and A.J. Laub. Backward error, sensitivity, and 
 *     refinment of computed solutions of algebraic Riccati equations. 
 *     Numerical Linear Algebra with Applications, vol. 2, pp. 29-49, 
 *     1995. 
 * [2] P.Hr. Petkov, M.M. Konstantinov, and V. Mehrmann. DGRSVX and 
 *     DMSRIC: Fortan 77 subroutines for solving continuous-time matrix 
 *     algebraic Riccati equations with condition and accuracy 
 *     estimates. Preprint SFB393/98-16, Fak. f. Mathematik, Tech. Univ. 
 *     Chemnitz, May 1998. 
 */

int
nsp_ctrlpack_riccrc (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *d__, int *ldd, double *x,
		     int *ldx, double *rcond, double *t, int *ldt, double *u,
		     int *ldu, double *wr, double *wi, double *work,
		     int *lwork, int *iwork, int *info, long int trana_len,
		     long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, d_dim1, d_offset, t_dim1, t_offset,
    u_dim1, u_offset, x_dim1, x_offset, i__1, i__2;

  /* Local variables */
  int idlc, kase, sdim, itmp, iwrk, info2, i__, j;
  double scale;
  double anorm, cnorm, dnorm;
  int bwork[1], lower;
  double xnorm;
  int ij;
  char tranat[1];
  int notrna;
  double pinorm;
  int minwrk;
  double thnorm;
  int lwa;
  double sep, est;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  --wr;
  --wi;
  --work;
  --iwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldd < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -11;
    }
  else if (*ldt < Max (1, *n))
    {
      *info = -14;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -16;
    }
  /* 
   *    Compute workspace 
   * 
   *Computing MAX 
   */
  i__1 = 1, i__2 = *n * 3;
  minwrk = *n * 3 * *n + Max (i__1, i__2);
  if (*lwork < minwrk)
    {
      *info = -20;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("RICCRC", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  xnorm = C2F (dlansy) ("1", uplo, n, &x[x_offset], ldx, &work[1], 1L, 1L);
  if (xnorm == 0.)
    {
      *rcond = 0.;
      return 0;
    }
  /* 
   *    Compute the norms of the matrices A, C and D 
   * 
   */
  anorm = C2F (dlange) ("1", n, n, &a[a_offset], lda, &work[1], 1L);
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  dnorm = C2F (dlansy) ("1", uplo, n, &d__[d_offset], ldd, &work[1], 1L, 1L);
  /* 
   *    Workspace usage 
   * 
   */
  lwa = *n * 3 * *n;
  idlc = *n * *n;
  itmp = idlc + *n * *n;
  iwrk = itmp + *n * *n;
  /* 
   */
  C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &t[t_offset], ldt, 4L);
  if (notrna)
    {
      /* 
       *       Compute Ac = A - D*X 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b79, &d__[d_offset], ldd,
		   &x[x_offset], ldx, &c_b82, &t[t_offset], ldt, 1L, 1L);
    }
  else
    {
      /* 
       *       Compute Ac = A - X*D 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b79, &d__[d_offset], ldd,
		   &x[x_offset], ldx, &c_b82, &t[t_offset], ldt, 1L, 1L);
    }
  /* 
   *    Compute the Schur factorization of Ac 
   * 
   */
  i__1 = *lwork - iwrk;
  C2F (dgees) ("V", "N", nsp_ctrlpack_voiddummy, n, &t[t_offset], ldt,
	       &sdim, &wr[1], &wi[1], &u[u_offset], ldu,
	       &work[iwrk + 1], &i__1, bwork, &info2, 1L, 1L);
  if (info2 > 0)
    {
      *info = 1;
      return 0;
    }
  lwa += (int) work[iwrk + 1];
  /* 
   *    Estimate sep(op(Ac),-transpose(Ac)) 
   * 
   */
  if (notrna)
    {
      *(unsigned char *) tranat = 'T';
    }
  else
    {
      *(unsigned char *) tranat = 'N';
    }
  /* 
   */
  est = 0.;
  kase = 0;
 L10:
  i__1 = *n * (*n + 1) / 2;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      /* 
       *       Unpack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L20: */
		}
	      /* L30: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L40: */
		}
	      /* L50: */
	    }
	}
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      if (kase == 1)
	{
	  /* 
	   *          Solve op(A')*Y + Y*op(A) = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (trana, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(A)*Z + Z*op(A') = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (tranat, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n,
		   &u[u_offset], ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      /* 
       *       Pack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L60: */
		}
	      /* L70: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L80: */
		}
	      /* L90: */
	    }
	}
      goto L10;
    }
  /* 
   */
  sep = scale / 2. / est;
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (sep == 0.)
    {
      *rcond = 0.;
      return 0;
    }
  /* 
   *    Estimate norm(Theta) 
   * 
   */
  est = 0.;
  kase = 0;
 L100:
  i__1 = *n * *n;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      /* 
       *       Compute RHS = op(W')*X + X*op(W) 
       * 
       */
      C2F (dsyr2k) (uplo, tranat, n, n, &c_b82, &work[1], n,
		    &x[x_offset], ldx, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dlacpy) (uplo, n, n, &work[itmp + 1], n, &work[1], n, 1L);
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[1], n, &u[u_offset],
		   ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[itmp + 1], n, &c_b83, &work[1], n, 1L, 1L);
      if (kase == 1)
	{
	  /* 
	   *          Solve op(Ac')*Y + Y*op(Ac) = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (trana, n, &t[t_offset], ldt, &work[1], n,
			       &scale, &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(Ac)*Z + Z*op(Ac') = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (tranat, n, &t[t_offset], ldt, &work[1], n,
			       &scale, &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[1], n, &u[u_offset],
		   ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      goto L100;
    }
  /* 
   */
  thnorm = est / scale;
  /* 
   *    Estimate norm(Pi) 
   * 
   */
  est = 0.;
  kase = 0;
 L110:
  i__1 = *n * (*n + 1) / 2;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      /* 
       *       Unpack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L120: */
		}
	      /* L130: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L140: */
		}
	      /* L150: */
	    }
	}
      /* 
       *       Compute RHS = X*W*X 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &x[x_offset], ldx, &c_b83, &work[1], n, 1L, 1L);
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &x[x_offset], ldx,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      if (kase == 1)
	{
	  /* 
	   *          Solve op(Ac')*Y + Y*op(Ac) = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (trana, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(Ac)*Z + Z*op(Ac') = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypctr (tranat, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' . 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n,
		   &u[u_offset], ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      /* 
       *       Pack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L160: */
		}
	      /* L170: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L180: */
		}
	      /* L190: */
	    }
	}
      goto L110;
    }
  /* 
   */
  pinorm = est * 2. / scale;
  /* 
   *    Estimate the reciprocal condition number 
   * 
   */
  *rcond = sep * xnorm / (cnorm + sep * (thnorm * anorm + pinorm * dnorm));
  if (*rcond > 1.)
    {
      *rcond = 1.;
    }
  /* 
   */
  work[1] = (double) lwa;
  return 0;
  /* 
   *    End of RICCRC 
   * 
   */
}				/* riccrc_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * RICCSL solves the matrix algebraic Riccati equation 
 *         = 4: the Hamiltonian matrix has less than N eigenvalues 
 *              with negative real parts 
 *         = 5: the system of linear equations for the solution is 
 *              singular to working precision 
 *         = 6: the matrix A-D*X (or A-X*D) can not be reduced to Schur 
 *              canonical form and condition number estimate and 
 *              forward error estimate are not computed 
 * 
 * Further Details 
 * =============== 
 * 
 * The matrix Riccati equation is solved by the Schur method [1]. 
 * 
 * The condition number of the equation is estimated using 1-norm 
 * estimator. 
 * 
 * The forward error bound is estimated using a practical error bound 
 * similar to the one proposed in [3]. 
 * 
 * References 
 * ========== 
 * 
 * [1] A.J. Laub. A Schur method for solving algebraic Riccati 
 *     equations. IEEE Trans. Autom. Control, vol. 24, pp. 913-921, 
 *     1979. 
 * [2] A.R. Ghavimi and A.J. Laub. Backward error, sensitivity, and 
 *     refinment of computed solutions of algebraic Riccati equations. 
 *     Numerical Linear Algebra with Applications, vol. 2, pp. 29-49, 
 *     1995. 
 * [3] N.J. Higham. Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * [4] P.Hr. Petkov, M.M. Konstantinov, and V. Mehrmann. DGRSVX and 
 *     DMSRIC: Fortan 77 subroutines for solving continuous-time matrix 
 *     algebraic Riccati equations with condition and accuracy 
 *     estimates. Preprint SFB393/98-16, Fak. f. Mathematik, Tech. Univ. 
 *     Chemnitz, May 1998. 
 */

int
nsp_ctrlpack_riccsl (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *d__, int *ldd, double *x,
		     int *ldx, double *wr, double *wi, double *rcond,
		     double *ferr, double *work, int *lwork, int *iwork,
		     int *bwork, int *info, long int trana_len,
		     long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, d_dim1, d_offset, x_dim1, x_offset,
    i__1, i__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int iscl, sdim;
  double temp;
  int iwrk, info2, i__, j;
  char equed[1];
  double cnorm, dnorm;
  int lower;
  int n2;
  double cnorm2, dnorm2;
  int ib, ic, ij, ir, iu, iv;
  int lwamax;
  int notrna;
  int minwrk, iaf, ibr, ifr, lwa, iwi, ivs, iwr;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --wr;
  --wi;
  --work;
  --iwork;
  --bwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldd < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -11;
    }
  /* 
   *    Compute workspace 
   * 
   *Computing MAX 
   */
  i__1 = 1, i__2 = *n * 6;
  minwrk = *n * 9 * *n + (*n << 2) + Max (i__1, i__2);
  if (*lwork < minwrk)
    {
      *info = -17;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("RICCSL", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Compute the norms of the matrices C and D 
   * 
   */
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  dnorm = C2F (dlansy) ("1", uplo, n, &d__[d_offset], ldd, &work[1], 1L, 1L);
  /* 
   */
  n2 = *n << 1;
  /* 
   *    Construct the Hamiltonian matrix 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = (j - 1) * n2 + i__;
	  if (notrna)
	    {
	      work[ij] = a[i__ + j * a_dim1];
	    }
	  else
	    {
	      work[ij] = a[j + i__ * a_dim1];
	    }
	  ij = (j - 1) * n2 + *n + i__;
	  if (!lower)
	    {
	      if (i__ <= j)
		{
		  work[ij] = -c__[i__ + j * c_dim1];
		}
	      else
		{
		  work[ij] = -c__[j + i__ * c_dim1];
		}
	    }
	  else
	    {
	      if (i__ >= j)
		{
		  work[ij] = -c__[i__ + j * c_dim1];
		}
	      else
		{
		  work[ij] = -c__[j + i__ * c_dim1];
		}
	    }
	  ij = (*n + j - 1) * n2 + i__;
	  if (!lower)
	    {
	      if (i__ <= j)
		{
		  work[ij] = -d__[i__ + j * d_dim1];
		}
	      else
		{
		  work[ij] = -d__[j + i__ * d_dim1];
		}
	    }
	  else
	    {
	      if (i__ >= j)
		{
		  work[ij] = -d__[i__ + j * d_dim1];
		}
	      else
		{
		  work[ij] = -d__[j + i__ * d_dim1];
		}
	    }
	  ij = (*n + j - 1) * n2 + *n + i__;
	  if (notrna)
	    {
	      work[ij] = -a[j + i__ * a_dim1];
	    }
	  else
	    {
	      work[ij] = -a[i__ + j * a_dim1];
	    }
	  /* L10: */
	}
      /* L20: */
    }
  /* 
   *    Scale the Hamiltonian matrix 
   * 
   */
  cnorm2 = sqrt (cnorm);
  dnorm2 = sqrt (dnorm);
  iscl = 0;
  if (cnorm2 > dnorm2 && dnorm2 > 0.)
    {
      C2F (dlascl) ("G", &c__0, &c__0, &cnorm2, &dnorm2, n, n,
		    &work[*n + 1], &n2, &info2, 1L);
      C2F (dlascl) ("G", &c__0, &c__0, &dnorm2, &cnorm2, n, n,
		    &work[n2 * *n + 1], &n2, &info2, 1L);
      iscl = 1;
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa = (*n << 3) * *n + (*n << 2);
  iwr = n2 * n2;
  iwi = iwr + n2;
  ivs = iwi + n2;
  iwrk = ivs + n2 * n2;
  /* 
   *    Compute the Schur factorization of the Hamiltonian matrix 
   * 
   */
  i__1 = *lwork - iwrk;
  C2F (dgees) ("V", "S", nsp_ctrlpack_selneg, &n2, &work[1], &n2, &sdim,
	       &work[iwr + 1], &work[iwi + 1], &work[ivs + 1], &n2,
	       &work[iwrk + 1], &i__1, &bwork[1], &info2, 1L, 1L);
  if (info2 > 0 && info2 <= n2)
    {
      *info = 1;
      return 0;
    }
  else if (info2 == n2 + 1)
    {
      *info = 2;
      return 0;
    }
  else if (info2 == n2 + 2)
    {
      *info = 3;
      return 0;
    }
  else if (sdim != *n)
    {
      *info = 4;
      return 0;
    }
  lwamax = lwa + (int) work[iwrk + 1];
  /* 
   *    Store the matrices V11 and V21 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = (j - 1) * *n + i__;
	  iv = (i__ - 1) * n2 + ivs + j;
	  work[ij] = work[iv];
	  ij = (j - 1) * *n + (*n << 1) * *n + i__;
	  iv = (i__ - 1) * n2 + ivs + *n + j;
	  work[ij] = work[iv];
	  /* L30: */
	}
      /* L40: */
    }
  /* 
   *    Workspace usage 
   * 
   */
  iaf = *n * *n;
  ib = iaf + *n * *n;
  ir = ib + *n * *n;
  ic = ir + *n;
  ifr = ic + *n;
  ibr = ifr + *n;
  iwrk = ibr + *n;
  /* 
   *    Compute the solution matrix X 
   * 
   */
  C2F (dgesvx) ("E", "N", n, n, &work[1], n, &work[iaf + 1], n,
		&iwork[1], equed, &work[ir + 1], &work[ic + 1],
		&work[ib + 1], n, &x[x_offset], ldx, rcond,
		&work[ifr + 1], &work[ibr + 1], &work[iwrk + 1],
		&iwork[*n + 1], &info2, 1L, 1L, 1L);
  if (info2 > 0)
    {
      *info = 5;
      return 0;
    }
  /* 
   *    Symmetrize the solution 
   * 
   */
  if (*n > 1)
    {
      i__1 = *n - 1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  i__2 = *n;
	  for (j = i__ + 1; j <= i__2; ++j)
	    {
	      temp = (x[i__ + j * x_dim1] + x[j + i__ * x_dim1]) / 2.;
	      x[i__ + j * x_dim1] = temp;
	      x[j + i__ * x_dim1] = temp;
	      /* L50: */
	    }
	  /* L60: */
	}
    }
  /* 
   *    Undo scaling for the solution matrix 
   * 
   */
  if (iscl == 1)
    {
      C2F (dlascl) ("G", &c__0, &c__0, &dnorm2, &cnorm2, n, n,
		    &x[x_offset], ldx, &info2, 1L);
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa = (*n << 1) * *n;
  iu = *n * *n;
  iwrk = iu + *n * *n;
  /* 
   *    Estimate the reciprocal condition number 
   * 
   */
  i__1 = *lwork - iwrk;
  nsp_ctrlpack_riccrc (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &d__[d_offset], ldd, &x[x_offset], ldx, rcond,
		       &work[1], n, &work[iu + 1], n, &wr[1], &wi[1],
		       &work[iwrk + 1], &i__1, &iwork[1], &info2, 1L, 1L);
  if (info2 > 0)
    {
      *info = 6;
      return 0;
    }
  lwa += (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (*rcond == 0.)
    {
      *ferr = 1.;
      work[1] = (double) lwamax;
      return 0;
    }
  /* 
   *    Estimate the bound on the forward error 
   * 
   */
  i__1 = *lwork - iwrk;
  nsp_ctrlpack_riccfr (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &d__[d_offset], ldd, &x[x_offset], ldx, &work[1], n,
		       &work[iu + 1], n, ferr, &work[iwrk + 1], &i__1,
		       &iwork[1], &info2, 1L, 1L);
  lwa = *n * 9 * *n;
  lwamax = Max (lwa, lwamax);
  work[1] = (double) lwamax;
  return 0;
  /* 
   *    End of RICCSL 
   * 
   */
}				/* riccsl_ */

/* 
 * -- LISPACK auxiliary routine (version 3.0) -- 
 *    Tech. University of Sofia 
 *    July 5, 1999 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 * 
 *    Purpose 
 *    ======= 
 * 
 *    SELNEG is used to select eigenvalues with negative real parts 
 *    to sort to the top left of the Schur form of the Hamiltonian 
 *    matrix in solving matrix algebraic Riccati equations 
 * 
 *    .. Parameters .. 
 *    avoid a warning 
 */

int nsp_ctrlpack_selneg (const double *wr, const double *wi)
{
  /* System generated locals */
  int ret_val;

  /* Local variables */


  if (*wr < 0.)
    {
      ret_val = TRUE;
    }
  else
    {
      ret_val = FALSE;
    }
  /* 
   *    End of SELNEG 
   * 
   */
  return ret_val;
}				/* selneg_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * RICDFR estimates the forward error bound for the computed solution of 
 * the discrete-time matrix algebraic Riccati equation 
 *                              -1 
 * transpose(op(A))*X*(In + D*X)  *op(A) - X + C = 0 
 * 
 * where op(A) = A or A**T and C, D are symmetric (C = C**T, D = D**T). 
 * The matrices A, C, D and X are N-by-N. 
 * 
 * Arguments 
 * ========= 
 * 
 * TRANA   (input) CHARACTER*1 
 *         Specifies the option op(A): 
 *         = 'N': op(A) = A    (No transpose) 
 *         = 'T': op(A) = A**T (Transpose) 
 *         = 'C': op(A) = A**T (Conjugate transpose = Transpose) 
 * 
 * N       (input) INT 
 *         The order of the matrix A, and the order of the 
 *         matrices C, D and X. N >= 0. 
 * 
 * A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
 *         The N-by-N matrix A. 
 * 
 * LDA     (input) INT 
 *         The leading dimension of the array A. LDA >= Max(1,N). 
 * 
 * UPLO    (input) CHARACTER*1 
 *         = 'U':  Upper triangle of C is stored; 
 *         = 'L':  Lower triangle of C is stored. 
 * 
 * C       (input) DOUBLE PRECISION array, dimension (LDC,N) 
 *         If UPLO = 'U', the leading N-by-N upper triangular part of C 
 *         contains the upper triangular part of the matrix C. 
 *         If UPLO = 'L', the leading N-by-N lower triangular part of C 
 *         contains the lower triangular part of the matrix C. 
 * 
 * LDC     (input) INT 
 *         The leading dimension of the array C. LDC >= Max(1,N). 
 * 
 * X       (input) DOUBLE PRECISION array, dimension (LDX,N) 
 *         The N-by-N solution matrix X. 
 * 
 * LDX     (input) INT 
 *         The leading dimension of the array X. LDC >= Max(1,N). 
 * 
 * AC      (input) DOUBLE PRECISION array, dimension (LDAC,N) 
 *                                  -1 
 *         The matrix Ac = (I + D*X)  *A (if TRANA = 'N') or 
 *                         -1 
 *         Ac = A*(I + X*D)  (if TRANA = 'T' or 'C'). 
 * 
 * LDAC    (input) INT 
 *         The leading dimension of the array AC. LDAC >= Max(1,N). 
 * 
 * T       (input) DOUBLE PRECISION array, dimension (LDT,N) 
 *         The upper quasi-triangular matrix in Schur canonical form 
 *         from the Schur factorization of the matrix Ac. 
 * 
 * LDT     (input) INT 
 *         The leading dimension of the array T. LDT >= Max(1,N) 
 * 
 * U       (input) DOUBLE PRECISION array, dimension (LDU,N) 
 *         The orthogonal N-by-N matrix from the real Schur 
 *         factorization of the matrix Ac. 
 * 
 * LDU     (input) INT 
 *         The leading dimension of the array U. LDU >= Max(1,N) 
 * 
 * WFERR   (input) DOUBLE PRECISION array, dimension (N) 
 *         The vector of estimated forward error bound for each column 
 *         of the matrix Ac, as obtained by the subroutine RICDRC. 
 * 
 * FERR    (output) DOUBLE PRECISION 
 *         The estimated forward error bound for the solution X. 
 *         If XTRUE is the true solution, FERR bounds the magnitude 
 *         of the largest entry in (X - XTRUE) divided by the magnitude 
 *         of the largest entry in X. 
 * 
 * WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK) 
 * 
 * LWORK   INT 
 *         The dimension of the array WORK. LWORK >= 7*N*N + 2*N 
 * 
 * IWORK   (workspace) INT array, dimension (N*N) 
 * 
 * INFO    INT 
 *         = 0: successful exit 
 *         < 0: if INFO = -i, the i-th argument had an illegal value 
 * 
 * Further details 
 * =============== 
 * 
 * The forward error bound is estimated using a practical error bound 
 * similar to the one proposed in [1]. 
 * 
 * References 
 * ========== 
 * 
 * [1] N.J. Higham. Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 */


int
nsp_ctrlpack_ricdfr (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *x, int *ldx, double *ac,
		     int *ldac, double *t, int *ldt, double *u, int *ldu,
		     double *wferr, double *ferr, double *work, int *lwork,
		     int *iwork, int *info, long int trana_len,
		     long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, ac_dim1, ac_offset, c_dim1, c_offset, t_dim1,
    t_offset, u_dim1, u_offset, x_dim1, x_offset, i__1, i__2;
  double d__1, d__2, d__3;

  /* Local variables */
  int idlc, iabs, kase, ixma, ires, ixbs, itmp, iwrk, info2, i__, j;
  double scale;
  int lower;
  double xnorm;
  int ij;
  double acjmax;
  char tranat[1];
  int notrna;
  int minwrk;
  double eps, est;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  ac_dim1 = *ldac;
  ac_offset = ac_dim1 + 1;
  ac -= ac_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  --wferr;
  --work;
  --iwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldac < Max (1, *n))
    {
      *info = -11;
    }
  else if (*ldt < Max (1, *n))
    {
      *info = -13;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -15;
    }
  /* 
   *    Get the machine precision 
   * 
   */
  eps = C2F (dlamch) ("Epsilon", 7L);
  /* 
   *    Compute workspace 
   * 
   */
  minwrk = *n * 7 * *n + (*n << 1);
  if (*lwork < minwrk)
    {
      *info = -19;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("RICDFR", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  xnorm = C2F (dlansy) ("1", uplo, n, &x[x_offset], ldx, &work[1], 1L, 1L);
  if (xnorm == 0.)
    {
      *ferr = 0.;
      return 0;
    }
  /* 
   *    Workspace usage 
   * 
   */
  idlc = *n * *n;
  itmp = idlc + *n * *n;
  ixma = itmp + *n * *n;
  iabs = ixma + *n * *n;
  ixbs = iabs + *n * *n;
  ires = ixbs + *n * *n;
  iwrk = ires + *n * *n;
  /* 
   */
  if (notrna)
    {
      *(unsigned char *) tranat = 'T';
    }
  else
    {
      *(unsigned char *) tranat = 'N';
    }
  /* 
   *    Form residual matrix R = transpose(op(A))*X*op(Ac) + C - X 
   * 
   */
  C2F (dgemm) (tranat, "N", n, n, n, &c_b82, &a[a_offset], lda,
	       &x[x_offset], ldx, &c_b83, &work[ixma + 1], n, 1L, 1L);
  C2F (dgemm) ("N", trana, n, n, n, &c_b82, &work[ixma + 1], n,
	       &ac[ac_offset], ldac, &c_b83, &work[itmp + 1], n, 1L, 1L);
  if (lower)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = j; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] =
		c__[i__ + j * c_dim1] - x[i__ + j * x_dim1] + work[itmp +
								   i__ + (j -
									  1) *
								   *n];
	      /* L10: */
	    }
	  /* L20: */
	}
    }
  else
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = j;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] =
		c__[i__ + j * c_dim1] - x[i__ + j * x_dim1] + work[itmp +
								   i__ + (j -
									  1) *
								   *n];
	      /* L30: */
	    }
	  /* L40: */
	}
    }
  /* 
   *    Add to Abs(R) a term that takes account of rounding errors in 
   *    forming R: 
   *      Abs(R) := Abs(R) + EPS*( 4*abs(C) + 4*abs(X) + 
   *                (2*n+3)*abs(op(A'))*abs(X)*abs(op(Ac) + 
   *                2*(n+1)*abs(op(A'))*abs(X)*abs(op(DAc) ) 
   *    where EPS is the machine precision and DAc is a bound on the 
   *    absolute error in computing the matrix Ac 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = i__ + (j - 1) * *n;
	  work[iabs + ij] = (d__1 = a[i__ + j * a_dim1], Abs (d__1));
	  work[ixbs + ij] = (d__1 = x[i__ + j * x_dim1], Abs (d__1));
	  work[idlc + ij] = (d__1 = ac[i__ + j * ac_dim1], Abs (d__1));
	  /* L50: */
	}
      /* L60: */
    }
  C2F (dgemm) (tranat, "N", n, n, n, &c_b82, &work[iabs + 1], n,
	       &work[ixbs + 1], n, &c_b83, &work[ixma + 1], n, 1L, 1L);
  C2F (dgemm) ("N", trana, n, n, n, &c_b82, &work[ixma + 1], n,
	       &work[idlc + 1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      acjmax =
	C2F (dlange) ("M", n, &c__1, &ac[j * ac_dim1 + 1], ldac,
		      &work[1], 1L);
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  work[iabs + i__ + (j - 1) * *n] = acjmax * wferr[j];
	  /* L70: */
	}
      /* L80: */
    }
  C2F (dgemm) ("N", trana, n, n, n, &c_b82, &work[ixma + 1], n,
	       &work[iabs + 1], n, &c_b83, &work[idlc + 1], n, 1L, 1L);
  if (lower)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = j; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] = (d__1 =
						 work[ires + i__ +
						      (j - 1) * *n],
						 Abs (d__1)) +
		eps * 4. * ((d__2 = c__[i__ + j * c_dim1], Abs (d__2)) +
			    (d__3 =
			     x[i__ + j * x_dim1],
			     Abs (d__3))) + (double) ((*n << 1) +
						      3) * eps * work[itmp +
								      i__ +
								      (j -
								       1) *
								      *n] +
		(double) ((*n << 1) + 2) * work[idlc + 1];
	      /* L90: */
	    }
	  /* L100: */
	}
    }
  else
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = j;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      work[ires + i__ + (j - 1) * *n] = (d__1 =
						 work[ires + i__ +
						      (j - 1) * *n],
						 Abs (d__1)) +
		eps * 4. * ((d__2 = c__[i__ + j * c_dim1], Abs (d__2)) +
			    (d__3 =
			     x[i__ + j * x_dim1],
			     Abs (d__3))) + (double) ((*n << 1) +
						      3) * eps * work[itmp +
								      i__ +
								      (j -
								       1) *
								      *n] +
		(double) ((*n << 1) + 2) * work[idlc + 1];
	      /* L110: */
	    }
	  /* L120: */
	}
    }
  /* 
   *    Compute forward error bound, using matrix norm estimator 
   * 
   */
  est = 0.;
  kase = 0;
 L130:
  i__1 = *n * (*n + 1) / 2;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] =
			work[ij] * work[ires + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Unpack the lower triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] = work[ij];
		    }
		  /* L140: */
		}
	      /* L150: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] =
			work[ij] * work[ires + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Unpack the upper triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[itmp + i__ + (j - 1) * *n] = work[ij];
		    }
		  /* L160: */
		}
	      /* L170: */
	    }
	}
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      if (kase == 2)
	{
	  /* 
	   *          Solve op(A')*Y + Y*op(A) = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (trana, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &work[iwrk + 1], &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(A)*Z + Z*op(A') = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (tranat, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &work[iwrk + 1], &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n,
		   &u[u_offset], ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Pack the lower triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[ij] = work[itmp + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[ij] =
			work[itmp + i__ + (j - 1) * *n] * work[ires + i__ +
							       (j - 1) * *n];
		    }
		  /* L180: */
		}
	      /* L190: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  if (kase == 2)
		    {
		      /* 
		       *                   Pack the upper triangular part of symmetric 
		       *                   matrix 
		       * 
		       */
		      work[ij] = work[itmp + i__ + (j - 1) * *n];
		    }
		  else
		    {
		      /* 
		       *                   Scale by the residual matrix 
		       * 
		       */
		      work[ij] =
			work[itmp + i__ + (j - 1) * *n] * work[ires + i__ +
							       (j - 1) * *n];
		    }
		  /* L200: */
		}
	      /* L210: */
	    }
	}
      goto L130;
    }
  /* 
   *    Compute the estimate of the forward error 
   * 
   */
  *ferr =
    est * 2. / C2F (dlansy) ("Max", uplo, n, &x[x_offset], ldx,
			     &work[1], 3L, 1L) / scale;
  if (*ferr > 1.)
    {
      *ferr = 1.;
    }
  /* 
   */
  return 0;
  /* 
   *    End of RICDFR 
   * 
   */
}				/* ricdfr_ */



/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * RICDMF solves the discrete-time matrix algebraic Riccati equation 
 *         = 3: the matrix Ac = (I + D*X)  *A or Ac = A*(I + X*D) 
 *              can not be reduced to Schur canonical form and condition 
 *              number estimate and forward error estimate are not 
 *              computed 
 * 
 * Further Details 
 * =============== 
 * 
 * The discrete-time matrix Riccati equation is solved by using the 
 * inverse free spectral decomposition method, proposed in [1]. 
 * 
 * The condition number of the equation is estimated using 1-norm 
 * estimator. 
 * 
 * The forward error bound is estimated using a practical error bound 
 * similar to the one proposed in [2]. 
 * 
 * References 
 * ========== 
 * 
 * [1] Z. Bai, J. Demmel and M. Gu. An inverse free parallel spectral 
 *     divide and conquer algorithm for nonsymmetric eigenproblems. 
 *     Numer. Math., vol. 76, pp. 279-308, 1997. 
 * [2] N.J. Higham. Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * [3] M.M. Konstantinov, P.Hr. Petkov, and N.D. Christov. Perturbation 
 *     analysis of the discrete Riccati equation. Kybernetica (Prague), 
 *     vol. 29,pp. 18-29, 1993. 
 */

int
nsp_ctrlpack_ricdmf (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *d__, int *ldd, double *x,
		     int *ldx, double *wr, double *wi, double *rcond,
		     double *ferr, double *work, int *lwork, int *iwork,
		     int *info, long int trana_len, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, d_dim1, d_offset, x_dim1, x_offset,
    i__1, i__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int iscl, itau, iter;
  double temp;
  int iwrk, info2, i__, j;
  char equed[1];
  double cnorm, dnorm;
  int lower;
  double rnorm=0.0;
  int n2, n4;
  double cnorm2, dnorm2;
  int ia, ib, ic, ij;
  int iq, ir, is;
  int iu, iv;
  int lwamax;
  int iwferr;
  int notrna;
  double rdnorm=0.0;
  int ij1, ij2;
  int minwrk;
  int iac, iaf, ibr, ifr, lwa;
  double eps, tol;
  int lwa0;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --wr;
  --wi;
  --work;
  --iwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldd < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -11;
    }
  /* 
   *    Set tol 
   * 
   */
  eps = C2F (dlamch) ("Epsilon", 7L);
  tol = (double) (*n) * 10. * eps;
  /* 
   *    Compute workspace 
   * 
   *Computing MAX 
   */
  i__1 = 1, i__2 = *n << 1;
  minwrk = *n * 28 * *n + (*n << 1) + Max (i__1, i__2);
  if (*lwork < minwrk)
    {
      *info = -17;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("RICDMF", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Compute the norms of the matrices C and D 
   * 
   */
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  dnorm = C2F (dlansy) ("1", uplo, n, &d__[d_offset], ldd, &work[1], 1L, 1L);
  /* 
   */
  n2 = *n << 1;
  n4 = *n << 2;
  /* 
   *    Construct B0 and -A0 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = (*n + j - 1) * n2 + i__;
	  if (!lower)
	    {
	      if (i__ <= j)
		{
		  work[ij] = d__[i__ + j * d_dim1];
		}
	      else
		{
		  work[ij] = d__[j + i__ * d_dim1];
		}
	    }
	  else
	    {
	      if (i__ >= j)
		{
		  work[ij] = d__[i__ + j * d_dim1];
		}
	      else
		{
		  work[ij] = d__[j + i__ * d_dim1];
		}
	    }
	  ij = (*n + j - 1) * n2 + *n + i__;
	  if (notrna)
	    {
	      work[ij] = a[j + i__ * a_dim1];
	    }
	  else
	    {
	      work[ij] = a[i__ + j * a_dim1];
	    }
	  ij = n2 * n2 + (j - 1) * n2 + i__;
	  if (notrna)
	    {
	      work[ij] = -a[i__ + j * a_dim1];
	    }
	  else
	    {
	      work[ij] = -a[j + i__ * a_dim1];
	    }
	  ij = n2 * n2 + (j - 1) * n2 + *n + i__;
	  if (!lower)
	    {
	      if (i__ <= j)
		{
		  work[ij] = c__[i__ + j * c_dim1];
		}
	      else
		{
		  work[ij] = c__[j + i__ * c_dim1];
		}
	    }
	  else
	    {
	      if (i__ >= j)
		{
		  work[ij] = c__[i__ + j * c_dim1];
		}
	      else
		{
		  work[ij] = c__[j + i__ * c_dim1];
		}
	    }
	  /* L10: */
	}
      /* L20: */
    }
  C2F (dlaset) ("Full", n, n, &c_b83, &c_b83, &work[*n + 1], &n2, 4L);
  C2F (dlaset) ("Full", n, n, &c_b83, &c_b83,
		&work[n2 * n2 + n2 * *n + 1], &n2, 4L);
  C2F (dlaset) ("Full", n, n, &c_b83, &c_b82, &work[1], &n2, 4L);
  C2F (dlaset) ("Full", n, n, &c_b83, &c_b79,
		&work[n2 * n2 + n2 * *n + *n + 1], &n2, 4L);
  /* 
   *    Scale the matrices B0 and -A0 
   * 
   */
  cnorm2 = sqrt (cnorm);
  dnorm2 = sqrt (dnorm);
  iscl = 0;
  if (cnorm2 > dnorm2 && dnorm2 > 0.)
    {
      C2F (dlascl) ("G", &c__0, &c__0, &cnorm2, &dnorm2, n, n,
		    &work[n2 * n2 + *n + 1], &n2, &info2, 1L);
      C2F (dlascl) ("G", &c__0, &c__0, &dnorm2, &cnorm2, n, n,
		    &work[n2 * *n + 1], &n2, &info2, 1L);
      iscl = 1;
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa0 = *n * 28 * *n + (*n << 1);
  lwamax = 0;
  ia = n2 * n2;
  ir = ia + n2 * n2;
  is = ir + n4 * n2;
  iq = is + n2 * n2;
  itau = iq + n4 * n2;
  iwrk = itau + n2;
  /* 
   *    Copy B0 and -A0 
   * 
   */
  C2F (dlacpy) ("F", &n2, &n2, &work[1], &n2, &work[ir + 1], &n4, 1L);
  C2F (dlacpy) ("F", &n2, &n2, &work[ia + 1], &n2, &work[ir + n2 + 1],
		&n4, 1L);
  /* 
   *    Main iteration loop 
   * 
   */
  for (iter = 1; iter <= 50; ++iter)
    {
      /* 
       *                           [ Bj] 
       *       QR decomposition of [   ] 
       *                           [-Aj] 
       * 
       */
      i__1 = *lwork - iwrk;
      C2F (dgeqrf) (&n4, &n2, &work[ir + 1], &n4, &work[itau + 1],
		    &work[iwrk + 1], &i__1, &info2);
      lwa = lwa0 + (int) work[iwrk + 1];
      lwamax = Max (lwa, lwamax);
      /* 
       *       Make the diagonal elements of Rj positive 
       * 
       */
      i__1 = n2;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (work[ir + (i__ - 1) * n4 + i__] < 0.)
	    {
	      i__2 = n2 - i__ + 1;
	      C2F (dscal) (&i__2, &c_b79,
			   &work[ir + (i__ - 1) * n4 + i__], &n4);
	    }
	  /* L30: */
	}
      if (iter > 1)
	{
	  /* 
	   *          Compute Rj+1 - Rj 
	   * 
	   */
	  i__1 = n2;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ij1 = ir + (j - 1) * n4 + i__;
		  ij2 = is + (j - 1) * n2 + i__;
		  work[ij2] = work[ij1] - work[ij2];
		  /* L40: */
		}
	      /* L50: */
	    }
	  rdnorm =
	    C2F (dlange) ("1", &n2, &n2, &work[is + 1], &n2,
			  &work[iwrk + 1], 1L);
	}
      /* 
       *       Save Rj for future use 
       * 
       */
      C2F (dlacpy) ("U", &n2, &n2, &work[ir + 1], &n4, &work[is + 1],
		    &n2, 1L);
      if (iter == 1)
	{
	  i__1 = n2 - 1;
	  i__2 = n2 - 1;
	  C2F (dlaset) ("L", &i__1, &i__2, &c_b83, &c_b83,
			&work[is + 2], &n2, 1L);
	}
      /* 
       *       Generate the matrices Q12 and Q22 
       * 
       */
      C2F (dlaset) ("F", &n2, &n2, &c_b83, &c_b83, &work[iq + 1], &n4, 1L);
      C2F (dlaset) ("F", &n2, &n2, &c_b83, &c_b82, &work[iq + n2 + 1],
		    &n4, 1L);
      i__1 = *lwork - iwrk;
      C2F (dormqr) ("L", "N", &n4, &n2, &n2, &work[ir + 1], &n4,
		    &work[itau + 1], &work[iq + 1], &n4,
		    &work[iwrk + 1], &i__1, &info2, 1L, 1L);
      lwa = lwa0 + (int) work[iwrk + 1];
      lwamax = Max (lwa, lwamax);
      /* 
       *       Compute Bj and -Aj 
       * 
       */
      C2F (dgemm) ("T", "N", &n2, &n2, &n2, &c_b82, &work[iq + n2 + 1],
		   &n4, &work[1], &n2, &c_b83, &work[ir + 1], &n4, 1L, 1L);
      C2F (dgemm) ("T", "N", &n2, &n2, &n2, &c_b82, &work[iq + 1], &n4,
		   &work[ia + 1], &n2, &c_b83, &work[ir + n2 + 1], &n4,
		   1L, 1L);
      C2F (dlacpy) ("F", &n2, &n2, &work[ir + 1], &n4, &work[1], &n2, 1L);
      C2F (dlacpy) ("F", &n2, &n2, &work[ir + n2 + 1], &n4,
		    &work[ia + 1], &n2, 1L);
      /* 
       *       Test for convergence 
       * 
       */
      if (iter > 1 && rdnorm <= tol * rnorm)
	{
	  goto L70;
	}
      rnorm =
	C2F (dlange) ("1", &n2, &n2, &work[is + 1], &n2, &work[iwrk + 1], 1L);
      /* L60: */
    }
  *info = 1;
 L70:
  lwa0 = *n * 10 * *n + (*n << 1);
  iq = ia + n2 * n2;
  itau = iq + n2 * *n;
  iwrk = itau + n2;
  /* 
   *    Compute Ap + Bp 
   * 
   */
  i__1 = n2 * n2;
  C2F (dscal) (&i__1, &c_b79, &work[ia + 1], &c__1);
  i__1 = n2 * n2;
  C2F (daxpy) (&i__1, &c_b82, &work[1], &c__1, &work[ia + 1], &c__1);
  /* 
   *    QR decomposition with column pivoting of Bp 
   * 
   */
  i__1 = n2;
  for (j = 1; j <= i__1; ++j)
    {
      iwork[j] = 0;
      /* L80: */
    }
  i__1 = *lwork - iwrk;
  C2F (dgeqp3) (&n2, &n2, &work[1], &n2, &iwork[1], &work[itau + 1],
		&work[iwrk + 1], &i__1, &info2);
  lwa = lwa0 + (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *              T 
   *    Compute Q1 (Ap + Bp) 
   * 
   */
  i__1 = *lwork - iwrk;
  C2F (dormqr) ("L", "T", &n2, &n2, &n2, &work[1], &n2,
		&work[itau + 1], &work[ia + 1], &n2, &work[iwrk + 1],
		&i__1, &info2, 1L, 1L);
  lwa = lwa0 + (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *                          T 
   *    RQ decomposition of Q1 (Ap + Bp) 
   * 
   */
  i__1 = *lwork - iwrk;
  C2F (dgerqf) (&n2, &n2, &work[ia + 1], &n2, &work[itau + 1],
		&work[iwrk + 1], &i__1, &info2);
  lwa = lwa0 + (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Generate Q11 and Q21 
   * 
   */
  C2F (dlaset) ("F", n, n, &c_b83, &c_b82, &work[iq + 1], &n2, 1L);
  C2F (dlaset) ("F", n, n, &c_b83, &c_b83, &work[iq + *n + 1], &n2, 1L);
  i__1 = *lwork - iwrk;
  C2F (dormrq) ("L", "T", &n2, n, &n2, &work[ia + 1], &n2,
		&work[itau + 1], &work[iq + 1], &n2, &work[iwrk + 1],
		&i__1, &info2, 1L, 1L);
  lwa = lwa0 + (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Store the matrices Q11 and Q21 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = (j - 1) * *n + i__;
	  iv = iq + (i__ - 1) * n2 + j;
	  work[ij] = work[iv];
	  ij = (j - 1) * *n + (*n << 1) * *n + i__;
	  iv = iq + (i__ - 1) * n2 + *n + j;
	  work[ij] = work[iv];
	  /* L90: */
	}
      /* L100: */
    }
  /* 
   *    Workspace usage 
   * 
   */
  iaf = *n * *n;
  ib = iaf + *n * *n;
  ir = ib + *n * *n;
  ic = ir + *n;
  ifr = ic + *n;
  ibr = ifr + *n;
  iwrk = ibr + *n;
  /* 
   *    Compute the solution matrix X 
   * 
   */
  C2F (dgesvx) ("E", "N", n, n, &work[1], n, &work[iaf + 1], n,
		&iwork[1], equed, &work[ir + 1], &work[ic + 1],
		&work[ib + 1], n, &x[x_offset], ldx, rcond,
		&work[ifr + 1], &work[ibr + 1], &work[iwrk + 1],
		&iwork[*n + 1], &info2, 1L, 1L, 1L);
  if (info2 > 0)
    {
      *info = 2;
      return 0;
    }
  /* 
   *    Symmetrize the solution 
   * 
   */
  if (*n > 1)
    {
      i__1 = *n - 1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  i__2 = *n;
	  for (j = i__ + 1; j <= i__2; ++j)
	    {
	      temp = (x[i__ + j * x_dim1] + x[j + i__ * x_dim1]) / 2.;
	      x[i__ + j * x_dim1] = temp;
	      x[j + i__ * x_dim1] = temp;
	      /* L110: */
	    }
	  /* L120: */
	}
    }
  /* 
   *    Undo scaling for the solution matrix 
   * 
   */
  if (iscl == 1)
    {
      C2F (dlascl) ("G", &c__0, &c__0, &dnorm2, &cnorm2, n, n,
		    &x[x_offset], ldx, &info2, 1L);
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa = *n * 3 * *n + *n;
  iu = *n * *n;
  iwferr = iu + *n * *n;
  iac = iwferr + *n;
  iwrk = iac + *n * *n;
  /* 
   *    Estimate the reciprocal condition number 
   * 
   */
  i__1 = *lwork - iwrk;
  nsp_ctrlpack_ricdrc (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &d__[d_offset], ldd, &x[x_offset], ldx, rcond,
		       &work[iac + 1], n, &work[1], n, &work[iu + 1], n,
		       &wr[1], &wi[1], &work[iwferr + 1], &work[iwrk + 1],
		       &i__1, &iwork[1], &info2, 1L, 1L);
  if (info2 > 0)
    {
      *info = 3;
      return 0;
    }
  lwa += (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (*rcond == 0.)
    {
      *ferr = 1.;
      work[1] = (double) lwamax;
      return 0;
    }
  /* 
   *    Estimate the bound on the forward error 
   * 
   */
  i__1 = *lwork - iwrk;
  nsp_ctrlpack_ricdfr (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &x[x_offset], ldx, &work[iac + 1], n, &work[1], n,
		       &work[iu + 1], n, &work[iwferr + 1], ferr,
		       &work[iwrk + 1], &i__1, &iwork[1], &info2, 1L, 1L);
  lwa = *n * 9 * *n + *n * 3;
  lwamax = Max (lwa, lwamax);
  work[1] = (double) lwamax;
  return 0;
  /* 
   *    End of RICDMF 
   * 
   */
}				/* ricdmf_ */


/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * RICDRC estimates the reciprocal of the condition number of the 
 *         < 0: if INFO = -i, the i-th argument had an illegal value 
 *         = 1: the matrix I + D*X is singular to working precision 
 *              and condition number estimate is not computed 
 *         = 2: the matrix Ac can not be reduced to Schur canonical 
 *              form and condition number estimate is not computed 
 * 
 * Further details 
 * =============== 
 * 
 * The condition number of the discrete-time Riccati equation is 
 * estimated as 
 * 
 * cond = ( norm(Theta)*norm(A) + norm(inv(Omega))*norm(C) + 
 *              norm(Pi)*norm(D) ) / norm(X) 
 * 
 * where Omega, Theta and Pi are linear operators defined by 
 * 
 * Omega(Z) = transpose(op(Ac))*Z*op(Ac) - Z, 
 * Theta(Z) = inv(Omega(transpose(op(Z))*X*op(Ac) + 
 *                transpose(op(Ac))*X*op(Z))), 
 * Pi(Z) = inv(Omega(transpose(op(Ac))*X*Z*X*op(Ac))) 
 *                   -1                                      -1 
 * and Ac = (I + D*X)  *A (if TRANA = 'N') or Ac = A*(I + X*D) 
 * (if TRANA = 'T' or 'C'). 
 * 
 * The program estimates the quantities 
 * 
 * sepd(op(Ac),transpose(op(Ac)) = 1 / norm(inv(Omega)), 
 * 
 * norm(Theta) and norm(Pi) using 1-norm condition estimator. 
 * 
 * References 
 * ========== 
 * 
 * [1] N.J. Higham. Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * [2] M.M. Konstantinov, P.Hr. Petkov, and N.D. Christov. Perturbation 
 *     analysis of the discrete Riccati equation. Kybernetica (Prague), 
 *     vol. 29,pp. 18-29, 1993. 
 */


int
nsp_ctrlpack_ricdrc (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *d__, int *ldd, double *x,
		     int *ldx, double *rcond, double *ac, int *ldac,
		     double *t, int *ldt, double *u, int *ldu, double *wr,
		     double *wi, double *wferr, double *work, int *lwork,
		     int *iwork, int *info, long int trana_len,
		     long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, ac_dim1, ac_offset, c_dim1, c_offset, d_dim1,
    d_offset, t_dim1, t_offset, u_dim1, u_offset, x_dim1, x_offset, i__1,
    i__2;

  /* Local variables */
  int idlc, kase, sdim;
  double sepd;
  int ixma, itmp, iwrk, info2, i__, j;
  double scale;
  char equed[1];
  double anorm, cnorm, dnorm;
  int bwork[1], lower;
  double wrcon;
  double xnorm;
  int ic;
  int ij;
  int ir;
  char tranat[1];
  int notrna;
  double pinorm;
  int minwrk;
  double thnorm;
  int iaf, ibr, lwa;
  double est;
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  ac_dim1 = *ldac;
  ac_offset = ac_dim1 + 1;
  ac -= ac_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  --wr;
  --wi;
  --wferr;
  --work;
  --iwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldd < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -11;
    }
  else if (*ldac < Max (1, *n))
    {
      *info = -14;
    }
  else if (*ldt < Max (1, *n))
    {
      *info = -16;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -18;
    }
  /* 
   *    Compute workspace 
   * 
   *Computing MAX 
   */
  i__1 = 1, i__2 = *n << 2;
  minwrk = *n * 5 * *n + *n * 3 + Max (i__1, i__2);
  if (*lwork < minwrk)
    {
      *info = -23;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("RICDRC", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  xnorm = C2F (dlansy) ("1", uplo, n, &x[x_offset], ldx, &work[1], 1L, 1L);
  if (xnorm == 0.)
    {
      *rcond = 0.;
      return 0;
    }
  /* 
   *    Compute the norms of the matrices A, C and D 
   * 
   */
  anorm = C2F (dlange) ("1", n, n, &a[a_offset], lda, &work[1], 1L);
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  dnorm = C2F (dlansy) ("1", uplo, n, &d__[d_offset], ldd, &work[1], 1L, 1L);
  /* 
   *    Workspace usage 
   * 
   */
  lwa = *n * 5 * *n + *n * 3;
  idlc = *n * *n;
  itmp = idlc + *n * *n;
  ixma = itmp + *n * *n;
  iaf = ixma + *n * *n;
  ir = iaf + *n * *n;
  ic = ir + *n;
  ibr = ic + *n;
  iwrk = ibr + *n;
  /* 
   */
  C2F (dlaset) ("Full", n, n, &c_b83, &c_b82, &work[1], n, 4L);
  C2F (dsymm) ("L", uplo, n, n, &c_b82, &d__[d_offset], ldd,
	       &x[x_offset], ldx, &c_b82, &work[1], n, 1L, 1L);
  if (notrna)
    {
      /*                             -1 
       *       Compute Ac = (I + D*X)  *A 
       * 
       */
      C2F (dlacpy) ("F", n, n, &a[a_offset], lda, &t[t_offset], ldt, 1L);
      C2F (dgesvx) ("E", "N", n, n, &work[1], n, &work[iaf + 1], n,
		    &iwork[1], equed, &work[ir + 1], &work[ic + 1],
		    &t[t_offset], ldt, &ac[ac_offset], ldac, &wrcon,
		    &wferr[1], &work[ibr + 1], &work[iwrk + 1],
		    &iwork[*n + 1], &info2, 1L, 1L, 1L);
      if (info2 > 0)
	{
	  *info = 1;
	  return 0;
	}
    }
  else
    {
      /*                               -1 
       *       Compute Ac = A*(I + X*D) 
       * 
       */
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      t[i__ + j * t_dim1] = a[j + i__ * a_dim1];
	      /* L10: */
	    }
	  /* L20: */
	}
      C2F (dgesvx) ("E", "N", n, n, &work[1], n, &work[iaf + 1], n,
		    &iwork[1], equed, &work[ir + 1], &work[ic + 1],
		    &t[t_offset], ldt, &work[itmp + 1], n, &wrcon,
		    &wferr[1], &work[ibr + 1], &work[iwrk + 1],
		    &iwork[*n + 1], &info2, 1L, 1L, 1L);
      if (info2 > 0)
	{
	  *info = 1;
	  return 0;
	}
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      ac[i__ + j * ac_dim1] = work[itmp + j + (i__ - 1) * *n];
	      /* L30: */
	    }
	  /* L40: */
	}
    }
  /* 
   *    Compute the Schur factorization of Ac 
   * 
   */
  C2F (dlacpy) ("F", n, n, &ac[ac_offset], ldac, &t[t_offset], ldt, 1L);
  i__1 = *lwork - iwrk;
  C2F (dgees) ("V", "N", nsp_ctrlpack_voiddummy, n, &t[t_offset], ldt,
	       &sdim, &wr[1], &wi[1], &u[u_offset], ldu,
	       &work[iwrk + 1], &i__1, bwork, &info2, 1L, 1L);
  if (info2 > 0)
    {
      *info = 2;
      return 0;
    }
  lwa += (int) work[iwrk + 1];
  /* 
   *    Compute X*op(Ac) 
   * 
   */
  C2F (dgemm) ("N", trana, n, n, n, &c_b82, &x[x_offset], ldx,
	       &ac[ac_offset], ldac, &c_b83, &work[ixma + 1], n, 1L, 1L);
  /* 
   *    Estimate sepd(op(Ac),transpose(op(Ac))) 
   * 
   */
  if (notrna)
    {
      *(unsigned char *) tranat = 'T';
    }
  else
    {
      *(unsigned char *) tranat = 'N';
    }
  /* 
   */
  est = 0.;
  kase = 0;
 L50:
  i__1 = *n * (*n + 1) / 2;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      /* 
       *       Unpack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L60: */
		}
	      /* L70: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L80: */
		}
	      /* L90: */
	    }
	}
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      if (kase == 1)
	{
	  /* 
	   *          Solve op(Ac')*Y*op(Ac) - Y = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (trana, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &work[iwrk + 1], &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(Ac)*Z*op(Ac') - Z = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (tranat, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &work[iwrk + 1], &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n,
		   &u[u_offset], ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      /* 
       *       Pack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L100: */
		}
	      /* L110: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L120: */
		}
	      /* L130: */
	    }
	}
      goto L50;
    }
  /* 
   */
  sepd = scale / 2. / est;
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (sepd == 0.)
    {
      *rcond = 0.;
      return 0;
    }
  /* 
   *    Estimate norm(Theta) 
   * 
   */
  est = 0.;
  kase = 0;
 L140:
  i__1 = *n * *n;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      /* 
       *       Compute RHS = op(W')*X*op(A) + op(A')*X*op(W) 
       * 
       */
      C2F (dsyr2k) (uplo, tranat, n, n, &c_b82, &work[1], n,
		    &work[ixma + 1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dlacpy) (uplo, n, n, &work[itmp + 1], n, &work[1], n, 1L);
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[1], n, &u[u_offset],
		   ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[itmp + 1], n, &c_b83, &work[1], n, 1L, 1L);
      if (kase == 1)
	{
	  /* 
	   *          Solve op(Ac')*Y*op(Ac) - Y = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (trana, n, &t[t_offset], ldt, &work[1], n,
			       &scale, &work[iwrk + 1], &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(Ac)*Z*op(Ac') - Z = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (tranat, n, &t[t_offset], ldt, &work[1], n,
			       &scale, &work[iwrk + 1], &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[1], n, &u[u_offset],
		   ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      goto L140;
    }
  /* 
   */
  thnorm = est / scale;
  /* 
   *    Estimate norm(Pi) 
   * 
   */
  est = 0.;
  kase = 0;
 L150:
  i__1 = *n * (*n + 1) / 2;
  C2F (dlacon) (&i__1, &work[idlc + 1], &work[1], &iwork[1], &est, &kase);
  if (kase != 0)
    {
      /* 
       *       Unpack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L160: */
		}
	      /* L170: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[itmp + i__ + (j - 1) * *n] = work[ij];
		  /* L180: */
		}
	      /* L190: */
	    }
	}
      /* 
       *       Compute RHS = op(Ac')*X*W*X*op(Ac) 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &work[ixma + 1], n, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &work[ixma + 1], n,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      /* 
       *       Transform the right-hand side: RHS := U'*RHS*U 
       * 
       */
      C2F (dsymm) ("L", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("T", "N", n, n, n, &c_b82, &u[u_offset], ldu,
		   &work[1], n, &c_b83, &work[itmp + 1], n, 1L, 1L);
      if (kase == 1)
	{
	  /* 
	   *          Solve op(Ac')*Y*op(Ac) - Y = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (trana, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &work[iwrk + 1], &info2, 1L);
	}
      else
	{
	  /* 
	   *          Solve op(Ac)*Z*op(Ac') - Z = scale*RHS 
	   * 
	   */
	  nsp_ctrlpack_lypdtr (tranat, n, &t[t_offset], ldt, &work[itmp + 1],
			       n, &scale, &work[iwrk + 1], &info2, 1L);
	}
      /* 
       *       Transform back to obtain the solution: X := U*X*U' . 
       * 
       */
      C2F (dsymm) ("R", uplo, n, n, &c_b82, &work[itmp + 1], n,
		   &u[u_offset], ldu, &c_b83, &work[1], n, 1L, 1L);
      C2F (dgemm) ("N", "T", n, n, n, &c_b82, &work[1], n,
		   &u[u_offset], ldu, &c_b83, &work[itmp + 1], n, 1L, 1L);
      /* 
       *       Pack the triangular part of symmetric matrix 
       * 
       */
      ij = 0;
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L200: */
		}
	      /* L210: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  ++ij;
		  work[ij] = work[itmp + i__ + (j - 1) * *n];
		  /* L220: */
		}
	      /* L230: */
	    }
	}
      goto L150;
    }
  /* 
   */
  pinorm = est * 2. / scale;
  /* 
   *    Estimate the reciprocal condition number 
   * 
   */
  *rcond = sepd * xnorm / (cnorm + sepd * (thnorm * anorm + pinorm * dnorm));
  if (*rcond > 1.)
    {
      *rcond = 1.;
    }
  /* 
   */
  work[1] = (double) lwa;
  return 0;
  /* 
   *    End of RICDRC 
   * 
   */
}				/* ricdrc_ */

/* 
 * -- RICCPACK routine (version 1.0) -- 
 *    May 10, 2000 
 * 
 *    .. Scalar Arguments .. 
 *    .. 
 *    .. Array Arguments .. 
 *    .. 
 * 
 * Purpose 
 * ======= 
 * 
 * RICDSL solves the discrete-time matrix algebraic Riccati equation 
 *         = 3: reordering of the generalized Shur form failed 
 *         = 4: the matrix pencil has less than N generalized 
 *              eigenvalues with moduli less than one 
 *         = 5: the system of linear equations for the solution is 
 *              singular to working precision 
 *                                       -1                      -1 
 *         = 6: the matrix Ac = (I + D*X)  *A or Ac = A*(I + X*D) 
 *              can not be reduced to Schur canonical form and condition 
 *              number estimate and forward error estimate are not 
 *              computed 
 * 
 * Further Details 
 * =============== 
 * 
 * The discrete-time matrix Riccati equation is solved by the 
 * generalized Schur method [1]. 
 * 
 * The condition number of the equation is estimated using 1-norm 
 * estimator. 
 * 
 * The forward error bound is estimated using a practical error bound 
 * similar to the one proposed in [2]. 
 * 
 * References 
 * ========== 
 * 
 * [1] W.F Arnold, III and A.J. Laub. Generalized eigenproblem 
 *     algorithms and software for algebraic Riccati equations, 
 *     Proc. IEEE, vol. 72, pp. 1746-1754, 1984. 
 * [2] N.J. Higham. Perturbation theory and backward error for AX - XB = 
 *     C, BIT, vol. 33, pp. 124-136, 1993. 
 * [3] M.M. Konstantinov, P.Hr. Petkov, and N.D. Christov. Perturbation 
 *     analysis of the discrete Riccati equation. Kybernetica (Prague), 
 *     vol. 29,pp. 18-29, 1993. 
 */

int
nsp_ctrlpack_ricdsl (char *trana, int *n, double *a, int *lda, char *uplo,
		     double *c__, int *ldc, double *d__, int *ldd, double *x,
		     int *ldx, double *wr, double *wi, double *rcond,
		     double *ferr, double *work, int *lwork, int *iwork,
		     int *bwork, int *info, long int trana_len,
		     long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, d_dim1, d_offset, x_dim1, x_offset,
    i__1, i__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int iscl, sdim;
  double temp;
  int iwrk, info2, i__, j, ibeta;
  char equed[1];
  double cnorm, dnorm;
  int lower;
  int n2;
  double cnorm2, dnorm2;
  int ib, ic, ij, ialfai, ir, ialfar, iu, iv;
  int lwamax;
  int iwferr;
  int notrna;
  int minwrk, iac, iaf, ibr, ifr, lwa, ivs;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --wr;
  --wi;
  --work;
  --iwork;
  --bwork;

  /* Function Body */
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  /* 
   */
  *info = 0;
  if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
      && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -4;
    }
  else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
    {
      *info = -5;
    }
  else if (*ldc < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldd < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -11;
    }
  /* 
   *    Compute workspace 
   * 
   *Computing MAX 
   */
  i__1 = 16, i__2 = *n << 2;
  minwrk = *n * 12 * *n + *n * 22 + Max (i__1, i__2);
  if (*lwork < minwrk)
    {
      *info = -17;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("RICDSL", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Compute the norms of the matrices C and D 
   * 
   */
  cnorm = C2F (dlansy) ("1", uplo, n, &c__[c_offset], ldc, &work[1], 1L, 1L);
  dnorm = C2F (dlansy) ("1", uplo, n, &d__[d_offset], ldd, &work[1], 1L, 1L);
  /* 
   */
  n2 = *n << 1;
  /* 
   *    Construct the matrix pencil 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = (j - 1) * n2 + i__;
	  if (notrna)
	    {
	      work[ij] = a[i__ + j * a_dim1];
	    }
	  else
	    {
	      work[ij] = a[j + i__ * a_dim1];
	    }
	  ij = (j - 1) * n2 + *n + i__;
	  if (!lower)
	    {
	      if (i__ <= j)
		{
		  work[ij] = -c__[i__ + j * c_dim1];
		}
	      else
		{
		  work[ij] = -c__[j + i__ * c_dim1];
		}
	    }
	  else
	    {
	      if (i__ >= j)
		{
		  work[ij] = -c__[i__ + j * c_dim1];
		}
	      else
		{
		  work[ij] = -c__[j + i__ * c_dim1];
		}
	    }
	  ij = n2 * n2 + (*n + j - 1) * n2 + i__;
	  if (!lower)
	    {
	      if (i__ <= j)
		{
		  work[ij] = d__[i__ + j * d_dim1];
		}
	      else
		{
		  work[ij] = d__[j + i__ * d_dim1];
		}
	    }
	  else
	    {
	      if (i__ >= j)
		{
		  work[ij] = d__[i__ + j * d_dim1];
		}
	      else
		{
		  work[ij] = d__[j + i__ * d_dim1];
		}
	    }
	  ij = n2 * n2 + (*n + j - 1) * n2 + *n + i__;
	  if (notrna)
	    {
	      work[ij] = a[j + i__ * a_dim1];
	    }
	  else
	    {
	      work[ij] = a[i__ + j * a_dim1];
	    }
	  /* L10: */
	}
      /* L20: */
    }
  C2F (dlaset) ("Full", n, n, &c_b83, &c_b83, &work[n2 * *n + 1], &n2, 4L);
  C2F (dlaset) ("Full", n, n, &c_b83, &c_b83, &work[n2 * n2 + *n + 1],
		&n2, 4L);
  C2F (dlaset) ("Full", n, n, &c_b83, &c_b82, &work[n2 * *n + *n + 1],
		&n2, 4L);
  C2F (dlaset) ("Full", n, n, &c_b83, &c_b82, &work[n2 * n2 + 1], &n2, 4L);
  /* 
   *    Scale the matrix pencil 
   * 
   */
  cnorm2 = sqrt (cnorm);
  dnorm2 = sqrt (dnorm);
  iscl = 0;
  if (cnorm2 > dnorm2 && dnorm2 > 0.)
    {
      C2F (dlascl) ("G", &c__0, &c__0, &cnorm2, &dnorm2, n, n,
		    &work[*n + 1], &n2, &info2, 1L);
      C2F (dlascl) ("G", &c__0, &c__0, &dnorm2, &cnorm2, n, n,
		    &work[n2 * n2 + n2 * *n + 1], &n2, &info2, 1L);
      iscl = 1;
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa = *n * 12 * *n + *n * 6;
  ialfar = (n2 << 1) * n2;
  ialfai = ialfar + n2;
  ibeta = ialfai + n2;
  ivs = ibeta + n2;
  iwrk = ivs + n2 * n2;
  /* 
   *    Compute the generalized Schur factorization of the matrix pencil 
   * 
   */
  i__1 = *lwork - iwrk;
  C2F (dgges) ("N", "V", "S", nsp_ctrlpack_selmlo, &n2, &work[1], &n2,
	       &work[n2 * n2 + 1], &n2, &sdim, &work[ialfar + 1],
	       &work[ialfai + 1], &work[ibeta + 1], &work[ivs + 1],
	       &n2, &work[ivs + 1], &n2, &work[iwrk + 1], &i__1,
	       &bwork[1], &info2, 1L, 1L, 1L);
  if (info2 > 0 && info2 <= n2 + 1)
    {
      *info = 1;
      return 0;
    }
  else if (info2 == n2 + 2)
    {
      *info = 2;
      return 0;
    }
  else if (info2 == n2 + 3)
    {
      *info = 3;
      return 0;
    }
  else if (sdim != *n)
    {
      *info = 4;
      return 0;
    }
  lwamax = lwa + (int) work[iwrk + 1];
  /* 
   *    Store the matrices V11 and V21 
   * 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ij = (j - 1) * *n + i__;
	  iv = (i__ - 1) * n2 + ivs + j;
	  work[ij] = work[iv];
	  ij = (j - 1) * *n + (*n << 1) * *n + i__;
	  iv = (i__ - 1) * n2 + ivs + *n + j;
	  work[ij] = work[iv];
	  /* L30: */
	}
      /* L40: */
    }
  /* 
   *    Workspace usage 
   * 
   */
  iaf = *n * *n;
  ib = iaf + *n * *n;
  ir = ib + *n * *n;
  ic = ir + *n;
  ifr = ic + *n;
  ibr = ifr + *n;
  iwrk = ibr + *n;
  /* 
   *    Compute the solution matrix X 
   * 
   */
  C2F (dgesvx) ("E", "N", n, n, &work[1], n, &work[iaf + 1], n,
		&iwork[1], equed, &work[ir + 1], &work[ic + 1],
		&work[ib + 1], n, &x[x_offset], ldx, rcond,
		&work[ifr + 1], &work[ibr + 1], &work[iwrk + 1],
		&iwork[*n + 1], &info2, 1L, 1L, 1L);
  if (info2 > 0)
    {
      *info = 5;
      return 0;
    }
  /* 
   *    Symmetrize the solution 
   * 
   */
  if (*n > 1)
    {
      i__1 = *n - 1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  i__2 = *n;
	  for (j = i__ + 1; j <= i__2; ++j)
	    {
	      temp = (x[i__ + j * x_dim1] + x[j + i__ * x_dim1]) / 2.;
	      x[i__ + j * x_dim1] = temp;
	      x[j + i__ * x_dim1] = temp;
	      /* L50: */
	    }
	  /* L60: */
	}
    }
  /* 
   *    Undo scaling for the solution matrix 
   * 
   */
  if (iscl == 1)
    {
      C2F (dlascl) ("G", &c__0, &c__0, &dnorm2, &cnorm2, n, n,
		    &x[x_offset], ldx, &info2, 1L);
    }
  /* 
   *    Workspace usage 
   * 
   */
  lwa = *n * 3 * *n + *n;
  iu = *n * *n;
  iwferr = iu + *n * *n;
  iac = iwferr + *n;
  iwrk = iac + *n * *n;
  /* 
   *    Estimate the reciprocal condition number 
   * 
   */
  i__1 = *lwork - iwrk;
  nsp_ctrlpack_ricdrc (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &d__[d_offset], ldd, &x[x_offset], ldx, rcond,
		       &work[iac + 1], n, &work[1], n, &work[iu + 1], n,
		       &wr[1], &wi[1], &work[iwferr + 1], &work[iwrk + 1],
		       &i__1, &iwork[1], &info2, 1L, 1L);
  if (info2 > 0)
    {
      *info = 6;
      return 0;
    }
  lwa += (int) work[iwrk + 1];
  lwamax = Max (lwa, lwamax);
  /* 
   *    Return if the equation is singular 
   * 
   */
  if (*rcond == 0.)
    {
      *ferr = 1.;
      return 0;
    }
  /* 
   *    Estimate the bound on the forward error 
   * 
   */
  i__1 = *lwork - iwrk;
  nsp_ctrlpack_ricdfr (trana, n, &a[a_offset], lda, uplo, &c__[c_offset], ldc,
		       &x[x_offset], ldx, &work[iac + 1], n, &work[1], n,
		       &work[iu + 1], n, &work[iwferr + 1], ferr,
		       &work[iwrk + 1], &i__1, &iwork[1], &info2, 1L, 1L);
  lwa = *n * 9 * *n + *n * 3;
  lwamax = Max (lwa, lwamax);
  work[1] = (double) lwamax;
  return 0;
  /* 
   *    End of RICDSL 
   * 
   */
}				/* ricdsl_ */

/* 
 * -- LISPACK auxiliary routine (version 3.0) -- 
 *    Tech. University of Sofia 
 *    September 22, 1999 
 * 
 *    Purpose 
 *    ======= 
 * 
 *    SELMLO is used to select eigenvalues with modules less than one 
 *    to sort to the top left of the generalized Schur form of the 
 *    matrix pencil in solving discrete-time matrix algebraic Riccati 
 *    equations 
 */

int
nsp_ctrlpack_selmlo (const double *alphar, const double *alphai, const double *beta)
{
  int ret_val;
  ret_val = C2F (dlapy2) (alphar, alphai) < Abs (*beta);
  return ret_val;
}

static int nsp_ctrlpack_voiddummy (const double *ar, const double *ai)
{
  return FALSE;
}
