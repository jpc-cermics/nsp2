/* dbsknu.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__15 = 15;
static int c__5 = 5;
static int c__3 = 3;
static int c__2 = 2;
static int c__1 = 1;

/*DECK DBSKNU 
 */
int
nsp_calpack_dbsknu (double *x, double *fnu, int *kode, int *n, double *y,
		    int *nz)
{
  /* Initialized data */

  static double x1 = 2.;
  static double x2 = 17.;
  static double pi = 3.14159265358979;
  static double rthpi = 1.2533141373155;
  static double cc[8] =
    { .577215664901533, -.0420026350340952, -.0421977345555443,
    .007218943246663, -2.152416741149e-4, -2.01348547807e-5, 1.133027232e-6,
    6.116095e-9
  };

  /* System generated locals */
  int i__1;


  /* Local variables */
  double coef, elim, flrx, a[160], b[160], f;
  int i__, j, k;
  double p, q;
  int iflag;
  double s;
  int koded;
  double a1, a2, etest, g1, g2;
  double p1;
  double p2, s1, s2, t1, t2, fc, ak, bk, ck, dk, fk;
  int kk;
  double cx;
  int nn;
  double ex, tm, pt, st, rx;
  double fhs, fks, dnu, fmu;
  int inu;
  double sqk, tol, smu, dnu2;

  /****BEGIN PROLOGUE  DBSKNU 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to DBESK 
   ****LIBRARY   SLATEC 
   ****TYPE      DOUBLE PRECISION (BESKNU-S, DBSKNU-D) 
   ****AUTHOR  Amos, D. E., (SNLA) 
   ****DESCRIPTION 
   * 
   *    Abstract  **** A DOUBLE PRECISION routine **** 
   *        DBSKNU computes N member sequences of K Bessel functions 
   *        K/SUB(FNU+I-1)/(X), I=1,N for non-negative orders FNU and 
   *        positive X. Equations of the references are implemented on 
   *        small orders DNU for K/SUB(DNU)/(X) and K/SUB(DNU+1)/(X). 
   *        Forward recursion with the three term recursion relation 
   *        generates higher orders FNU+I-1, I=1,...,N. The parameter 
   *        KODE permits K/SUB(FNU+I-1)/(X) values or scaled values 
   *        EXP(X)*K/SUB(FNU+I-1)/(X), I=1,N to be returned. 
   * 
   *        To start the recursion FNU is normalized to the interval 
   *        -0.5.LE.DNU.LT.0.5. A special form of the power series is 
   *        implemented on 0.LT.X.LE.X1 while the Miller algorithm for the 
   *        K Bessel function in terms of the confluent hypergeometric 
   *        function U(FNU+0.5,2*FNU+1,X) is implemented on X1.LT.X.LE.X2. 
   *        For X.GT.X2, the asymptotic expansion for large X is used. 
   *        When FNU is a half odd int, a special formula for 
   *        DNU=-0.5 and DNU+1.0=0.5 is used to start the recursion. 
   * 
   *        The maximum number of significant digits obtainable 
   *        is the smaller of 14 and the number of digits carried in 
   *        DOUBLE PRECISION arithmetic. 
   * 
   *        DBSKNU assumes that a significant digit SINH function is 
   *        available. 
   * 
   *    Description of Arguments 
   * 
   *        INPUT      X,FNU are DOUBLE PRECISION 
   *          X      - X.GT.0.0D0 
   *          FNU    - Order of initial K function, FNU.GE.0.0D0 
   *          N      - Number of members of the sequence, N.GE.1 
   *          KODE   - A parameter to indicate the scaling option 
   *                   KODE= 1  returns 
   *                            Y(I)=       K/SUB(FNU+I-1)/(X) 
   *                                 I=1,...,N 
   *                       = 2  returns 
   *                            Y(I)=EXP(X)*K/SUB(FNU+I-1)/(X) 
   *                                 I=1,...,N 
   * 
   *        OUTPUT     Y is DOUBLE PRECISION 
   *          Y      - A vector whose first N components contain values 
   *                   for the sequence 
   *                   Y(I)=       K/SUB(FNU+I-1)/(X), I=1,...,N or 
   *                   Y(I)=EXP(X)*K/SUB(FNU+I-1)/(X), I=1,...,N 
   *                   depending on KODE 
   *          NZ     - Number of components set to zero due to 
   *                   underflow, 
   *                   NZ= 0   , normal return 
   *                   NZ.NE.0 , first NZ components of Y set to zero 
   *                             due to underflow, Y(I)=0.0D0,I=1,...,NZ 
   * 
   *    Error Conditions 
   *        Improper input arguments - a fatal error 
   *        Overflow - a fatal error 
   *        Underflow with KODE=1 - a non-fatal error (NZ.NE.0) 
   * 
   ****SEE ALSO  DBESK 
   ****REFERENCES  N. M. Temme, On the numerical evaluation of the modified 
   *                Bessel function of the third kind, Journal of 
   *                Computational Physics 19, (1975), pp. 324-337. 
   ****ROUTINES CALLED  D1MACH, DGAMMA, I1MACH, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  790201  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   *  900326  Removed duplicate information from DESCRIPTION section. 
   *          (WRB) 
   *  900328  Added TYPE section.  (WRB) 
   *  900727  Added EXTERNAL statement.  (WRB) 
   *  910408  Updated the AUTHOR and REFERENCES sections.  (WRB) 
   *  920501  Reformatted the REFERENCES section.  (WRB) 
   ****END PROLOGUE  DBSKNU 
   * 
   */
  /* Parameter adjustments */
  --y;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  DBSKNU 
   */
  kk = -nsp_calpack_i1mach (&c__15);
  elim = (kk * nsp_calpack_d1mach (&c__5) - 3.) * 2.303;
  ak = nsp_calpack_d1mach (&c__3);
  tol = Max (ak, 1e-15);
  if (*x <= 0.)
    {
      goto L350;
    }
  if (*fnu < 0.)
    {
      goto L360;
    }
  if (*kode < 1 || *kode > 2)
    {
      goto L370;
    }
  if (*n < 1)
    {
      goto L380;
    }
  *nz = 0;
  iflag = 0;
  koded = *kode;
  rx = 2. / *x;
  inu = (int) (*fnu + .5);
  dnu = *fnu - inu;
  if (Abs (dnu) == .5)
    {
      goto L120;
    }
  dnu2 = 0.;
  if (Abs (dnu) < tol)
    {
      goto L10;
    }
  dnu2 = dnu * dnu;
L10:
  if (*x > x1)
    {
      goto L120;
    }
  /* 
   *    SERIES FOR X.LE.X1 
   * 
   */
  a1 = 1. - dnu;
  a2 = dnu + 1.;
  t1 = 1. / nsp_calpack_dgamma (&a1);
  t2 = 1. / nsp_calpack_dgamma (&a2);
  if (Abs (dnu) > .1)
    {
      goto L40;
    }
  /*    SERIES FOR F0 TO RESOLVE INDETERMINACY FOR SMALL ABS(DNU) 
   */
  s = cc[0];
  ak = 1.;
  for (k = 2; k <= 8; ++k)
    {
      ak *= dnu2;
      tm = cc[k - 1] * ak;
      s += tm;
      if (Abs (tm) < tol)
	{
	  goto L30;
	}
      /* L20: */
    }
L30:
  g1 = -s;
  goto L50;
L40:
  g1 = (t1 - t2) / (dnu + dnu);
L50:
  g2 = (t1 + t2) * .5;
  smu = 1.;
  fc = 1.;
  flrx = log (rx);
  fmu = dnu * flrx;
  if (dnu == 0.)
    {
      goto L60;
    }
  fc = dnu * pi;
  fc /= sin (fc);
  if (fmu != 0.)
    {
      smu = sinh (fmu) / fmu;
    }
L60:
  f = fc * (g1 * cosh (fmu) + g2 * flrx * smu);
  fc = exp (fmu);
  p = fc * .5 / t2;
  q = .5 / (fc * t1);
  ak = 1.;
  ck = 1.;
  bk = 1.;
  s1 = f;
  s2 = p;
  if (inu > 0 || *n > 1)
    {
      goto L90;
    }
  if (*x < tol)
    {
      goto L80;
    }
  cx = *x * *x * .25;
L70:
  f = (ak * f + p + q) / (bk - dnu2);
  p /= ak - dnu;
  q /= ak + dnu;
  ck = ck * cx / ak;
  t1 = ck * f;
  s1 += t1;
  bk = bk + ak + ak + 1.;
  ak += 1.;
  s = Abs (t1) / (Abs (s1) + 1.);
  if (s > tol)
    {
      goto L70;
    }
L80:
  y[1] = s1;
  if (koded == 1)
    {
      return 0;
    }
  y[1] = s1 * exp (*x);
  return 0;
L90:
  if (*x < tol)
    {
      goto L110;
    }
  cx = *x * *x * .25;
L100:
  f = (ak * f + p + q) / (bk - dnu2);
  p /= ak - dnu;
  q /= ak + dnu;
  ck = ck * cx / ak;
  t1 = ck * f;
  s1 += t1;
  t2 = ck * (p - ak * f);
  s2 += t2;
  bk = bk + ak + ak + 1.;
  ak += 1.;
  s = Abs (t1) / (Abs (s1) + 1.) + Abs (t2) / (Abs (s2) + 1.);
  if (s > tol)
    {
      goto L100;
    }
L110:
  s2 *= rx;
  if (koded == 1)
    {
      goto L170;
    }
  f = exp (*x);
  s1 *= f;
  s2 *= f;
  goto L170;
L120:
  coef = rthpi / sqrt (*x);
  if (koded == 2)
    {
      goto L130;
    }
  if (*x > elim)
    {
      goto L330;
    }
  coef *= exp (-(*x));
L130:
  if (Abs (dnu) == .5)
    {
      goto L340;
    }
  if (*x > x2)
    {
      goto L280;
    }
  /* 
   *    MILLER ALGORITHM FOR X1.LT.X.LE.X2 
   * 
   */
  etest = cos (pi * dnu) / (pi * *x * tol);
  fks = 1.;
  fhs = .25;
  fk = 0.;
  ck = *x + *x + 2.;
  p1 = 0.;
  p2 = 1.;
  k = 0;
L140:
  ++k;
  fk += 1.;
  ak = (fhs - dnu2) / (fks + fk);
  bk = ck / (fk + 1.);
  pt = p2;
  p2 = bk * p2 - ak * p1;
  p1 = pt;
  a[k - 1] = ak;
  b[k - 1] = bk;
  ck += 2.;
  fks = fks + fk + fk + 1.;
  fhs = fhs + fk + fk;
  if (etest > fk * p1)
    {
      goto L140;
    }
  kk = k;
  s = 1.;
  p1 = 0.;
  p2 = 1.;
  i__1 = k;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      pt = p2;
      p2 = (b[kk - 1] * p2 - p1) / a[kk - 1];
      p1 = pt;
      s += p2;
      --kk;
      /* L150: */
    }
  s1 = coef * (p2 / s);
  if (inu > 0 || *n > 1)
    {
      goto L160;
    }
  goto L200;
L160:
  s2 = s1 * (*x + dnu + .5 - p1 / p2) / *x;
  /* 
   *    FORWARD RECURSION ON THE THREE TERM RECURSION RELATION 
   * 
   */
L170:
  ck = (dnu + dnu + 2.) / *x;
  if (*n == 1)
    {
      --inu;
    }
  if (inu > 0)
    {
      goto L180;
    }
  if (*n > 1)
    {
      goto L200;
    }
  s1 = s2;
  goto L200;
L180:
  i__1 = inu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      st = s2;
      s2 = ck * s2 + s1;
      s1 = st;
      ck += rx;
      /* L190: */
    }
  if (*n == 1)
    {
      s1 = s2;
    }
L200:
  if (iflag == 1)
    {
      goto L220;
    }
  y[1] = s1;
  if (*n == 1)
    {
      return 0;
    }
  y[2] = s2;
  if (*n == 2)
    {
      return 0;
    }
  i__1 = *n;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      y[i__] = ck * y[i__ - 1] + y[i__ - 2];
      ck += rx;
      /* L210: */
    }
  return 0;
  /*    IFLAG=1 CASES 
   */
L220:
  s = -(*x) + log (s1);
  y[1] = 0.;
  *nz = 1;
  if (s < -elim)
    {
      goto L230;
    }
  y[1] = exp (s);
  *nz = 0;
L230:
  if (*n == 1)
    {
      return 0;
    }
  s = -(*x) + log (s2);
  y[2] = 0.;
  ++(*nz);
  if (s < -elim)
    {
      goto L240;
    }
  --(*nz);
  y[2] = exp (s);
L240:
  if (*n == 2)
    {
      return 0;
    }
  kk = 2;
  if (*nz < 2)
    {
      goto L260;
    }
  i__1 = *n;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      kk = i__;
      st = s2;
      s2 = ck * s2 + s1;
      s1 = st;
      ck += rx;
      s = -(*x) + log (s2);
      ++(*nz);
      y[i__] = 0.;
      if (s < -elim)
	{
	  goto L250;
	}
      y[i__] = exp (s);
      --(*nz);
      goto L260;
    L250:
      ;
    }
  return 0;
L260:
  if (kk == *n)
    {
      return 0;
    }
  s2 = s2 * ck + s1;
  ck += rx;
  ++kk;
  y[kk] = exp (-(*x) + log (s2));
  if (kk == *n)
    {
      return 0;
    }
  ++kk;
  i__1 = *n;
  for (i__ = kk; i__ <= i__1; ++i__)
    {
      y[i__] = ck * y[i__ - 1] + y[i__ - 2];
      ck += rx;
      /* L270: */
    }
  return 0;
  /* 
   *    ASYMPTOTIC EXPANSION FOR LARGE X, X.GT.X2 
   * 
   *    IFLAG=0 MEANS NO UNDERFLOW OCCURRED 
   *    IFLAG=1 MEANS AN UNDERFLOW OCCURRED- COMPUTATION PROCEEDS WITH 
   *    KODED=2 AND A TEST FOR ON SCALE VALUES IS MADE DURING FORWARD 
   *    RECURSION 
   */
L280:
  nn = 2;
  if (inu == 0 && *n == 1)
    {
      nn = 1;
    }
  dnu2 = dnu + dnu;
  fmu = 0.;
  if (Abs (dnu2) < tol)
    {
      goto L290;
    }
  fmu = dnu2 * dnu2;
L290:
  ex = *x * 8.;
  s2 = 0.;
  i__1 = nn;
  for (k = 1; k <= i__1; ++k)
    {
      s1 = s2;
      s = 1.;
      ak = 0.;
      ck = 1.;
      sqk = 1.;
      dk = ex;
      for (j = 1; j <= 30; ++j)
	{
	  ck = ck * (fmu - sqk) / dk;
	  s += ck;
	  dk += ex;
	  ak += 8.;
	  sqk += ak;
	  if (Abs (ck) < tol)
	    {
	      goto L310;
	    }
	  /* L300: */
	}
    L310:
      s2 = s * coef;
      fmu = fmu + dnu * 8. + 4.;
      /* L320: */
    }
  if (nn > 1)
    {
      goto L170;
    }
  s1 = s2;
  goto L200;
L330:
  koded = 2;
  iflag = 1;
  goto L120;
  /* 
   *    FNU=HALF ODD INT CASE 
   * 
   */
L340:
  s1 = coef;
  s2 = coef;
  goto L170;
  /* 
   * 
   */
L350:
  nsp_calpack_xermsg ("SLATEC", "DBSKNU", "X NOT GREATER THAN ZERO", &c__2,
		      &c__1, 6L, 6L, 23L);
  return 0;
L360:
  nsp_calpack_xermsg ("SLATEC", "DBSKNU", "FNU NOT ZERO OR POSITIVE", &c__2,
		      &c__1, 6L, 6L, 24L);
  return 0;
L370:
  nsp_calpack_xermsg ("SLATEC", "DBSKNU", "KODE NOT 1 OR 2", &c__2, &c__1,
		      6L, 6L, 15L);
  return 0;
L380:
  nsp_calpack_xermsg ("SLATEC", "DBSKNU", "N NOT GREATER THAN 0", &c__2,
		      &c__1, 6L, 6L, 20L);
  return 0;
}				/* dbsknu_ */
#endif
