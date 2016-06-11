/* dbesi.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__3 = 3;
static int c__15 = 15;
static int c__5 = 5;
static int c__14 = 14;

/*DECK DBESI 
 */
int
nsp_calpack_dbesi (double *x, double *alpha, int *kode, int *n, double *y,
		   int *nz, int *ierr)
{
  /* Initialized data */

  static double rttpi = .398942280401433;
  static int inlim = 80;

  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  double earg;
  int ialp;
  double elim, atol, temp[3];
  int i__, k;
  double s, t, z__, flgik, tolln;
  int i1;
  double s1, s2, t2, ak, ap, ra, fn, ta;
  int kk, in, km;
  double tb;
  int is, nn;
  double dx;
  int kt, ns;
  double tm;
  double sx;
  double xo2, ain, akm, arg, dfn, fnf, fni, gln, ans, dtm, tfn, fnu, tol, etx,
    trx, fnp1, xo2l, sxo2;

  /****BEGIN PROLOGUE  DBESI 
   ****PURPOSE  Compute an N member sequence of I Bessel functions 
   *           I/SUB(ALPHA+K-1)/(X), K=1,...,N or scaled Bessel functions 
   *           EXP(-X)*I/SUB(ALPHA+K-1)/(X), K=1,...,N for nonnegative 
   *           ALPHA and X. 
   ****LIBRARY   SLATEC 
   ****CATEGORY  C10B3 
   ****TYPE      DOUBLE PRECISION (BESI-S, DBESI-D) 
   ****KEYWORDS  I BESSEL FUNCTION, SPECIAL FUNCTIONS 
   ****AUTHOR  Amos, D. E., (SNLA) 
   *          Daniel, S. L., (SNLA) 
   ****DESCRIPTION 
   * 
   *    Abstract  **** a double precision routine **** 
   *        DBESI computes an N member sequence of I Bessel functions 
   *        I/sub(ALPHA+K-1)/(X), K=1,...,N or scaled Bessel functions 
   *        EXP(-X)*I/sub(ALPHA+K-1)/(X), K=1,...,N for nonnegative ALPHA 
   *        and X.  A combination of the power series, the asymptotic 
   *        expansion for X to infinity, and the uniform asymptotic 
   *        expansion for NU to infinity are applied over subdivisions of 
   *        the (NU,X) plane.  For values not covered by one of these 
   *        formulae, the order is incremented by an int so that one 
   *        of these formulae apply.  Backward recursion is used to reduce 
   *        orders by int values.  The asymptotic expansion for X to 
   *        infinity is used only when the entire sequence (specifically 
   *        the last member) lies within the region covered by the 
   *        expansion.  Leading terms of these expansions are used to test 
   *        for over or underflow where appropriate.  If a sequence is 
   *        requested and the last member would underflow, the result is 
   *        set to zero and the next lower order tried, etc., until a 
   *        member comes on scale or all are set to zero.  An overflow 
   *        cannot occur with scaling. 
   * 
   *        The maximum number of significant digits obtainable 
   *        is the smaller of 14 and the number of digits carried in 
   *        double precision arithmetic. 
   * 
   *    Description of Arguments 
   * 
   *        Input      X,ALPHA are double precision 
   *          X      - X .GE. 0.0D0 
   *          ALPHA  - order of first member of the sequence, 
   *                   ALPHA .GE. 0.0D0 
   *          KODE   - a parameter to indicate the scaling option 
   *                   KODE=1 returns 
   *                          Y(K)=        I/sub(ALPHA+K-1)/(X), 
   *                               K=1,...,N 
   *                   KODE=2 returns 
   *                          Y(K)=EXP(-X)*I/sub(ALPHA+K-1)/(X), 
   *                               K=1,...,N 
   *          N      - number of members in the sequence, N .GE. 1 
   * 
   *        Output     Y is double precision 
   *          Y      - a vector whose first N components contain 
   *                   values for I/sub(ALPHA+K-1)/(X) or scaled 
   *                   values for EXP(-X)*I/sub(ALPHA+K-1)/(X), 
   *                   K=1,...,N depending on KODE 
   *          NZ     - number of components of Y set to zero due to 
   *                   underflow, 
   *                   NZ=0   , normal return, computation completed 
   *                   NZ .NE. 0, last NZ components of Y set to zero, 
   *                            Y(K)=0.0D0, K=N-NZ+1,...,N. 
   * 
   *    Error Conditions 
   *        Improper input arguments - a fatal error 
   *        Overflow with KODE=1 - a fatal error 
   *        Underflow - a non-fatal error(NZ .NE. 0) 
   * 
   ****REFERENCES  D. E. Amos, S. L. Daniel and M. K. Weston, CDC 6600 
   *                subroutines IBESS and JBESS for Bessel functions 
   *                I(NU,X) and J(NU,X), X .GE. 0, NU .GE. 0, ACM 
   *                Transactions on Mathematical Software 3, (1977), 
   *                pp. 76-92. 
   *              F. W. J. Olver, Tables of Bessel Functions of Moderate 
   *                or Large Orders, NPL Mathematical Tables 6, Her 
   *                Majesty's Stationery Office, London, 1962. 
   ****ROUTINES CALLED  D1MACH, DASYIK, DLNGAM, I1MACH, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  750101  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  890911  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   *  900326  Removed duplicate information from DESCRIPTION section. 
   *          (WRB) 
   *  920501  Reformatted the REFERENCES section.  (WRB) 
   ****END PROLOGUE  DBESI 
   * 
   */
  /* Parameter adjustments */
  --y;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  DBESI 
   */
  *ierr = 0;
  *nz = 0;
  kt = 1;
  /*    I1MACH(15) REPLACES I1MACH(12) IN A DOUBLE PRECISION CODE 
   *    I1MACH(14) REPLACES I1MACH(11) IN A DOUBLE PRECISION CODE 
   */
  ra = nsp_calpack_d1mach (&c__3);
  tol = Max (ra, 1e-15);
  i1 = -nsp_calpack_i1mach (&c__15);
  gln = nsp_calpack_d1mach (&c__5);
  elim = (i1 * gln - 3.) * 2.303;
  /*    TOLLN = -LN(TOL) 
   */
  i1 = nsp_calpack_i1mach (&c__14) + 1;
  tolln = gln * 2.303 * i1;
  tolln = Min (tolln, 34.5388);
  if (*n - 1 < 0)
    {
      goto L590;
    }
  else if (*n - 1 == 0)
    {
      goto L10;
    }
  else
    {
      goto L20;
    }
L10:
  kt = 2;
L20:
  nn = *n;
  if (*kode < 1 || *kode > 2)
    {
      goto L570;
    }
  if (*x < 0.)
    {
      goto L600;
    }
  else if (*x == 0.)
    {
      goto L30;
    }
  else
    {
      goto L80;
    }
L30:
  if (*alpha < 0.)
    {
      goto L580;
    }
  else if (*alpha == 0.)
    {
      goto L40;
    }
  else
    {
      goto L50;
    }
L40:
  y[1] = 1.;
  if (*n == 1)
    {
      return 0;
    }
  i1 = 2;
  goto L60;
L50:
  i1 = 1;
L60:
  i__1 = *n;
  for (i__ = i1; i__ <= i__1; ++i__)
    {
      y[i__] = 0.;
      /* L70: */
    }
  return 0;
L80:
  if (*alpha < 0.)
    {
      goto L580;
    }
  /* 
   */
  ialp = (int) (*alpha);
  fni = (double) (ialp + *n - 1);
  fnf = *alpha - ialp;
  dfn = fni + fnf;
  fnu = dfn;
  in = 0;
  xo2 = *x * .5;
  sxo2 = xo2 * xo2;
  etx = (double) (*kode - 1);
  sx = etx * *x;
  /* 
   *    DECISION TREE FOR REGION WHERE SERIES, ASYMPTOTIC EXPANSION FOR X 
   *    TO INFINITY AND ASYMPTOTIC EXPANSION FOR NU TO INFINITY ARE 
   *    APPLIED. 
   * 
   */
  if (sxo2 <= fnu + 1.)
    {
      goto L90;
    }
  if (*x <= 12.)
    {
      goto L110;
    }
  fn = fnu * .55 * fnu;
  fn = Max (17., fn);
  if (*x >= fn)
    {
      goto L430;
    }
  /*Computing MAX 
   */
  d__1 = 36. - fnu;
  ans = Max (d__1, 0.);
  ns = (int) ans;
  fni += ns;
  dfn = fni + fnf;
  fn = dfn;
  is = kt;
  km = *n - 1 + ns;
  if (km > 0)
    {
      is = 3;
    }
  goto L120;
L90:
  fn = fnu;
  fnp1 = fn + 1.;
  xo2l = log (xo2);
  is = kt;
  if (*x <= .5)
    {
      goto L230;
    }
  ns = 0;
L100:
  fni += ns;
  dfn = fni + fnf;
  fn = dfn;
  fnp1 = fn + 1.;
  is = kt;
  if (*n - 1 + ns > 0)
    {
      is = 3;
    }
  goto L230;
L110:
  xo2l = log (xo2);
  ns = (int) (sxo2 - fnu);
  goto L100;
L120:
  /* 
   *    OVERFLOW TEST ON UNIFORM ASYMPTOTIC EXPANSION 
   * 
   */
  if (*kode == 2)
    {
      goto L130;
    }
  if (*alpha < 1.)
    {
      goto L150;
    }
  z__ = *x / *alpha;
  ra = sqrt (z__ * z__ + 1.);
  gln = log ((ra + 1.) / z__);
  t = ra * (1. - etx) + etx / (z__ + ra);
  arg = *alpha * (t - gln);
  if (arg > elim)
    {
      goto L610;
    }
  if (km == 0)
    {
      goto L140;
    }
L130:
  /* 
   *    UNDERFLOW TEST ON UNIFORM ASYMPTOTIC EXPANSION 
   * 
   */
  z__ = *x / fn;
  ra = sqrt (z__ * z__ + 1.);
  gln = log ((ra + 1.) / z__);
  t = ra * (1. - etx) + etx / (z__ + ra);
  arg = fn * (t - gln);
L140:
  if (arg < -elim)
    {
      goto L280;
    }
  goto L190;
L150:
  if (*x > elim)
    {
      goto L610;
    }
  goto L130;
  /* 
   *    UNIFORM ASYMPTOTIC EXPANSION FOR NU TO INFINITY 
   * 
   */
L160:
  if (km != 0)
    {
      goto L170;
    }
  y[1] = temp[2];
  return 0;
L170:
  temp[0] = temp[2];
  in = ns;
  kt = 1;
  i1 = 0;
L180:
  is = 2;
  fni += -1.;
  dfn = fni + fnf;
  fn = dfn;
  if (i1 == 2)
    {
      goto L350;
    }
  z__ = *x / fn;
  ra = sqrt (z__ * z__ + 1.);
  gln = log ((ra + 1.) / z__);
  t = ra * (1. - etx) + etx / (z__ + ra);
  arg = fn * (t - gln);
L190:
  i1 = (i__1 = 3 - is, Abs (i__1));
  i1 = Max (i1, 1);
  flgik = 1.;
  nsp_calpack_dasyik (x, &fn, kode, &flgik, &ra, &arg, &i1, &temp[is - 1]);
  switch (is)
    {
    case 1:
      goto L180;
    case 2:
      goto L350;
    case 3:
      goto L510;
    }
  /* 
   *    SERIES FOR (X/2)**2.LE.NU+1 
   * 
   */
L230:
  gln = nsp_calpack_dlngam (&fnp1);
  arg = fn * xo2l - gln - sx;
  if (arg < -elim)
    {
      goto L300;
    }
  earg = exp (arg);
L240:
  s = 1.;
  if (*x < tol)
    {
      goto L260;
    }
  ak = 3.;
  t2 = 1.;
  t = 1.;
  s1 = fn;
  for (k = 1; k <= 17; ++k)
    {
      s2 = t2 + s1;
      t = t * sxo2 / s2;
      s += t;
      if (Abs (t) < tol)
	{
	  goto L260;
	}
      t2 += ak;
      ak += 2.;
      s1 += fn;
      /* L250: */
    }
L260:
  temp[is - 1] = s * earg;
  switch (is)
    {
    case 1:
      goto L270;
    case 2:
      goto L350;
    case 3:
      goto L500;
    }
L270:
  earg = earg * fn / xo2;
  fni += -1.;
  dfn = fni + fnf;
  fn = dfn;
  is = 2;
  goto L240;
  /* 
   *    SET UNDERFLOW VALUE AND UPDATE PARAMETERS 
   * 
   */
L280:
  y[nn] = 0.;
  --nn;
  fni += -1.;
  dfn = fni + fnf;
  fn = dfn;
  if (nn - 1 < 0)
    {
      goto L340;
    }
  else if (nn - 1 == 0)
    {
      goto L290;
    }
  else
    {
      goto L130;
    }
L290:
  kt = 2;
  is = 2;
  goto L130;
L300:
  y[nn] = 0.;
  --nn;
  fnp1 = fn;
  fni += -1.;
  dfn = fni + fnf;
  fn = dfn;
  if (nn - 1 < 0)
    {
      goto L340;
    }
  else if (nn - 1 == 0)
    {
      goto L310;
    }
  else
    {
      goto L320;
    }
L310:
  kt = 2;
  is = 2;
L320:
  if (sxo2 <= fnp1)
    {
      goto L330;
    }
  goto L130;
L330:
  arg = arg - xo2l + log (fnp1);
  if (arg < -elim)
    {
      goto L300;
    }
  goto L230;
L340:
  *nz = *n - nn;
  return 0;
  /* 
   *    BACKWARD RECURSION SECTION 
   * 
   */
L350:
  *nz = *n - nn;
L360:
  if (kt == 2)
    {
      goto L420;
    }
  s1 = temp[0];
  s2 = temp[1];
  trx = 2. / *x;
  dtm = fni;
  tm = (dtm + fnf) * trx;
  if (in == 0)
    {
      goto L390;
    }
  /*    BACKWARD RECUR TO INDEX ALPHA+NN-1 
   */
  i__1 = in;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      s = s2;
      s2 = tm * s2 + s1;
      s1 = s;
      dtm += -1.;
      tm = (dtm + fnf) * trx;
      /* L380: */
    }
  y[nn] = s1;
  if (nn == 1)
    {
      return 0;
    }
  y[nn - 1] = s2;
  if (nn == 2)
    {
      return 0;
    }
  goto L400;
L390:
  /*    BACKWARD RECUR FROM INDEX ALPHA+NN-1 TO ALPHA 
   */
  y[nn] = s1;
  y[nn - 1] = s2;
  if (nn == 2)
    {
      return 0;
    }
L400:
  k = nn + 1;
  i__1 = nn;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      --k;
      y[k - 2] = tm * y[k - 1] + y[k];
      dtm += -1.;
      tm = (dtm + fnf) * trx;
      /* L410: */
    }
  return 0;
L420:
  y[1] = temp[1];
  return 0;
  /* 
   *    ASYMPTOTIC EXPANSION FOR X TO INFINITY 
   * 
   */
L430:
  earg = rttpi / sqrt (*x);
  if (*kode == 2)
    {
      goto L440;
    }
  if (*x > elim)
    {
      goto L610;
    }
  earg *= exp (*x);
L440:
  etx = *x * 8.;
  is = kt;
  in = 0;
  fn = fnu;
L450:
  dx = fni + fni;
  tm = 0.;
  if (fni == 0. && Abs (fnf) < tol)
    {
      goto L460;
    }
  tm = fnf * 4. * (fni + fni + fnf);
L460:
  dtm = dx * dx;
  s1 = etx;
  trx = dtm - 1.;
  dx = -(trx + tm) / etx;
  t = dx;
  s = dx + 1.;
  atol = tol * Abs (s);
  s2 = 1.;
  ak = 8.;
  for (k = 1; k <= 25; ++k)
    {
      s1 += etx;
      s2 += ak;
      dx = dtm - s2;
      ap = dx + tm;
      t = -t * ap / s1;
      s += t;
      if (Abs (t) <= atol)
	{
	  goto L480;
	}
      ak += 8.;
      /* L470: */
    }
L480:
  temp[is - 1] = s * earg;
  if (is == 2)
    {
      goto L360;
    }
  is = 2;
  fni += -1.;
  dfn = fni + fnf;
  fn = dfn;
  goto L450;
  /* 
   *    BACKWARD RECURSION WITH NORMALIZATION BY 
   *    ASYMPTOTIC EXPANSION FOR NU TO INFINITY OR POWER SERIES. 
   * 
   */
L500:
  /*    COMPUTATION OF LAST ORDER FOR SERIES NORMALIZATION 
   *Computing MAX 
   */
  d__1 = 3. - fn;
  akm = Max (d__1, 0.);
  km = (int) akm;
  tfn = fn + km;
  ta = (gln + tfn - .9189385332 - .0833333333 / tfn) / (tfn + .5);
  ta = xo2l - ta;
  tb = -(1. - 1. / tfn) / tfn;
  ain = tolln / (-ta + sqrt (ta * ta - tolln * tb)) + 1.5;
  in = (int) ain;
  in += km;
  goto L520;
L510:
  /*    COMPUTATION OF LAST ORDER FOR ASYMPTOTIC EXPANSION NORMALIZATION 
   */
  t = 1. / (fn * ra);
  ain = tolln / (gln + sqrt (gln * gln + t * tolln)) + 1.5;
  in = (int) ain;
  if (in > inlim)
    {
      goto L160;
    }
L520:
  trx = 2. / *x;
  dtm = fni + in;
  tm = (dtm + fnf) * trx;
  ta = 0.;
  tb = tol;
  kk = 1;
L530:
  /* 
   *    BACKWARD RECUR UNINDEXED 
   * 
   */
  i__1 = in;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      s = tb;
      tb = tm * tb + ta;
      ta = s;
      dtm += -1.;
      tm = (dtm + fnf) * trx;
      /* L540: */
    }
  /*    NORMALIZATION 
   */
  if (kk != 1)
    {
      goto L550;
    }
  ta = ta / tb * temp[2];
  tb = temp[2];
  kk = 2;
  in = ns;
  if (ns != 0)
    {
      goto L530;
    }
L550:
  y[nn] = tb;
  *nz = *n - nn;
  if (nn == 1)
    {
      return 0;
    }
  tb = tm * tb + ta;
  k = nn - 1;
  y[k] = tb;
  if (nn == 2)
    {
      return 0;
    }
  dtm += -1.;
  tm = (dtm + fnf) * trx;
  km = k - 1;
  /* 
   *    BACKWARD RECUR INDEXED 
   * 
   */
  i__1 = km;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[k - 1] = tm * y[k] + y[k + 1];
      dtm += -1.;
      tm = (dtm + fnf) * trx;
      --k;
      /* L560: */
    }
  return 0;
  /* 
   * 
   * 
   */
L570:
  /*     CALL XERMSG ('SLATEC', 'DBESI', 
   *    +   'SCALING OPTION, KODE, NOT 1 OR 2.', 2, 1) 
   */
  *ierr = 1;
  return 0;
L580:
  /*     CALL XERMSG ('SLATEC', 'DBESI', 'ORDER, ALPHA, LESS THAN ZERO.', 
   *    +   2, 1) 
   */
  *ierr = 1;
  return 0;
L590:
  /*     CALL XERMSG ('SLATEC', 'DBESI', 'N LESS THAN ONE.', 2, 1) 
   */
  *ierr = 1;
  return 0;
L600:
  /*     CALL XERMSG ('SLATEC', 'DBESI', 'X LESS THAN ZERO.', 2, 1) 
   */
  *ierr = 1;
  return 0;
L610:
  /*     CALL XERMSG ('SLATEC', 'DBESI', 
   *    +   'OVERFLOW, X TOO LARGE FOR KODE = 1.', 6, 1) 
   */
  *ierr = 2;
  return 0;
}				/* dbesi_ */
#endif
