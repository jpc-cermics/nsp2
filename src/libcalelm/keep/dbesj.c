/* dbesj.f -- translated by f2c (version 19961017).
 *
 *
 */

#if 0
#include "calpack.h"

/* Table of constant values */

static int c__3 = 3;
static int c__14 = 14;
static int c__15 = 15;
static int c__5 = 5;
static int c__1 = 1;

/*DECK DBESJ 
 */
int
nsp_calpack_dbesj (double *x, double *alpha, int *n, double *y, int *nz,
		   int *ierr)
{
  /* Initialized data */

  static double rtwo = 1.34839972492648;
  static double pdf = .785398163397448;
  static double rttp = .797884560802865;
  static double pidt = 1.5707963267949;
  static double pp[4] = { 8.72909153935547, .26569393226503, .124578576865586,
    7.70133747430388e-4
  };
  static int inlim = 150;
  static double fnulim[2] = { 100., 60. };

  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  double coef, earg, relb;
  int ialp;
  double rden;
  int iflw;
  double slim, temp[3], rtol, elim1;
  int i__, k;
  double fidal, s, t;
  int idalp;
  double flgjy, rzden, tolln;
  int i1, i2;
  double s1, s2, t1, t2, ak, ap, fn, sa;
  int kk, in, km;
  double dalpha, sb, ta;
  int is, nn;
  double tb;
  int kt, ns;
  double tm, wk[7];
  double tx;
  double xo2, dfn, akm, arg, fnf, fni, gln, ans, dtm, tfn, fnu, tau, tol, etx,
    rtx, trx, fnp1, xo2l, sxo2;

  /****BEGIN PROLOGUE  DBESJ 
   ****PURPOSE  Compute an N member sequence of J Bessel functions 
   *           J/SUB(ALPHA+K-1)/(X), K=1,...,N for non-negative ALPHA 
   *           and X. 
   ****LIBRARY   SLATEC 
   ****CATEGORY  C10A3 
   ****TYPE      DOUBLE PRECISION (BESJ-S, DBESJ-D) 
   ****KEYWORDS  J BESSEL FUNCTION, SPECIAL FUNCTIONS 
   ****AUTHOR  Amos, D. E., (SNLA) 
   *          Daniel, S. L., (SNLA) 
   *          Weston, M. K., (SNLA) 
   ****DESCRIPTION 
   * 
   *    Abstract  **** a double precision routine **** 
   *        DBESJ computes an N member sequence of J Bessel functions 
   *        J/sub(ALPHA+K-1)/(X), K=1,...,N for non-negative ALPHA and X. 
   *        A combination of the power series, the asymptotic expansion 
   *        for X to infinity and the uniform asymptotic expansion for 
   *        NU to infinity are applied over subdivisions of the (NU,X) 
   *        plane.  For values of (NU,X) not covered by one of these 
   *        formulae, the order is incremented or decremented by int 
   *        values into a region where one of the formulae apply. Backward 
   *        recursion is applied to reduce orders by int values except 
   *        where the entire sequence lies in the oscillatory region.  In 
   *        this case forward recursion is stable and values from the 
   *        asymptotic expansion for X to infinity start the recursion 
   *        when it is efficient to do so. Leading terms of the series and 
   *        uniform expansion are tested for underflow.  If a sequence is 
   *        requested and the last member would underflow, the result is 
   *        set to zero and the next lower order tried, etc., until a 
   *        member comes on scale or all members are set to zero. 
   *        Overflow cannot occur. 
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
   *          N      - number of members in the sequence, N .GE. 1 
   * 
   *        Output     Y is double precision 
   *          Y      - a vector whose first N components contain 
   *                   values for J/sub(ALPHA+K-1)/(X), K=1,...,N 
   *          NZ     - number of components of Y set to zero due to 
   *                   underflow, 
   *                   NZ=0   , normal return, computation completed 
   *                   NZ .NE. 0, last NZ components of Y set to zero, 
   *                            Y(K)=0.0D0, K=N-NZ+1,...,N. 
   * 
   *    Error Conditions 
   *        Improper input arguments - a fatal error 
   *        Underflow  - a non-fatal error (NZ .NE. 0) 
   * 
   ****REFERENCES  D. E. Amos, S. L. Daniel and M. K. Weston, CDC 6600 
   *                subroutines IBESS and JBESS for Bessel functions 
   *                I(NU,X) and J(NU,X), X .GE. 0, NU .GE. 0, ACM 
   *                Transactions on Mathematical Software 3, (1977), 
   *                pp. 76-92. 
   *              F. W. J. Olver, Tables of Bessel Functions of Moderate 
   *                or Large Orders, NPL Mathematical Tables 6, Her 
   *                Majesty's Stationery Office, London, 1962. 
   ****ROUTINES CALLED  D1MACH, DASYJY, DJAIRY, DLNGAM, I1MACH, XERMSG 
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
   ****END PROLOGUE  DBESJ 
   */
  /* Parameter adjustments */
  --y;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  DBESJ 
   */
  *ierr = 0;
  *nz = 0;
  kt = 1;
  ns = 0;
  /*    I1MACH(14) REPLACES I1MACH(11) IN A DOUBLE PRECISION CODE 
   *    I1MACH(15) REPLACES I1MACH(12) IN A DOUBLE PRECISION CODE 
   */
  ta = nsp_calpack_d1mach (&c__3);
  tol = Max (ta, 1e-15);
  i1 = nsp_calpack_i1mach (&c__14) + 1;
  i2 = nsp_calpack_i1mach (&c__15);
  tb = nsp_calpack_d1mach (&c__5);
  elim1 = (i2 * tb + 3.) * -2.303;
  rtol = 1. / tol;
  slim = nsp_calpack_d1mach (&c__1) * rtol * 1e3;
  /*    TOLLN = -LN(TOL) 
   */
  tolln = tb * 2.303 * i1;
  tolln = Min (tolln, 34.5388);
  if (*n - 1 < 0)
    {
      goto L720;
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
  if (*x < 0.)
    {
      goto L730;
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
      goto L710;
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
      goto L710;
    }
  /* 
   */
  ialp = (int) (*alpha);
  fni = (double) (ialp + *n - 1);
  fnf = *alpha - ialp;
  dfn = fni + fnf;
  fnu = dfn;
  xo2 = *x * .5;
  sxo2 = xo2 * xo2;
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
  ta = Max (20., fnu);
  if (*x > ta)
    {
      goto L120;
    }
  if (*x > 12.)
    {
      goto L110;
    }
  xo2l = log (xo2);
  ns = (int) (sxo2 - fnu) + 1;
  goto L100;
L90:
  fn = fnu;
  fnp1 = fn + 1.;
  xo2l = log (xo2);
  is = kt;
  if (*x <= .5)
    {
      goto L330;
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
  goto L330;
L110:
  /*Computing MAX 
   */
  d__1 = 36. - fnu;
  ans = Max (d__1, 0.);
  ns = (int) ans;
  fni += ns;
  dfn = fni + fnf;
  fn = dfn;
  is = kt;
  if (*n - 1 + ns > 0)
    {
      is = 3;
    }
  goto L130;
L120:
  rtx = sqrt (*x);
  tau = rtwo * rtx;
  ta = tau + fnulim[kt - 1];
  if (fnu <= ta)
    {
      goto L480;
    }
  fn = fnu;
  is = kt;
  /* 
   *    UNIFORM ASYMPTOTIC EXPANSION FOR NU TO INFINITY 
   * 
   */
L130:
  i1 = (i__1 = 3 - is, Abs (i__1));
  i1 = Max (i1, 1);
  flgjy = 1.;
  nsp_calpack_dasyjy ((U_fp) djairy_, x, &fn, &flgjy, &i1, &temp[is - 1], wk,
		      &iflw);
  if (iflw != 0)
    {
      goto L380;
    }
  switch (is)
    {
    case 1:
      goto L320;
    case 2:
      goto L450;
    case 3:
      goto L620;
    }
L310:
  temp[0] = temp[2];
  kt = 1;
L320:
  is = 2;
  fni += -1.;
  dfn = fni + fnf;
  fn = dfn;
  if (i1 == 2)
    {
      goto L450;
    }
  goto L130;
  /* 
   *    SERIES FOR (X/2)**2.LE.NU+1 
   * 
   */
L330:
  gln = nsp_calpack_dlngam (&fnp1);
  arg = fn * xo2l - gln;
  if (arg < -elim1)
    {
      goto L400;
    }
  earg = exp (arg);
L340:
  s = 1.;
  if (*x < tol)
    {
      goto L360;
    }
  ak = 3.;
  t2 = 1.;
  t = 1.;
  s1 = fn;
  for (k = 1; k <= 17; ++k)
    {
      s2 = t2 + s1;
      t = -t * sxo2 / s2;
      s += t;
      if (Abs (t) < tol)
	{
	  goto L360;
	}
      t2 += ak;
      ak += 2.;
      s1 += fn;
      /* L350: */
    }
L360:
  temp[is - 1] = s * earg;
  switch (is)
    {
    case 1:
      goto L370;
    case 2:
      goto L450;
    case 3:
      goto L610;
    }
L370:
  earg = earg * fn / xo2;
  fni += -1.;
  dfn = fni + fnf;
  fn = dfn;
  is = 2;
  goto L340;
  /* 
   *    SET UNDERFLOW VALUE AND UPDATE PARAMETERS 
   *    UNDERFLOW CAN ONLY OCCUR FOR NS=0 SINCE THE ORDER MUST BE LARGER 
   *    THAN 36. THEREFORE, NS NEE NOT BE TESTED. 
   * 
   */
L380:
  y[nn] = 0.;
  --nn;
  fni += -1.;
  dfn = fni + fnf;
  fn = dfn;
  if (nn - 1 < 0)
    {
      goto L440;
    }
  else if (nn - 1 == 0)
    {
      goto L390;
    }
  else
    {
      goto L130;
    }
L390:
  kt = 2;
  is = 2;
  goto L130;
L400:
  y[nn] = 0.;
  --nn;
  fnp1 = fn;
  fni += -1.;
  dfn = fni + fnf;
  fn = dfn;
  if (nn - 1 < 0)
    {
      goto L440;
    }
  else if (nn - 1 == 0)
    {
      goto L410;
    }
  else
    {
      goto L420;
    }
L410:
  kt = 2;
  is = 2;
L420:
  if (sxo2 <= fnp1)
    {
      goto L430;
    }
  goto L130;
L430:
  arg = arg - xo2l + log (fnp1);
  if (arg < -elim1)
    {
      goto L400;
    }
  goto L330;
L440:
  *nz = *n - nn;
  return 0;
  /* 
   *    BACKWARD RECURSION SECTION 
   * 
   */
L450:
  if (ns != 0)
    {
      goto L451;
    }
  *nz = *n - nn;
  if (kt == 2)
    {
      goto L470;
    }
  /*    BACKWARD RECUR FROM INDEX ALPHA+NN-1 TO ALPHA 
   */
  y[nn] = temp[0];
  y[nn - 1] = temp[1];
  if (nn == 2)
    {
      return 0;
    }
L451:
  trx = 2. / *x;
  dtm = fni;
  tm = (dtm + fnf) * trx;
  ak = 1.;
  ta = temp[0];
  tb = temp[1];
  if (Abs (ta) > slim)
    {
      goto L455;
    }
  ta *= rtol;
  tb *= rtol;
  ak = tol;
L455:
  kk = 2;
  in = ns - 1;
  if (in == 0)
    {
      goto L690;
    }
  if (ns != 0)
    {
      goto L670;
    }
  k = nn - 2;
  i__1 = nn;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      s = tb;
      tb = tm * tb - ta;
      ta = s;
      y[k] = tb * ak;
      dtm += -1.;
      tm = (dtm + fnf) * trx;
      --k;
      /* L460: */
    }
  return 0;
L470:
  y[1] = temp[1];
  return 0;
  /* 
   *    ASYMPTOTIC EXPANSION FOR X TO INFINITY WITH FORWARD RECURSION IN 
   *    OSCILLATORY REGION X.GT.MAX(20, NU), PROVIDED THE LAST MEMBER 
   *    OF THE SEQUENCE IS ALSO IN THE REGION. 
   * 
   */
L480:
  in = (int) (*alpha - tau + 2.);
  if (in <= 0)
    {
      goto L490;
    }
  idalp = ialp - in - 1;
  kt = 1;
  goto L500;
L490:
  idalp = ialp;
  in = 0;
L500:
  is = kt;
  fidal = (double) idalp;
  dalpha = fidal + fnf;
  arg = *x - pidt * dalpha - pdf;
  sa = sin (arg);
  sb = cos (arg);
  coef = rttp / rtx;
  etx = *x * 8.;
L510:
  dtm = fidal + fidal;
  dtm *= dtm;
  tm = 0.;
  if (fidal == 0. && Abs (fnf) < tol)
    {
      goto L520;
    }
  tm = fnf * 4. * (fidal + fidal + fnf);
L520:
  trx = dtm - 1.;
  t2 = (trx + tm) / etx;
  s2 = t2;
  relb = tol * Abs (t2);
  t1 = etx;
  s1 = 1.;
  fn = 1.;
  ak = 8.;
  for (k = 1; k <= 13; ++k)
    {
      t1 += etx;
      fn += ak;
      trx = dtm - fn;
      ap = trx + tm;
      t2 = -t2 * ap / t1;
      s1 += t2;
      t1 += etx;
      ak += 8.;
      fn += ak;
      trx = dtm - fn;
      ap = trx + tm;
      t2 = t2 * ap / t1;
      s2 += t2;
      if (Abs (t2) <= relb)
	{
	  goto L540;
	}
      ak += 8.;
      /* L530: */
    }
L540:
  temp[is - 1] = coef * (s1 * sb - s2 * sa);
  if (is == 2)
    {
      goto L560;
    }
  fidal += 1.;
  dalpha = fidal + fnf;
  is = 2;
  tb = sa;
  sa = -sb;
  sb = tb;
  goto L510;
  /* 
   *    FORWARD RECURSION SECTION 
   * 
   */
L560:
  if (kt == 2)
    {
      goto L470;
    }
  s1 = temp[0];
  s2 = temp[1];
  tx = 2. / *x;
  tm = dalpha * tx;
  if (in == 0)
    {
      goto L580;
    }
  /* 
   *    FORWARD RECUR TO INDEX ALPHA 
   * 
   */
  i__1 = in;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      s = s2;
      s2 = tm * s2 - s1;
      tm += tx;
      s1 = s;
      /* L570: */
    }
  if (nn == 1)
    {
      goto L600;
    }
  s = s2;
  s2 = tm * s2 - s1;
  tm += tx;
  s1 = s;
L580:
  /* 
   *    FORWARD RECUR FROM INDEX ALPHA TO ALPHA+N-1 
   * 
   */
  y[1] = s1;
  y[2] = s2;
  if (nn == 2)
    {
      return 0;
    }
  i__1 = nn;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      y[i__] = tm * y[i__ - 1] - y[i__ - 2];
      tm += tx;
      /* L590: */
    }
  return 0;
L600:
  y[1] = s2;
  return 0;
  /* 
   *    BACKWARD RECURSION WITH NORMALIZATION BY 
   *    ASYMPTOTIC EXPANSION FOR NU TO INFINITY OR POWER SERIES. 
   * 
   */
L610:
  /*    COMPUTATION OF LAST ORDER FOR SERIES NORMALIZATION 
   *Computing MAX 
   */
  d__1 = 3. - fn;
  akm = Max (d__1, 0.);
  km = (int) akm;
  tfn = fn + km;
  ta = (gln + tfn - .9189385332 - .0833333333 / tfn) / (tfn + .5);
  ta = xo2l - ta;
  tb = -(1. - 1.5 / tfn) / tfn;
  akm = tolln / (-ta + sqrt (ta * ta - tolln * tb)) + 1.5;
  in = km + (int) akm;
  goto L660;
L620:
  /*    COMPUTATION OF LAST ORDER FOR ASYMPTOTIC EXPANSION NORMALIZATION 
   */
  gln = wk[2] + wk[1];
  if (wk[5] > 30.)
    {
      goto L640;
    }
  rden = (pp[3] * wk[5] + pp[2]) * wk[5] + 1.;
  rzden = pp[0] + pp[1] * wk[5];
  ta = rzden / rden;
  if (wk[0] < .1)
    {
      goto L630;
    }
  tb = gln / wk[4];
  goto L650;
L630:
  tb = ((wk[0] * .0887944358 + .167989473) * wk[0] + 1.259921049) / wk[6];
  goto L650;
L640:
  ta = tolln * .5 / wk[3];
  ta = ((ta * .049382716 - .1111111111) * ta + .6666666667) * ta * wk[5];
  if (wk[0] < .1)
    {
      goto L630;
    }
  tb = gln / wk[4];
L650:
  in = (int) (ta / tb + 1.5);
  if (in > inlim)
    {
      goto L310;
    }
L660:
  dtm = fni + in;
  trx = 2. / *x;
  tm = (dtm + fnf) * trx;
  ta = 0.;
  tb = tol;
  kk = 1;
  ak = 1.;
L670:
  /* 
   *    BACKWARD RECUR UNINDEXED 
   * 
   */
  i__1 = in;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      s = tb;
      tb = tm * tb - ta;
      ta = s;
      dtm += -1.;
      tm = (dtm + fnf) * trx;
      /* L680: */
    }
  /*    NORMALIZATION 
   */
  if (kk != 1)
    {
      goto L690;
    }
  s = temp[2];
  sa = ta / tb;
  ta = s;
  tb = s;
  if (Abs (s) > slim)
    {
      goto L685;
    }
  ta *= rtol;
  tb *= rtol;
  ak = tol;
L685:
  ta *= sa;
  kk = 2;
  in = ns;
  if (ns != 0)
    {
      goto L670;
    }
L690:
  y[nn] = tb * ak;
  *nz = *n - nn;
  if (nn == 1)
    {
      return 0;
    }
  k = nn - 1;
  s = tb;
  tb = tm * tb - ta;
  ta = s;
  y[k] = tb * ak;
  if (nn == 2)
    {
      return 0;
    }
  dtm += -1.;
  tm = (dtm + fnf) * trx;
  k = nn - 2;
  /* 
   *    BACKWARD RECUR INDEXED 
   * 
   */
  i__1 = nn;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      s = tb;
      tb = tm * tb - ta;
      ta = s;
      y[k] = tb * ak;
      dtm += -1.;
      tm = (dtm + fnf) * trx;
      --k;
      /* L700: */
    }
  return 0;
  /* 
   * 
   * 
   */
L710:
  *ierr = 1;
  /*     CALL XERMSG ('SLATEC', 'DBESJ', 'ORDER, ALPHA, LESS THAN ZERO.', 
   *    +   2, 1) 
   */
  return 0;
L720:
  *ierr = 1;
  /*     CALL XERMSG ('SLATEC', 'DBESJ', 'N LESS THAN ONE.', 2, 1) 
   */
  return 0;
L730:
  *ierr = 1;
  /*     CALL XERMSG ('SLATEC', 'DBESJ', 'X LESS THAN ZERO.', 2, 1) 
   */
  return 0;
}				/* dbesj_ */
#endif
