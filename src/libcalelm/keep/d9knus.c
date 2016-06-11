/* d9knus.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__3 = 3;
static int c__29 = 29;
static int c__20 = 20;
static int c__1 = 1;
static int c__2 = 2;

/****BEGIN PROLOGUE  D9KNUS 
 ****SUBSIDIARY 
 ****PURPOSE  Compute Bessel functions EXP(X)*K-SUB-XNU(X) and EXP(X)* 
 *           K-SUB-XNU+1(X) for 0.0 .LE. XNU .LT. 1.0. 
 ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C10B3 
   ****TYPE      DOUBLE PRECISION (R9KNUS-S, D9KNUS-D) 
   ****KEYWORDS  BESSEL FUNCTION, FNLIB, SPECIAL FUNCTIONS 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *Compute Bessel functions EXP(X) * K-sub-XNU (X)  and 
   *EXP(X) * K-sub-XNU+1 (X) for 0.0 .LE. XNU .LT. 1.0 . 
   * 
   *Series for C0K        on the interval  0.          to  2.50000E-01 
   *                                       with weighted error   2.16E-32 
   *                                        log weighted error  31.67 
   *                              significant figures required  30.86 
   *                                   decimal places required  32.40 
   * 
   *Series for ZNU1       on the interval -7.00000E-01 to  0. 
   *                                       with weighted error   2.45E-33 
   *                                        log weighted error  32.61 
   *                              significant figures required  31.85 
   *                                   decimal places required  33.26 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, DCSEVL, DGAMMA, INITDS, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770601  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  890911  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   *  900720  Routine changed from user-callable to subsidiary.  (WRB) 
   *  900727  Added EXTERNAL statement.  (WRB) 
   *  920618  Removed space from variable names.  (RWC, WRB) 
   ****END PROLOGUE  D9KNUS 
   */
  /****FIRST EXECUTABLE STATEMENT  D9KNUS 
   */

int nsp_calpack_d9knus (double *xnu, double *x, double *bknu, double *bknu1,  int *iswtch)
{
  /* Initialized data */

  static double c0kcs[29] =
    { .060183057242626108387577445180329, -.15364871433017286092959755943124,
    -.011751176008210492040068229226213,
    -8.5248788891979509827048401550987e-4,
    -6.1329838767496791874098176922111e-5,
    -4.4052281245510444562679889548505e-6,
    -3.1631246728384488192915445892199e-7,
    -2.2710719382899588330673771793396e-8,
    -1.630564460807760955227462051536e-9,
    -1.170693929941477656875604404313e-10,
    -8.4052063786464437174546593413792e-12,
    -6.0346670118979991487096050737198e-13,
    -4.3326960335681371952045997366903e-14,
    -3.1107358030203546214634697772237e-15,
    -2.233407822673698225448613340984e-16,
    -1.603514671686422630063579152861e-17,
    -1.1512717363666556196035697705305e-18,
    -8.2657591746836959105169479089258e-20,
    -5.9345480806383948172333436695984e-21,
    -4.2608138196467143926499613023976e-22,
    -3.0591266864812876299263698370542e-23,
    -2.1963541426734575224975501815516e-24,
    -1.576911326149583607110575068476e-25,
    -1.1321713935950320948757731048056e-26,
    -8.1286248834598404082792349714433e-28,
    -5.8360900893453226552829349315949e-29,
    -4.1901241623610922519452337780905e-30,
    -3.0083737960206435069530504212862e-31,
    -2.1599152067808647728342168089832e-32
  };
  static double znu1cs[20] =
    { .203306756994191729674444001216911, .140077933413219771062943670790563,
    .0079167969610016135284097224197232, 3.3980118253210404535293009220575e-4,
    1.1741975688989336666450722835269e-5,
    3.39357570612261680333825865475121e-7,
    8.42594176976219910194629891264803e-9,
    1.8333667702485008918474815090009e-10,
    3.54969844704416310863007064469557e-12,
    6.19032496469887332205244342078407e-14,
    9.81964535680439424960346115456527e-16,
    1.42851314396490474211473563005985e-17,
    1.91894921887825298966162467488436e-19,
    2.39430979739498914162313140597128e-21,
    2.78890246815347354835870465474995e-23,
    3.04606650633033442582845214092865e-25,
    3.13173237042191815771564260932089e-27,
    3.04133098987854951645174908005034e-29,
    2.79840384636833084343185097659733e-31,
    2.44637186274497596485238794922666e-33
  };
  static double euler = .5772156649015328606065120900824;
  static double sqpi2 = 1.2533141373155002512078826424055;
  static double aln2 = .69314718055994530941723212145818;
  static int first = TRUE;

  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  double beta[32], alnz;
  static double xsml;
  double expx, vlnz, ztov, bknu0;
  static int ntc0k;
  double a[32];
  int i__, n;
  double x2tov, alpha[32], v, z__, bknud, a0, b0, c0;
  double p1, p2, p3, sqrtx, an, bn;
  int ii;
  static double alnbig;
  static int ntznu1;
  double xi, qq;
  static double alneps, alnsml;
  int nterms;
  double result;
  static double xnusml;
  double x2n, eta;
  int inu;
  double xmu;

  if (first)
    {
      eta = nsp_calpack_d1mach (&c__3) * .1;
      ntc0k = nsp_calpack_initds (c0kcs, &c__29, &eta);
      ntznu1 = nsp_calpack_initds (znu1cs, &c__20, &eta);
      /* 
       */
      xnusml = sqrt (nsp_calpack_d1mach (&c__3) / 8.);
      xsml = nsp_calpack_d1mach (&c__3) * .1;
      alnsml = log (nsp_calpack_d1mach (&c__1));
      alnbig = log (nsp_calpack_d1mach (&c__2));
      alneps = log (nsp_calpack_d1mach (&c__3) * .1);
    }
  first = FALSE;
  /* 
   */
  if (*xnu < 0. || *xnu >= 1.)
    {
      nsp_calpack_xermsg ("SLATEC", "D9KNUS", "XNU MUST BE GE 0 AND LT 1",
			  &c__1, &c__2, 6L, 6L, 25L);
    }
  if (*x <= 0.)
    {
      nsp_calpack_xermsg ("SLATEC", "D9KNUS", "X MUST BE GT 0", &c__2, &c__2,
			  6L, 6L, 14L);
    }
  /* 
   */
  *iswtch = 0;
  if (*x > 2.)
    {
      goto L50;
    }
  /* 
   *X IS SMALL.  COMPUTE K-SUB-XNU (X) AND THE DERIVATIVE OF K-SUB-XNU (X) 
   *THEN FIND K-SUB-XNU+1 (X).  XNU IS REDUCED TO THE INTERVAL (-.5,+.5) 
   *THEN TO (0., .5), BECAUSE K OF NEGATIVE ORDER (-NU) = K OF POSITIVE 
   *ORDER (+NU). 
   * 
   */
  v = *xnu;
  if (*xnu > .5)
    {
      v = 1. - *xnu;
    }
  /* 
   *CAREFULLY FIND (X/2)**XNU AND Z**XNU WHERE Z = X*X/4. 
   */
  alnz = (log (*x) - aln2) * 2.;
  /* 
   */
  if (*x > *xnu)
    {
      goto L20;
    }
  if (*xnu * -.5 * alnz - aln2 - log (*xnu) > alnbig)
    {
      nsp_calpack_xermsg ("SLATEC", "D9KNUS",
			  "X SO SMALL BESSEL K-SUB-XNU OVERFLOWS", &c__3,
			  &c__2, 6L, 6L, 37L);
    }
  /* 
   */
L20:
  vlnz = v * alnz;
  x2tov = exp (vlnz * .5);
  ztov = 0.;
  if (vlnz > alnsml)
    {
      /*Computing 2nd power 
       */
      d__1 = x2tov;
      ztov = d__1 * d__1;
    }
  /* 
   */
  d__1 = v + 1.;
  a0 = nsp_calpack_dgamma (&d__1) * .5;
  d__1 = 1. - v;
  b0 = nsp_calpack_dgamma (&d__1) * .5;
  c0 = -euler;
  if (ztov > .5 && v > xnusml)
    {
      d__1 = v * 8. * v - 1.;
      c0 = nsp_calpack_dcsevl (&d__1, c0kcs, &ntc0k) - .75;
    }
  /* 
   */
  if (ztov <= .5)
    {
      alpha[0] = (a0 - ztov * b0) / v;
    }
  if (ztov > .5)
    {
      d__1 = vlnz / .35 + 1.;
      alpha[0] =
	c0 - alnz * (nsp_calpack_dcsevl (&d__1, znu1cs, &ntznu1) + .75) * b0;
    }
  beta[0] = (a0 + ztov * b0) * -.5;
  /* 
   */
  z__ = 0.;
  if (*x > xsml)
    {
      z__ = *x * .25 * *x;
    }
  /*Computing MAX 
   */
  d__1 = 2., d__2 = (alnz * 8. - 25.19 - alneps) / (4.28 - alnz) + 11.;
  nterms = (int) Max (d__1, d__2);
  i__1 = nterms;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      xi = (double) (i__ - 1);
      a0 /= xi * (xi - v);
      b0 /= xi * (xi + v);
      alpha[i__ - 1] = (alpha[i__ - 2] + xi * 2. * a0) / (xi * (xi + v));
      beta[i__ - 1] = (xi - v * .5) * alpha[i__ - 1] - ztov * b0;
      /* L30: */
    }
  /* 
   */
  *bknu = alpha[nterms - 1];
  bknud = beta[nterms - 1];
  i__1 = nterms;
  for (ii = 2; ii <= i__1; ++ii)
    {
      i__ = nterms + 1 - ii;
      *bknu = alpha[i__ - 1] + *bknu * z__;
      bknud = beta[i__ - 1] + bknud * z__;
      /* L40: */
    }
  /* 
   */
  expx = exp (*x);
  *bknu = expx * *bknu / x2tov;
  /* 
   */
  if ((*xnu + 1.) * -.5 * alnz - aln2 * 2. > alnbig)
    {
      *iswtch = 1;
    }
  if (*iswtch == 1)
    {
      return 0;
    }
  bknud = expx * bknud * 2. / (x2tov * *x);
  /* 
   */
  if (*xnu <= .5)
    {
      *bknu1 = v * *bknu / *x - bknud;
    }
  if (*xnu <= .5)
    {
      return 0;
    }
  /* 
   */
  bknu0 = *bknu;
  *bknu = -v * *bknu / *x - bknud;
  *bknu1 = *xnu * 2. * *bknu / *x + bknu0;
  return 0;
  /* 
   *X IS LARGE.  FIND K-SUB-XNU (X) AND K-SUB-XNU+1 (X) WITH Y. L. LUKE-S 
   *RATIONAL EXPANSION. 
   * 
   */
L50:
  sqrtx = sqrt (*x);
  if (*x > 1. / xsml)
    {
      goto L90;
    }
  an = -.6 - 1.02 / *x;
  bn = -.27 - .53 / *x;
  /*Computing MIN 
   *Computing MAX 
   */
  d__1 = 3., d__2 = an + bn * alneps;
  i__1 = 32, i__2 = (int) Max (d__1, d__2);
  nterms = Min (i__1, i__2);
  /* 
   */
  for (inu = 1; inu <= 2; ++inu)
    {
      xmu = 0.;
      if (inu == 1 && *xnu > xnusml)
	{
	  xmu = *xnu * 4. * *xnu;
	}
      if (inu == 2)
	{
	  /*Computing 2nd power 
	   */
	  d__1 = Abs (*xnu) + 1.;
	  xmu = d__1 * d__1 * 4.;
	}
      /* 
       */
      a[0] = 1. - xmu;
      a[1] = 9. - xmu;
      a[2] = 25. - xmu;
      if (a[1] == 0.)
	{
	  result = sqpi2 * (*x * 16. + xmu + 7.) / (*x * 16. * sqrtx);
	}
      if (a[1] == 0.)
	{
	  goto L70;
	}
      /* 
       */
      alpha[0] = 1.;
      alpha[1] = (*x * 16. + a[1]) / a[1];
      alpha[2] =
	((*x * 768. + a[2] * 48.) * *x + a[1] * a[2]) / (a[1] * a[2]);
      /* 
       */
      beta[0] = 1.;
      beta[1] = (*x * 16. + (xmu + 7.)) / a[1];
      beta[2] =
	((*x * 768. + (xmu + 23.) * 48.) * *x +
	 ((xmu + 62.) * xmu + 129.)) / (a[1] * a[2]);
      /* 
       */
      if (nterms < 4)
	{
	  goto L65;
	}
      i__1 = nterms;
      for (i__ = 4; i__ <= i__1; ++i__)
	{
	  n = i__ - 1;
	  x2n = (double) ((n << 1) - 1);
	  /* 
	   *Computing 2nd power 
	   */
	  d__1 = x2n + 2.;
	  a[i__ - 1] = d__1 * d__1 - xmu;
	  qq = x2n * 16. / a[i__ - 1];
	  p1 =
	    -x2n * (n * 12 * n - n * 20 - a[0]) / ((x2n - 2.) * a[i__ - 1]) -
	    qq * *x;
	  p2 = (n * 12 * n - n * 28 + 8 - a[0]) / a[i__ - 1] - qq * *x;
	  p3 = -x2n * a[i__ - 4] / ((x2n - 2.) * a[i__ - 1]);
	  /* 
	   */
	  alpha[i__ - 1] =
	    -p1 * alpha[i__ - 2] - p2 * alpha[i__ - 3] - p3 * alpha[i__ - 4];
	  beta[i__ - 1] =
	    -p1 * beta[i__ - 2] - p2 * beta[i__ - 3] - p3 * beta[i__ - 4];
	  /* L60: */
	}
      /* 
       */
    L65:
      result = sqpi2 * beta[nterms - 1] / (sqrtx * alpha[nterms - 1]);
      /* 
       */
    L70:
      if (inu == 1)
	{
	  *bknu = result;
	}
      if (inu == 2)
	{
	  *bknu1 = result;
	}
      /* L80: */
    }
  return 0;
  /* 
   */
L90:
  *bknu = sqpi2 / sqrtx;
  *bknu1 = *bknu;
  return 0;
  /* 
   */
}				/* d9knus_ */
#endif
