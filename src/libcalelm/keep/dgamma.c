/* dgamma.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

#if 0
/* Table of constant values */

static int c__42 = 42;
static int c__3 = 3;
static int c__4 = 4;
static int c__2 = 2;
static int c__1 = 1;

/*DECK DGAMMA 
 */
double nsp_calpack_dgamma (double *x)
{
  /* Initialized data */

  static double gamcs[42] =
    { .008571195590989331421920062399942, .004415381324841006757191315771652,
    .05685043681599363378632664588789, -.004219835396418560501012500186624,
    .001326808181212460220584006796352, -1.893024529798880432523947023886e-4,
    3.606925327441245256578082217225e-5, -6.056761904460864218485548290365e-6,
    1.055829546302283344731823509093e-6, -1.811967365542384048291855891166e-7,
    3.117724964715322277790254593169e-8, -5.354219639019687140874081024347e-9,
    9.19327551985958894688778682594e-10,
    -1.577941280288339761767423273953e-10,
    2.707980622934954543266540433089e-11,
    -4.646818653825730144081661058933e-12,
    7.973350192007419656460767175359e-13,
    -1.368078209830916025799499172309e-13,
    2.347319486563800657233471771688e-14,
    -4.027432614949066932766570534699e-15,
    6.910051747372100912138336975257e-16,
    -1.185584500221992907052387126192e-16,
    2.034148542496373955201026051932e-17,
    -3.490054341717405849274012949108e-18,
    5.987993856485305567135051066026e-19,
    -1.027378057872228074490069778431e-19,
    1.762702816060529824942759660748e-20,
    -3.024320653735306260958772112042e-21,
    5.188914660218397839717833550506e-22,
    -8.902770842456576692449251601066e-23,
    1.527474068493342602274596891306e-23,
    -2.620731256187362900257328332799e-24,
    4.496464047830538670331046570666e-25,
    -7.714712731336877911703901525333e-26,
    1.323635453126044036486572714666e-26,
    -2.270999412942928816702313813333e-27,
    3.896418998003991449320816639999e-28,
    -6.685198115125953327792127999999e-29,
    1.146998663140024384347613866666e-29,
    -1.967938586345134677295103999999e-30,
    3.376448816585338090334890666666e-31,
    -5.793070335782135784625493333333e-32
  };
  static double pi = 3.1415926535897932384626433832795;
  static double sq2pil = .91893853320467274178032973640562;
  static int first = TRUE;

  /* System generated locals */
  int i__1;
  double ret_val, d__1, d__2;

  /* Local variables */
  static int ngam;
  static double xmin, xmax;
  int i__, n;
  double y;
  static double dxrel;
  double sinpiy;

  /****BEGIN PROLOGUE  DGAMMA 
   ****PURPOSE  Compute the complete Gamma function. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C7A 
   ****TYPE      DOUBLE PRECISION (GAMMA-S, DGAMMA-D, CGAMMA-C) 
   ****KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, SPECIAL FUNCTIONS 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *DGAMMA(X) calculates the double precision complete Gamma function 
   *for double precision argument X. 
   * 
   *Series for GAM        on the interval  0.          to  1.00000E+00 
   *                                       with weighted error   5.79E-32 
   *                                        log weighted error  31.24 
   *                              significant figures required  30.00 
   *                                   decimal places required  32.05 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, D9LGMC, DCSEVL, DGAMLM, INITDS, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770601  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  890911  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   *  920618  Removed space from variable name.  (RWC, WRB) 
   ****END PROLOGUE  DGAMMA 
   * 
   */
  /****FIRST EXECUTABLE STATEMENT  DGAMMA 
   */
  if (first)
    {
      d__1 = nsp_calpack_d1mach (&c__3) * .1;
      ngam = nsp_calpack_initds (gamcs, &c__42, &d__1);
      /* 
       */
      nsp_calpack_dgamlm (&xmin, &xmax);
      dxrel = sqrt (nsp_calpack_d1mach (&c__4));
    }
  first = FALSE;
  /* 
   */
  y = Abs (*x);
  if (y > 10.)
    {
      goto L50;
    }
  /* 
   *COMPUTE GAMMA(X) FOR -XBND .LE. X .LE. XBND.  REDUCE INTERVAL AND FIND 
   *GAMMA(1+Y) FOR 0.0 .LE. Y .LT. 1.0 FIRST OF ALL. 
   * 
   */
  n = (int) (*x);
  if (*x < 0.)
    {
      --n;
    }
  y = *x - n;
  --n;
  d__1 = y * 2. - 1.;
  ret_val = nsp_calpack_dcsevl (&d__1, gamcs, &ngam) + .9375;
  if (n == 0)
    {
      return ret_val;
    }
  /* 
   */
  if (n > 0)
    {
      goto L30;
    }
  /* 
   *COMPUTE GAMMA(X) FOR X .LT. 1.0 
   * 
   */
  n = -n;
  if (*x == 0.)
    {
      nsp_calpack_xermsg ("SLATEC", "DGAMMA", "X IS 0", &c__4, &c__2, 6L, 6L,
			  6L);
    }
  if (*x < 0. && *x + n - 2 == 0.)
    {
      nsp_calpack_xermsg ("SLATEC", "DGAMMA", "X IS A NEGATIVE INT", &c__4,
			  &c__2, 6L, 6L, 23L);
    }
  d__2 = *x - .5;
  if (*x < -.5 && (d__1 = (*x - d_int (&d__2)) / *x, Abs (d__1)) < dxrel)
    {
      nsp_calpack_xermsg ("SLATEC", "DGAMMA",
			  "ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NEGATIVE INT",
			  &c__1, &c__1, 6L, 6L, 60L);
    }
  /* 
   */
  i__1 = n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ret_val /= *x + i__ - 1;
      /* L20: */
    }
  return ret_val;
  /* 
   *GAMMA(X) FOR X .GE. 2.0 AND X .LE. 10.0 
   * 
   */
L30:
  i__1 = n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ret_val = (y + i__) * ret_val;
      /* L40: */
    }
  return ret_val;
  /* 
   *GAMMA(X) FOR ABS(X) .GT. 10.0.  RECALL Y = ABS(X). 
   * 
   */
L50:
  if (*x > xmax)
    {
      nsp_calpack_xermsg ("SLATEC", "DGAMMA", "X SO BIG GAMMA OVERFLOWS",
			  &c__3, &c__2, 6L, 6L, 24L);
    }
  /* 
   */
  ret_val = 0.;
  if (*x < xmin)
    {
      nsp_calpack_xermsg ("SLATEC", "DGAMMA", "X SO SMALL GAMMA UNDERFLOWS",
			  &c__2, &c__1, 6L, 6L, 27L);
    }
  if (*x < xmin)
    {
      return ret_val;
    }
  /* 
   */
  ret_val = exp ((y - .5) * log (y) - y + sq2pil + nsp_calpack_d9lgmc (&y));
  if (*x > 0.)
    {
      return ret_val;
    }
  /* 
   */
  d__2 = *x - .5;
  if ((d__1 = (*x - d_int (&d__2)) / *x, Abs (d__1)) < dxrel)
    {
      nsp_calpack_xermsg ("SLATEC", "DGAMMA",
			  "ANSWER LT HALF PRECISION, X TOO NEAR NEGATIVE INT",
			  &c__1, &c__1, 6L, 6L, 53L);
    }
  /* 
   */
  sinpiy = sin (pi * y);
  if (sinpiy == 0.)
    {
      nsp_calpack_xermsg ("SLATEC", "DGAMMA", "X IS A NEGATIVE INT", &c__4,
			  &c__2, 6L, 6L, 23L);
    }
  /* 
   */
  ret_val = -pi / (y * sinpiy * ret_val);
  /* 
   */
  return ret_val;
}


#endif
