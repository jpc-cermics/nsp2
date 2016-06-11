/* dbsk0e.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__3 = 3;
static int c__16 = 16;
static int c__38 = 38;
static int c__33 = 33;
static int c__2 = 2;

/*DECK DBSK0E 
 */
double nsp_calpack_dbsk0e (double *x)
{
  /* Initialized data */

  static double bk0cs[16] = { -.0353273932339027687201140060063153,
    .344289899924628486886344927529213, .0359799365153615016265721303687231,
    .00126461541144692592338479508673447,
    2.28621210311945178608269830297585e-5,
    2.53479107902614945730790013428354e-7,
    1.90451637722020885897214059381366e-9,
    1.03496952576336245851008317853089e-11,
    4.25981614279108257652445327170133e-14,
    1.3744654358807508969423832544e-16,
    3.57089652850837359099688597333333e-19,
    7.63164366011643737667498666666666e-22,
    1.36542498844078185908053333333333e-24,
    2.07527526690666808319999999999999e-27, 2.7128142180729856e-30,
    3.08259388791466666666666666666666e-33
  };
  static double ak0cs[38] =
    { -.07643947903327941424082978270088, -.02235652605699819052023095550791,
    7.734181154693858235300618174047e-4, -4.281006688886099464452146435416e-5,
    3.08170017386297474365001482666e-6, -2.639367222009664974067448892723e-7,
    2.563713036403469206294088265742e-8, -2.742705549900201263857211915244e-9,
    3.169429658097499592080832873403e-10,
    -3.902353286962184141601065717962e-11,
    5.068040698188575402050092127286e-12,
    -6.889574741007870679541713557984e-13,
    9.744978497825917691388201336831e-14,
    -1.427332841884548505389855340122e-14,
    2.156412571021463039558062976527e-15,
    -3.34965425514956277218878205853e-16,
    5.335260216952911692145280392601e-17,
    -8.693669980890753807639622378837e-18,
    1.446404347862212227887763442346e-18,
    -2.452889825500129682404678751573e-19,
    4.2337545262321715728217063424e-20, -7.427946526454464195695341294933e-21,
    1.3231505293926668662779674624e-21, -2.390587164739649451335981465599e-22,
    4.376827585923226140165712554666e-23,
    -8.113700607345118059339011413333e-24,
    1.521819913832172958310378154666e-24,
    -2.886041941483397770235958613333e-25,
    5.530620667054717979992610133333e-26,
    -1.070377329249898728591633066666e-26,
    2.091086893142384300296328533333e-27,
    -4.121713723646203827410261333333e-28,
    8.19348397112130764013568e-29, -1.642000275459297726780757333333e-29,
    3.316143281480227195890346666666e-30,
    -6.746863644145295941085866666666e-31,
    1.382429146318424677635413333333e-31,
    -2.851874167359832570811733333333e-32
  };
  static double ak02cs[33] =
    { -.01201869826307592239839346212452, -.009174852691025695310652561075713,
    1.444550931775005821048843878057e-4, -4.013614175435709728671021077879e-6,
    1.567831810852310672590348990333e-7, -7.77011043852173771031579975446e-9,
    4.611182576179717882533130529586e-10,
    -3.158592997860565770526665803309e-11,
    2.435018039365041127835887814329e-12,
    -2.074331387398347897709853373506e-13,
    1.925787280589917084742736504693e-14,
    -1.927554805838956103600347182218e-15,
    2.062198029197818278285237869644e-16,
    -2.341685117579242402603640195071e-17,
    2.805902810643042246815178828458e-18,
    -3.530507631161807945815482463573e-19,
    4.645295422935108267424216337066e-20,
    -6.368625941344266473922053461333e-21,
    9.0695213109865155676223488e-22, -1.337974785423690739845005311999e-22,
    2.03983602185995231552208896e-23, -3.207027481367840500060869973333e-24,
    5.189744413662309963626359466666e-25,
    -8.629501497540572192964607999999e-26,
    1.4721611831025598552080384e-26, -2.573069023867011283812351999999e-27,
    4.60177408664351658737664e-28, -8.411555324201093737130666666666e-29,
    1.569806306635368939301546666666e-29,
    -2.988226453005757788979199999999e-30,
    5.796831375216836520618666666666e-31,
    -1.145035994347681332155733333333e-31,
    2.301266594249682802005333333333e-32
  };
  static int first = TRUE;

  /* System generated locals */
  double ret_val, d__1;

  /* Local variables */
  static double xsml;
  static int ntak0;
  double y;
  static int ntak02;
  double eta;
  static int ntk0;

  /****BEGIN PROLOGUE  DBSK0E 
   ****PURPOSE  Compute the exponentially scaled modified (hyperbolic) 
   *           Bessel function of the third kind of order zero. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C10B1 
   ****TYPE      DOUBLE PRECISION (BESK0E-S, DBSK0E-D) 
   ****KEYWORDS  EXPONENTIALLY SCALED, FNLIB, HYPERBOLIC BESSEL FUNCTION, 
   *            MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS, 
   *            THIRD KIND 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *DBSK0E(X) computes the double precision exponentially scaled 
   *modified (hyperbolic) Bessel function of the third kind of 
   *order zero for positive double precision argument X. 
   * 
   *Series for BK0        on the interval  0.          to  4.00000E+00 
   *                                       with weighted error   3.08E-33 
   *                                        log weighted error  32.51 
   *                              significant figures required  32.05 
   *                                   decimal places required  33.11 
   * 
   *Series for AK0        on the interval  1.25000E-01 to  5.00000E-01 
   *                                       with weighted error   2.85E-32 
   *                                        log weighted error  31.54 
   *                              significant figures required  30.19 
   *                                   decimal places required  32.33 
   * 
   *Series for AK02       on the interval  0.          to  1.25000E-01 
   *                                       with weighted error   2.30E-32 
   *                                        log weighted error  31.64 
   *                              significant figures required  29.68 
   *                                   decimal places required  32.40 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, DBESI0, DCSEVL, INITDS, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770701  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890531  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   ****END PROLOGUE  DBSK0E 
   */
  /****FIRST EXECUTABLE STATEMENT  DBSK0E 
   */
  if (first)
    {
      eta = nsp_calpack_d1mach (&c__3) * .1;
      ntk0 = nsp_calpack_initds (bk0cs, &c__16, &eta);
      ntak0 = nsp_calpack_initds (ak0cs, &c__38, &eta);
      ntak02 = nsp_calpack_initds (ak02cs, &c__33, &eta);
      xsml = sqrt (nsp_calpack_d1mach (&c__3) * 4.);
    }
  first = FALSE;
  /* 
   */
  if (*x <= 0.)
    {
      nsp_calpack_xermsg ("SLATEC", "DBSK0E", "X IS ZERO OR NEGATIVE", &c__2,
			  &c__2, 6L, 6L, 21L);
    }
  if (*x > 2.)
    {
      goto L20;
    }
  /* 
   */
  y = 0.;
  if (*x > xsml)
    {
      y = *x * *x;
    }
  d__1 = y * .5 - 1.;
  ret_val =
    exp (*x) * (-log (*x * .5) * nsp_calpack_dbesi0 (x) - .25 +
		nsp_calpack_dcsevl (&d__1, bk0cs, &ntk0));
  return ret_val;
  /* 
   */
L20:
  if (*x <= 8.)
    {
      d__1 = (16. / *x - 5.) / 3.;
      ret_val =
	(nsp_calpack_dcsevl (&d__1, ak0cs, &ntak0) + 1.25) / sqrt (*x);
    }
  if (*x > 8.)
    {
      d__1 = 16. / *x - 1.;
      ret_val =
	(nsp_calpack_dcsevl (&d__1, ak02cs, &ntak02) + 1.25) / sqrt (*x);
    }
  /* 
   */
  return ret_val;
}				/* dbsk0e_ */
#endif
