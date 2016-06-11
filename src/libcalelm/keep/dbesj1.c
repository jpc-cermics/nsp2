/* dbesj1.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__19 = 19;
static int c__3 = 3;
static int c__1 = 1;

/*DECK DBESJ1 
 */
double nsp_calpack_dbesj1 (double *x)
{
  /* Initialized data */

  static double bj1cs[19] = { -.117261415133327865606240574524003,
    -.253615218307906395623030884554698, .0501270809844695685053656363203743,
    -.00463151480962508191842619728789772,
    2.47996229415914024539124064592364e-4,
    -8.67894868627882584521246435176416e-6,
    2.14293917143793691502766250991292e-7,
    -3.93609307918317979229322764073061e-9,
    5.59118231794688004018248059864032e-11,
    -6.3276164046613930247769527401488e-13,
    5.84099161085724700326945563268266e-15,
    -4.48253381870125819039135059199999e-17, 2.90538449262502466306018688e-19,
    -1.61173219784144165412118186666666e-21,
    7.73947881939274637298346666666666e-24,
    -3.24869378211199841143466666666666e-26, 1.2022376772274102272e-28,
    -3.95201221265134933333333333333333e-31,
    1.16167808226645333333333333333333e-33
  };
  static int first = TRUE;

  /* System generated locals */
  double ret_val, d__1;

  /* Local variables */
  double ampl;
  static double xmin, xsml;
  double y;
  double theta;
  static int ntj1;

  /****BEGIN PROLOGUE  DBESJ1 
   ****PURPOSE  Compute the Bessel function of the first kind of order one. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C10A1 
   ****TYPE      DOUBLE PRECISION (BESJ1-S, DBESJ1-D) 
   ****KEYWORDS  BESSEL FUNCTION, FIRST KIND, FNLIB, ORDER ONE, 
   *            SPECIAL FUNCTIONS 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *DBESJ1(X) calculates the double precision Bessel function of the 
   *first kind of order one for double precision argument X. 
   * 
   *Series for BJ1        on the interval  0.          to  1.60000E+01 
   *                                       with weighted error   1.16E-33 
   *                                        log weighted error  32.93 
   *                              significant figures required  32.36 
   *                                   decimal places required  33.57 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, D9B1MP, DCSEVL, INITDS, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  780601  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890531  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   *  910401  Corrected error in code which caused values to have the 
   *          wrong sign for arguments less than 4.0.  (WRB) 
   ****END PROLOGUE  DBESJ1 
   */
  /****FIRST EXECUTABLE STATEMENT  DBESJ1 
   */
  if (first)
    {
      d__1 = nsp_calpack_d1mach (&c__3) * .1;
      ntj1 = nsp_calpack_initds (bj1cs, &c__19, &d__1);
      /* 
       */
      xsml = sqrt (nsp_calpack_d1mach (&c__3) * 8.);
      xmin = nsp_calpack_d1mach (&c__1) * 2.;
    }
  first = FALSE;
  /* 
   */
  y = Abs (*x);
  if (y > 4.)
    {
      goto L20;
    }
  /* 
   */
  ret_val = 0.;
  if (y == 0.)
    {
      return ret_val;
    }
  if (y <= xmin)
    {
      nsp_calpack_xermsg ("SLATEC", "DBESJ1",
			  "ABS(X) SO SMALL J1 UNDERFLOWS", &c__1, &c__1, 6L,
			  6L, 29L);
    }
  if (y > xmin)
    {
      ret_val = *x * .5;
    }
  if (y > xsml)
    {
      d__1 = y * .125 * y - 1.;
      ret_val = *x * (nsp_calpack_dcsevl (&d__1, bj1cs, &ntj1) + .25);
    }
  return ret_val;
  /* 
   */
L20:
  nsp_calpack_d9b1mp (&y, &ampl, &theta);
  ret_val = d_sign (&ampl, x) * cos (theta);
  /* 
   */
  return ret_val;
}				/* dbesj1_ */
#endif
