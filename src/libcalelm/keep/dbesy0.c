/* dbesy0.f -- translated by f2c (version 19961017).
 *
 *
 */

#if 0
#include "calpack.h"

/* Table of constant values */

static int c__19 = 19;
static int c__3 = 3;
static int c__1 = 1;
static int c__2 = 2;

/*DECK DBESY0 
 */
double nsp_calpack_dbesy0 (double *x)
{
  /* Initialized data */

  static double by0cs[19] =
    { -.01127783939286557321793980546028, -.1283452375604203460480884531838,
    -.1043788479979424936581762276618, .02366274918396969540924159264613,
    -.002090391647700486239196223950342, 1.039754539390572520999246576381e-4,
    -3.369747162423972096718775345037e-6, 7.729384267670667158521367216371e-8,
    -1.324976772664259591443476068964e-9,
    1.764823261540452792100389363158e-11,
    -1.881055071580196200602823012069e-13,
    1.641865485366149502792237185749e-15,
    -1.19565943860460608574599100672e-17,
    7.377296297440185842494112426666e-20,
    -3.906843476710437330740906666666e-22, 1.79550366443615794982912e-24,
    -7.229627125448010478933333333333e-27,
    2.571727931635168597333333333333e-29,
    -8.141268814163694933333333333333e-32
  };
  static double twodpi = .636619772367581343075535053490057;
  static int first = TRUE;

  /* System generated locals */
  double ret_val, d__1;

  /* Local variables */
  double ampl;
  static double xsml;
  double y;
  double theta;
  static int nty0;

  /****BEGIN PROLOGUE  DBESY0 
   ****PURPOSE  Compute the Bessel function of the second kind of order 
   *           zero. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C10A1 
   ****TYPE      DOUBLE PRECISION (BESY0-S, DBESY0-D) 
   ****KEYWORDS  BESSEL FUNCTION, FNLIB, ORDER ZERO, SECOND KIND, 
   *            SPECIAL FUNCTIONS 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *DBESY0(X) calculates the double precision Bessel function of the 
   *second kind of order zero for double precision argument X. 
   * 
   *Series for BY0        on the interval  0.          to  1.60000E+01 
   *                                       with weighted error   8.14E-32 
   *                                        log weighted error  31.09 
   *                              significant figures required  30.31 
   *                                   decimal places required  31.73 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, D9B0MP, DBESJ0, DCSEVL, INITDS, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770701  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890531  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   ****END PROLOGUE  DBESY0 
   */
  /****FIRST EXECUTABLE STATEMENT  DBESY0 
   */
  if (first)
    {
      d__1 = nsp_calpack_d1mach (&c__3) * .1;
      nty0 = nsp_calpack_initds (by0cs, &c__19, &d__1);
      xsml = sqrt (nsp_calpack_d1mach (&c__3) * 4.);
    }
  first = FALSE;
  /* 
   */
  if (*x <= 0.)
    {
      nsp_calpack_xermsg ("SLATEC", "DBESY0", "X IS ZERO OR NEGATIVE", &c__1,
			  &c__2, 6L, 6L, 21L);
    }
  if (*x > 4.)
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
  d__1 = y * .125 - 1.;
  ret_val =
    twodpi * log (*x * .5) * nsp_calpack_dbesj0 (x) + .375 +
    nsp_calpack_dcsevl (&d__1, by0cs, &nty0);
  return ret_val;
  /* 
   */
L20:
  nsp_calpack_d9b0mp (x, &ampl, &theta);
  ret_val = ampl * sin (theta);
  return ret_val;
  /* 
   */
}				/* dbesy0_ */
#endif
