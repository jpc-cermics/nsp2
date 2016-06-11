/* dbesi0.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__18 = 18;
static int c__3 = 3;
static int c__2 = 2;

/*DECK DBESI0 
 */
double nsp_calpack_dbesi0 (double *x)
{
  /* Initialized data */

  static double bi0cs[18] = { -.07660547252839144951081894976243285,
    1.927337953993808269952408750881196, .2282644586920301338937029292330415,
    .01304891466707290428079334210691888,
    4.344270900816487451378682681026107e-4,
    9.422657686001934663923171744118766e-6,
    1.434006289510691079962091878179957e-7,
    1.613849069661749069915419719994611e-9,
    1.396650044535669699495092708142522e-11,
    9.579451725505445344627523171893333e-14,
    5.333981859862502131015107744e-16,
    2.458716088437470774696785919999999e-18,
    9.535680890248770026944341333333333e-21,
    3.154382039721427336789333333333333e-23,
    9.004564101094637431466666666666666e-26,
    2.240647369123670016e-28, 4.903034603242837333333333333333333e-31,
    9.508172606122666666666666666666666e-34
  };
  static int first = TRUE;

  /* System generated locals */
  double ret_val, d__1;

  /* Local variables */
  static double xmax, xsml;
  double y;
  static int nti0;

  /****BEGIN PROLOGUE  DBESI0 
   ****PURPOSE  Compute the hyperbolic Bessel function of the first kind 
   *           of order zero. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C10B1 
   ****TYPE      DOUBLE PRECISION (BESI0-S, DBESI0-D) 
   ****KEYWORDS  FIRST KIND, FNLIB, HYPERBOLIC BESSEL FUNCTION, 
   *            MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *DBESI0(X) calculates the double precision modified (hyperbolic) 
   *Bessel function of the first kind of order zero and double 
   *precision argument X. 
   * 
   *Series for BI0        on the interval  0.          to  9.00000E+00 
   *                                       with weighted error   9.51E-34 
   *                                        log weighted error  33.02 
   *                              significant figures required  33.31 
   *                                   decimal places required  33.65 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, DBSI0E, DCSEVL, INITDS, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770701  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890531  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   ****END PROLOGUE  DBESI0 
   */
  /****FIRST EXECUTABLE STATEMENT  DBESI0 
   */
  if (first)
    {
      d__1 = nsp_calpack_d1mach (&c__3) * .1;
      nti0 = nsp_calpack_initds (bi0cs, &c__18, &d__1);
      xsml = sqrt (nsp_calpack_d1mach (&c__3) * 4.5);
      xmax = log (nsp_calpack_d1mach (&c__2));
    }
  first = FALSE;
  /* 
   */
  y = Abs (*x);
  if (y > 3.)
    {
      goto L20;
    }
  /* 
   */
  ret_val = 1.;
  if (y > xsml)
    {
      d__1 = y * y / 4.5 - 1.;
      ret_val = nsp_calpack_dcsevl (&d__1, bi0cs, &nti0) + 2.75;
    }
  return ret_val;
  /* 
   */
L20:
  if (y > xmax)
    {
      nsp_calpack_xermsg ("SLATEC", "DBESI0", "ABS(X) SO BIG I0 OVERFLOWS",
			  &c__2, &c__2, 6L, 6L, 26L);
    }
  /* 
   */
  ret_val = exp (y) * nsp_calpack_dbsi0e (x);
  /* 
   */
  return ret_val;
}				/* dbesi0_ */
#endif
