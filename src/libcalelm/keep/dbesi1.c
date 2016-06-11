/* dbesi1.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__17 = 17;
static int c__3 = 3;
static int c__1 = 1;
static int c__2 = 2;

/*DECK DBESI1 
 */
double nsp_calpack_dbesi1 (double *x)
{
  /* Initialized data */

  static double bi1cs[17] =
    { -.0019717132610998597316138503218149, .40734887667546480608155393652014,
    .034838994299959455866245037783787, .0015453945563001236038598401058489,
    4.188852109837778412945883200412e-5, 7.6490267648362114741959703966069e-7,
    1.0042493924741178689179808037238e-8,
    9.9322077919238106481371298054863e-11,
    7.6638017918447637275200171681349e-13,
    4.741418923816739498038809194816e-15,
    2.4041144040745181799863172032e-17, 1.0171505007093713649121100799999e-19,
    3.6450935657866949458491733333333e-22,
    1.1205749502562039344810666666666e-24,
    2.9875441934468088832e-27, 6.9732310939194709333333333333333e-30,
    1.43679482206208e-32
  };
  static int first = TRUE;

  /* System generated locals */
  double ret_val, d__1;

  /* Local variables */
  static double xmin, xmax, xsml;
  double y;
  static int nti1;

  /****BEGIN PROLOGUE  DBESI1 
   ****PURPOSE  Compute the modified (hyperbolic) Bessel function of the 
   *           first kind of order one. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C10B1 
   ****TYPE      DOUBLE PRECISION (BESI1-S, DBESI1-D) 
   ****KEYWORDS  FIRST KIND, FNLIB, HYPERBOLIC BESSEL FUNCTION, 
   *            MODIFIED BESSEL FUNCTION, ORDER ONE, SPECIAL FUNCTIONS 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *DBESI1(X) calculates the double precision modified (hyperbolic) 
   *Bessel function of the first kind of order one and double precision 
   *argument X. 
   * 
   *Series for BI1        on the interval  0.          to  9.00000E+00 
   *                                       with weighted error   1.44E-32 
   *                                        log weighted error  31.84 
   *                              significant figures required  31.45 
   *                                   decimal places required  32.46 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, DBSI1E, DCSEVL, INITDS, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770701  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890531  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   ****END PROLOGUE  DBESI1 
   */
  /****FIRST EXECUTABLE STATEMENT  DBESI1 
   */
  if (first)
    {
      d__1 = nsp_calpack_d1mach (&c__3) * .1;
      nti1 = nsp_calpack_initds (bi1cs, &c__17, &d__1);
      xmin = nsp_calpack_d1mach (&c__1) * 2.;
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
  ret_val = 0.;
  if (y == 0.)
    {
      return ret_val;
    }
  /* 
   */
  if (y <= xmin)
    {
      nsp_calpack_xermsg ("SLATEC", "DBESI1",
			  "ABS(X) SO SMALL I1 UNDERFLOWS", &c__1, &c__1, 6L,
			  6L, 29L);
    }
  if (y > xmin)
    {
      ret_val = *x * .5;
    }
  if (y > xsml)
    {
      d__1 = y * y / 4.5 - 1.;
      ret_val = *x * (nsp_calpack_dcsevl (&d__1, bi1cs, &nti1) + .875);
    }
  return ret_val;
  /* 
   */
L20:
  if (y > xmax)
    {
      nsp_calpack_xermsg ("SLATEC", "DBESI1", "ABS(X) SO BIG I1 OVERFLOWS",
			  &c__2, &c__2, 6L, 6L, 26L);
    }
  /* 
   */
  ret_val = exp (y) * nsp_calpack_dbsi1e (x);
  /* 
   */
  return ret_val;
}				/* dbesi1_ */
#endif
