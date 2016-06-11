/* dlngam.f -- translated by f2c (version 19961017).
 *
 *
 */

#if 0
#include "calpack.h"

/* Table of constant values */

static int c__2 = 2;
static int c__4 = 4;
static int c__3 = 3;
static int c__1 = 1;

/*DECK DLNGAM 
 */
double nsp_calpack_dlngam (double *x)
{
  /* Initialized data */

  static double sq2pil = .91893853320467274178032973640562;
  static double sqpi2l = .225791352644727432363097614947441;
  static double pi = 3.1415926535897932384626433832795;
  static int first = TRUE;

  /* System generated locals */
  double ret_val, d__1, d__2;

  /* Local variables */
  double temp;
  static double xmax;
  double y;
  static double dxrel;
  double sinpiy;

  /****BEGIN PROLOGUE  DLNGAM 
   ****PURPOSE  Compute the logarithm of the absolute value of the Gamma 
   *           function. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C7A 
   ****TYPE      DOUBLE PRECISION (ALNGAM-S, DLNGAM-D, CLNGAM-C) 
   ****KEYWORDS  ABSOLUTE VALUE, COMPLETE GAMMA FUNCTION, FNLIB, LOGARITHM, 
   *            SPECIAL FUNCTIONS 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *DLNGAM(X) calculates the double precision logarithm of the 
   *absolute value of the Gamma function for double precision 
   *argument X. 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, D9LGMC, DGAMMA, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770601  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890531  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   *  900727  Added EXTERNAL statement.  (WRB) 
   ****END PROLOGUE  DLNGAM 
   */
  /****FIRST EXECUTABLE STATEMENT  DLNGAM 
   */
  if (first)
    {
      temp = 1. / log (nsp_calpack_d1mach (&c__2));
      xmax = temp * nsp_calpack_d1mach (&c__2);
      dxrel = sqrt (nsp_calpack_d1mach (&c__4));
    }
  first = FALSE;
  /* 
   */
  y = Abs (*x);
  if (y > 10.)
    {
      goto L20;
    }
  /* 
   *LOG (ABS(DGAMMA(X)) ) FOR ABS(X) .LE. 10.0 
   * 
   */
  ret_val = log ((d__1 = nsp_calpack_dgamma (x), Abs (d__1)));
  return ret_val;
  /* 
   *LOG ( ABS(DGAMMA(X)) ) FOR ABS(X) .GT. 10.0 
   * 
   */
L20:
  if (y > xmax)
    {
      nsp_calpack_xermsg ("SLATEC", "DLNGAM",
			  "ABS(X) SO BIG DLNGAM OVERFLOWS", &c__2, &c__2, 6L,
			  6L, 30L);
    }
  /* 
   */
  if (*x > 0.)
    {
      ret_val = sq2pil + (*x - .5) * log (*x) - *x + nsp_calpack_d9lgmc (&y);
    }
  if (*x > 0.)
    {
      return ret_val;
    }
  /* 
   */
  sinpiy = (d__1 = sin (pi * y), Abs (d__1));
  if (sinpiy == 0.)
    {
      nsp_calpack_xermsg ("SLATEC", "DLNGAM", "X IS A NEGATIVE INT", &c__3,
			  &c__2, 6L, 6L, 23L);
    }
  /* 
   */
  d__2 = *x - .5;
  if ((d__1 = (*x - d_int (&d__2)) / *x, Abs (d__1)) < dxrel)
    {
      nsp_calpack_xermsg ("SLATEC", "DLNGAM",
			  "ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NEGATIVE INT",
			  &c__1, &c__1, 6L, 6L, 60L);
    }
  /* 
   */
  ret_val =
    sqpi2l + (*x - .5) * log (y) - *x - log (sinpiy) -
    nsp_calpack_d9lgmc (&y);
  return ret_val;
  /* 
   */
}				/* dlngam_ */
#endif
