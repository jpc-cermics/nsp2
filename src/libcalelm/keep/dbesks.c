/* dbesks.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__2 = 2;

/*DECK DBESKS 
 */
int nsp_calpack_dbesks (double *xnu, double *x, int *nin, double *bk)
{
  /* Initialized data */

  static double xmax = 0.;

  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, n;
  double expxi;

  /****BEGIN PROLOGUE  DBESKS 
   ****PURPOSE  Compute a sequence of modified Bessel functions of the 
   *           third kind of fractional order. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C10B3 
   ****TYPE      DOUBLE PRECISION (BESKS-S, DBESKS-D) 
   ****KEYWORDS  FNLIB, FRACTIONAL ORDER, MODIFIED BESSEL FUNCTION, 
   *            SEQUENCE OF BESSEL FUNCTIONS, SPECIAL FUNCTIONS, 
   *            THIRD KIND 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *DBESKS computes a sequence of modified Bessel functions of the third 
   *kind of order XNU + I at X, where X .GT. 0, XNU lies in (-1,1), 
   *and I = 0, 1, ... , NIN - 1, if NIN is positive and I = 0, 1, ... , 
   *NIN + 1, if NIN is negative.  On return, the vector BK(.) contains 
   *the results at X for order starting at XNU.  XNU, X, and BK are 
   *double precision.  NIN is an int. 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, DBSKES, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770601  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890831  Modified array declarations.  (WRB) 
   *  890831  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   ****END PROLOGUE  DBESKS 
   */
  /* Parameter adjustments */
  --bk;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  DBESKS 
   */
  if (xmax == 0.)
    {
      xmax = -log (nsp_calpack_d1mach (&c__1));
    }
  /* 
   */
  if (*x > xmax)
    {
      nsp_calpack_xermsg ("SLATEC", "DBESKS", "X SO BIG BESSEL K UNDERFLOWS",
			  &c__1, &c__2, 6L, 6L, 28L);
    }
  /* 
   */
  nsp_calpack_dbskes (xnu, x, nin, &bk[1]);
  /* 
   */
  expxi = exp (-(*x));
  n = Abs (*nin);
  i__1 = n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      bk[i__] = expxi * bk[i__];
      /* L20: */
    }
  /* 
   */
  return 0;
}				/* dbesks_ */
#endif
