/* dbskes.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__2 = 2;
static int c__3 = 3;
static int c__4 = 4;
static double c_b18 = 1.;
static int c__5 = 5;

/*DECK DBSKES 
 */
int nsp_calpack_dbskes (double *xnu, double *x, int *nin, double *bke)
{
  /* Initialized data */

  static double alnbig = 0.;

  /* System generated locals */
  int i__1;
  double d__1, d__2;

  /* Local variables */
  double vend, bknu1;
  int i__, n;
  double v, vincr;
  double direct;
  int iswtch;

  /****BEGIN PROLOGUE  DBSKES 
   ****PURPOSE  Compute a sequence of exponentially scaled modified Bessel 
   *           functions of the third kind of fractional order. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C10B3 
   ****TYPE      DOUBLE PRECISION (BESKES-S, DBSKES-D) 
   ****KEYWORDS  EXPONENTIALLY SCALED, FNLIB, FRACTIONAL ORDER, 
   *            MODIFIED BESSEL FUNCTION, SEQUENCE OF BESSEL FUNCTIONS, 
   *            SPECIAL FUNCTIONS, THIRD KIND 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *DBSKES(XNU,X,NIN,BKE) computes a double precision sequence 
   *of exponentially scaled modified Bessel functions 
   *of the third kind of order XNU + I at X, where X .GT. 0, 
   *XNU lies in (-1,1), and I = 0, 1, ... , NIN - 1, if NIN is positive 
   *and I = 0, -1, ... , NIN + 1, if NIN is negative.  On return, the 
   *vector BKE(.) contains the results at X for order starting at XNU. 
   *XNU, X, and BKE are double precision.  NIN is int. 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, D9KNUS, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770601  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  890911  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   ****END PROLOGUE  DBSKES 
   */
  /* Parameter adjustments */
  --bke;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  DBSKES 
   */
  if (alnbig == 0.)
    {
      alnbig = log (nsp_calpack_d1mach (&c__2));
    }
  /* 
   */
  v = Abs (*xnu);
  n = Abs (*nin);
  /* 
   */
  if (v >= 1.)
    {
      nsp_calpack_xermsg ("SLATEC", "DBSKES", "ABS(XNU) MUST BE LT 1", &c__2,
			  &c__2, 6L, 6L, 21L);
    }
  if (*x <= 0.)
    {
      nsp_calpack_xermsg ("SLATEC", "DBSKES", "X IS LE 0", &c__3, &c__2, 6L,
			  6L, 9L);
    }
  if (n == 0)
    {
      nsp_calpack_xermsg ("SLATEC", "DBSKES",
			  "N THE NUMBER IN THE SEQUENCE IS 0", &c__4, &c__2,
			  6L, 6L, 33L);
    }
  /* 
   */
  nsp_calpack_d9knus (&v, x, &bke[1], &bknu1, &iswtch);
  if (n == 1)
    {
      return 0;
    }
  /* 
   */
  d__1 = (double) (*nin);
  vincr = d_sign (&c_b18, &d__1);
  direct = vincr;
  if (*xnu != 0.)
    {
      direct = vincr * d_sign (&c_b18, xnu);
    }
  if (iswtch == 1 && direct > 0.)
    {
      nsp_calpack_xermsg ("SLATEC", "DBSKES",
			  "X SO SMALL BESSEL K-SUB-XNU+1 OVERFLOWS", &c__5,
			  &c__2, 6L, 6L, 39L);
    }
  bke[2] = bknu1;
  /* 
   */
  if (direct < 0.)
    {
      d__2 = (d__1 = *xnu + vincr, Abs (d__1));
      nsp_calpack_d9knus (&d__2, x, &bke[2], &bknu1, &iswtch);
    }
  if (n == 2)
    {
      return 0;
    }
  /* 
   */
  vend = (d__1 = *xnu + *nin, Abs (d__1)) - 1.;
  if ((vend - .5) * log (vend) + .27 - vend * (log (*x) - .694) > alnbig)
    {
      nsp_calpack_xermsg ("SLATEC", "DBSKES",
			  "X SO SMALL OR ABS(NU) SO BIG THAT BESSEL K-SUB-NU OVERFLOWS",
			  &c__5, &c__2, 6L, 6L, 59L);
    }
  /* 
   */
  v = *xnu;
  i__1 = n;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      v += vincr;
      bke[i__] = v * 2. * bke[i__ - 1] / *x + bke[i__ - 2];
      /* L10: */
    }
  /* 
   */
  return 0;
}				/* dbskes_ */
#endif
