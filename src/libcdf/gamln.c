#include "cdf.h"


/*            EVALUATION OF LN(GAMMA(A)) FOR POSITIVE A */
/*     WRITTEN BY ALFRED H. MORRIS */
/*          NAVAL SURFACE WARFARE CENTER */
/*          DAHLGREN, VIRGINIA */

double cdf_gamln (double *a)
{
  /*     D = 0.5*(LN(2*PI) - 1) */
  const double d__ = .418938533204673;
  const double c0 = .0833333333333333;
  const double c1 = -.00277777777760991;
  const double c2 = 7.9365066682539e-4;
  const double c3 = -5.9520293135187e-4;
  const double c4 = 8.37308034031215e-4;
  const double c5 = -.00165322962780713;
  int i__1;
  double ret_val, d__1;
  int i__, n;
  double t, w;

  if (*a > .8)
    {
      goto L10;
    }
  ret_val = cdf_gamln1 (a) - log (*a);
  return ret_val;
 L10:
  if (*a > 2.25)
    {
      goto L20;
    }
  t = *a - .5 - .5;
  ret_val = cdf_gamln1 (&t);
  return ret_val;
 L20:
  if (*a >= 10.)
    {
      goto L40;
    }
  n = (int) (*a - 1.25);
  t = *a;
  w = 1.;
  i__1 = n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      t += -1.;
      w = t * w;
      /* L30: */
    }
  d__1 = t - 1.;
  ret_val = cdf_gamln1 (&d__1) + log (w);
  return ret_val;
 L40:
  /* Computing 2nd power */
  d__1 = 1. / *a;
  t = d__1 * d__1;
  w = (((((c5 * t + c4) * t + c3) * t + c2) * t + c1) * t + c0) / *a;
  ret_val = d__ + w + (*a - .5) * (log (*a) - 1.);
  return ret_val;
}	

