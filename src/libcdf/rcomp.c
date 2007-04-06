#include "cdf.h"

/*     ------------------- */
/*     EVALUATION OF EXP(-X)*X**A/GAMMA(A) */
/*     ------------------- */
/*     ------------------- */


double cdf_rcomp (double *a, double *x)
{
  /*     RT2PIN = 1/SQRT(2*PI) */
  const double rt2pin = .398942280401433;
  double ret_val, d__1;
  double t, u, t1;

  ret_val = 0.;
  if (*a >= 20.)
    {
      goto L20;
    }
  t = *a * log (*x) - *x;
  if (*a >= 1.)
    {
      goto L10;
    }
  ret_val = *a * exp (t) * (cdf_gam1 (a) + 1.);
  return ret_val;
L10:
  ret_val = exp (t) / cdf_gamma (a);
  return ret_val;

L20:
  u = *x / *a;
  if (u == 0.)
    {
      return ret_val;
    }
/* Computing 2nd power */
  d__1 = 1. / *a;
  t = d__1 * d__1;
  t1 = (((t * .75 - 1.) * t + 3.5) * t - 105.) / (*a * 1260.);
  t1 -= *a * cdf_rlog (u);
  ret_val = rt2pin * sqrt (*a) * exp (t1);
  return ret_val;
}				/* rcomp_ */
