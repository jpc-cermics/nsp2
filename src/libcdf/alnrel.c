#include "cdf.h"

/*
 * ln(1 + A) 
 */

double cdf_alnrel (double *a)
{
  /* Initialized data */
  const double p1 = -1.29418923021993;
  const double p2 = .405303492862024;
  const double p3 = -.0178874546012214;
  const double q1 = -1.62752256355323;
  const double q2 = .747811014037616;
  const double q3 = -.0845104217945565;
  /* System generated locals */
  double ret_val;
  /* Local variables */
  double t, w, x, t2;
  if (Abs (*a) > .375)
    {
      x = *a + 1.;
      ret_val = log (x);
      return ret_val;
    }
  t = *a / (*a + 2.);
  t2 = t * t;
  w =
    (((p3 * t2 + p2) * t2 + p1) * t2 +
     1.) / (((q3 * t2 + q2) * t2 + q1) * t2 + 1.);
  ret_val = t * 2. * w;
  return ret_val;
}
