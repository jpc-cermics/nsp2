#include "cdf.h"

/**
 * cdf_bcorr:
 * @a0: 
 * @b0: 
 * 
 *     evaluation of  del(a0) + del(b0) - del(a0 + b0)  where 
 *     ln(gamma(a)) = (a - 0.5)*ln(a) - a + 0.5*ln(2*pi) + del(a). 
 *     it is assumed that a0 .ge. 8 and b0 .ge. 8. 
 *
 * Returns: 
 **/

double cdf_bcorr (double *a0, double *b0)
{
  const double c0 = .0833333333333333;
  const double c1 = -.00277777777760991;
  const double c2 = 7.9365066682539e-4;
  const double c3 = -5.9520293135187e-4;
  const double c4 = 8.37308034031215e-4;
  const double c5 = -.00165322962780713;
  double ret_val, d1;
  double a, b, c, h, t, w, x, s3, s5, s7, x2, s9, s11;

  a = Min (*a0, *b0);
  b = Max (*a0, *b0);

  h = a / b;
  c = h / (h + 1.);
  x = 1. / (h + 1.);
  x2 = x * x;

  /* set sn = (1 - x**n)/(1 - x) */

  s3 = x + x2 + 1.;
  s5 = x + x2 * s3 + 1.;
  s7 = x + x2 * s5 + 1.;
  s9 = x + x2 * s7 + 1.;
  s11 = x + x2 * s9 + 1.;

  /* set w = del(b) - del(a + b) */

  /* Computing 2nd power */
  d1 = 1. / b;
  t = d1 * d1;
  w = ((((c5*s11*t + c4*s9)*t + c3*s7)*t + c2*s5)*t + c1*s3)*t + c0;
  w *= c / b;
  /* compute  del(a) + w */
  /* Computing 2nd power */
  d1 = 1. / a;
  t = d1 * d1;
  ret_val = (((((c5 * t + c4) * t + c3) * t + c2) * t + c1) * t + c0) / a + w;
  return ret_val;
}		

