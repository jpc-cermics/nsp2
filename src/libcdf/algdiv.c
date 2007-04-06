#include "cdf.h"


/**
 * cdf_algdiv:
 * @a: 
 * @b: 
 * 
 *     computation of ln(gamma(b)/gamma(a+b)) when b >= 8 
 *     in this algorithm, del(x) is the function defined by 
 *     ln(gamma(x)) = (x - 0.5)*ln(x) - x + 0.5*ln(2*pi) + del(x). 
 * 
 * Returns: a double 
 **/

double cdf_algdiv (double *a, double *b)
{
  const double c0 = .0833333333333333;
  const double c1 = -.00277777777760991;
  const double c2 = 7.9365066682539e-4;
  const double c3 = -5.9520293135187e-4;
  const double c4 = 8.37308034031215e-4;
  const double c5 = -.00165322962780713;
  double ret_val, d1;
  double c, d, h, t, u, v, w, x, s3, s5, s7, x2, s9, s11;

  if (*a <= *b)
    {
      goto L10;
    }
  h = *b / *a;
  c = 1. / (h + 1.);
  x = h / (h + 1.);
  d = *a + (*b - .5);
  goto L20;
 L10:
  h = *a / *b;
  c = h / (h + 1.);
  x = 1. / (h + 1.);
  d = *b + (*a - .5);
  /* SET SN = (1 - X**N)/(1 - X) */
 L20:
  x2 = x * x;
  s3 = x + x2 + 1.;
  s5 = x + x2 * s3 + 1.;
  s7 = x + x2 * s5 + 1.;
  s9 = x + x2 * s7 + 1.;
  s11 = x + x2 * s9 + 1.;
  /* SET W = DEL(B) - DEL(A + B) */
  /* Computing 2nd power */
  d1 = 1. / *b;
  t = d1 * d1;
  w =
    ((((c5 * s11 * t + c4 * s9) * t + c3 * s7) * t + c2 * s5) * t +
     c1 * s3) * t + c0;
  w *= c / *b;
  /* COMBINE THE RESULTS */
  d1 = *a / *b;
  u = d * cdf_alnrel (&d1);
  v = *a * (log (*b) - 1.);
  if (u <= v)
    {
      ret_val = w - u - v;
      return ret_val;
    }
  ret_val = w - v - u;
  return ret_val;
}
