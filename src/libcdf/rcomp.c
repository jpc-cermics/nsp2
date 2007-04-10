#include "cdf.h"

/**
 * cdf_rcomp:
 * @a: 
 * @x: 
 * 
 *  evaluation of exp(-x)*x^a/gamma(a)
 * 
 * Returns: 
 **/

double cdf_rcomp (double a, double x)
{
  /*     rt2pin = 1/sqrt(2*pi) */
  const double rt2pin = .398942280401433;
  double d1, t, u, t1;

  if (a >= 20.)
    {
      u = x / a;
      if (u == 0.)
	{
	  return 0.0;
	}
      d1 = 1. / a;
      t = d1 * d1;
      t1 = (((t * .75 - 1.) * t + 3.5) * t - 105.) / (a * 1260.);
      t1 -= a * cdf_rlog (u);
      return  rt2pin * sqrt (a) * exp (t1);
    }
  t = a * log (x) - x ;
  if (a >= 1.)
    {
      return  exp (t) / cdf_gamma (a);
    }
  return  a * exp (t) * (cdf_gam1 (a) + 1.);
}

