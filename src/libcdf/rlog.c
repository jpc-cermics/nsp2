#include "cdf.h"

/**
 * cdf_rlog:
 * @x: a pointer to a double 
 * 
 * evaluation of the function  x - 1 - LN(x)
 * 
 * Returns: a double 
 **/


double cdf_rlog (double x)
{
  /*test:  A=[0.1:0.01:2]; norm(A-1 - log(A) - cdf_rlog(A))  */
  const double a = .0566749439387324;
  const double b = .0456512608815524;
  const double p0 = .333333333333333;
  const double p1 = -.224696413112536;
  const double p2 = .00620886815375787;
  const double q1 = -1.27408923933623;
  const double q2 = .354508718369557;
  double ret_val;
  double r, t, u, w, w1;
  if (x < .61 || x > 1.57)
    {
      r = x - .5 - .5;
      ret_val = r - log (x);
      return ret_val;
    }
  if (x < .82)
    {
      u = x - .7;
      u /= .7;
      w1 = a - u * .3;
    }
  else if (x > 1.18)
    {
      u = x * .75 - 1.;
      w1 = b + u / 3.;
    }
  else 
    {
      /*   argument reduction */
      u = x - .5 - .5;
      w1 = 0.;
    }
  /*       series expansion */
  r = u / (u + 2.);
  t = r * r;
  w = ((p2 * t + p1) * t + p0) / ((q2 * t + q1) * t + 1.);
  ret_val = t * 2. * (1. / (1. - r) - r * w) + w1;
  return ret_val;
}	

