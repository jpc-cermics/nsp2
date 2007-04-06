#include "cdf.h"

/**
 * cdf_rlog1:
 * @x: a pointer to a double 
 * 
 * evaluation of the function x - ln(1 + x) 
 * 
 * Returns: a double 
 **/

double cdf_rlog1 (double x)
{
  /*test:  A=[-0.2:0.01:1]; norm(A-log(1+A) -cdf_rlog1(A))  */
  const double a = .0566749439387324;
  const double b = .0456512608815524;
  const double p0 = .333333333333333;
  const double p1 = -.224696413112536;
  const double p2 = .00620886815375787;
  const double q1 = -1.27408923933623;
  const double q2 = .354508718369557;
  double h, r, t, w, w1;

  if (x < -.39 || x > .57)
    {
      return  x - log (x + .5 + .5);
    }
  if (x < -.18)
    {
      h = x + .3;
      h /= .7;
      w1 = a - h * .3;
    }
  else if (x > .18)
    {
      h = x * .75 - .25;
      w1 = b + h / 3.;      
    }
  else 
    {
      /* argument reduction */
      h = x;
      w1 = 0.;
    }
  /* series expansion */
  r = h / (h + 2.);
  t = r * r;
  w = ((p2 * t + p1) * t + p0) / ((q2 * t + q1) * t + 1.);
  return  t * 2. * (1. / (1. - r) - r * w) + w1;
}
