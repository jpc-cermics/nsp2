#include "cdf.h"

/**
 * cdf_alnrel:
 * @a: pointer to a double 
 * 
 * computes the natural logarithm of 1+@a
 * 
 * Returns: a double 
 **/

double cdf_alnrel (double a)
{
  /* test:  a=0.01:0.01:10; norm(cdf_alnrel(a) - log(1+a)) */
  static const double p1 = -1.29418923021993;
  static const double p2 = .405303492862024;
  static const double p3 = -.0178874546012214;
  static const double q1 = -1.62752256355323;
  static const double q2 = .747811014037616;
  static const double q3 = -.0845104217945565;
  double t, w, t2;
  if (Abs(a) > 0.375)
    {
      return log(1+a);
    }
  t = a / (a + 2.);
  t2 = t * t;
  w = (((p3 * t2 + p2) * t2 + p1) * t2 + 1.) 
    / (((q3 * t2 + q2) * t2 + q1) * t2 + 1.);
  return t * 2. * w;
}
