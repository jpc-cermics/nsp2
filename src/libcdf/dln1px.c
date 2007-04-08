#include "cdf.h"


/**
 * cdf_dln1px:
 * @a: a double, value for which ln(1+a) is desired.
 * 
 * Returns ln(1+ @a) for small @a (good accuracy if @a .le. 0.1). 
 * Note that the obvious code log(1.0 + @a) won't work for small @a
 * because 1.0+ @a loses accuracy 
 * 
 *     Renames ALNREL from: 
 *     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant 
 *     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM 
 *     Trans. Math.  Softw. 18 (1993), 360-373. 
 * 
 * Returns: a double 
 **/

double cdf_dln1px (double a)
{
  const double p1 = -1.29418923021993;
  const double p2 = .405303492862024;
  const double p3 = -.0178874546012214;
  const double q1 = -1.62752256355323;
  const double q2 = .747811014037616;
  const double q3 = -.0845104217945565;
  double t, w,  t2;

  if (Abs(a) > 0.375)  return log(1+a);
  t = a / (a + 2.);
  t2 = t * t;
  w = (((p3 * t2 + p2) * t2 + p1) * t2 + 1.) 
    / (((q3 * t2 + q2) * t2 + q1) * t2 + 1.);
  return  t * 2. * w;
}	

