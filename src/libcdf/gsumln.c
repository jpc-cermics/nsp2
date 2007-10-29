#include "cdf.h"

/**
 * cdf_gsumln:
 * @a: a double 
 * @b: a double
 * 
 *  evaluation of the function ln(gamma(a + b))
 *  for 1 <= a <= 2  and  1 <= b <= 2
 * 
 * Returns: 
 **/

double cdf_gsumln (double a, double b)
{
  double x = a + b - 2.;
  if (x > .25)
    {
      if (x > 1.25)
	{
	  return cdf_gamln1(x-1.0) + log (x * (x + 1.));
	}
      else 
	{
	  return  cdf_gamln1 (x) + cdf_dln1px (x);
	}
    }
  else 
    {
      return cdf_gamln1 ( x + 1.);
    }
}


