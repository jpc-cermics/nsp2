#include "cdf.h"

/**
 * cdf_dln1mx:
 * @x: a double, value for which ln(1-x) is desired.
 * 
 * Returns ln(1- @x) for small @x (good accuracy if @x .le. 0.1). 
 * Note that the obvious code log(1.0-X) won't work for small @x
 * because 1.0- @x loses accuracy 
 * 
 * Returns: a double 
 **/

double cdf_dln1mx (double x)
{
  return cdf_dln1px ( -x );
}
