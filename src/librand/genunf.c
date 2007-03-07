#include "grand.h"

/**
 * rand_genunf:
 * @low: Low bound (exclusive) on DOUBLE PRECISION value to be generated
 * @high: High bound (exclusive) on DOUBLE PRECISION value to be generated
 * 
 * generates a double precision uniformly distributed between @low and @high.
 * 
 * Return value: 
 **/

double rand_genunf (double low, double high)
{
  return  low + (high - low) * rand_ranf ();
}


