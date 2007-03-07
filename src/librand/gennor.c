#include "grand.h"


/**
 * rand_gennor:
 * @av: Mean of the normal distribution
 * @sd: Standard deviation of the normal distribution.
 * 
 *     generates a single random deviate from a normal distribution 
 *     with mean, @av, and standard deviation, @sd. 
 *     renames snorm from toms as slightly modified by bwb to use ranf 
 *     instead of sunif. 
 *     for details see: 
 *               ahrens, j.h. and dieter, u. 
 *               extensions of forsythe's method for random 
 *               sampling from the normal distribution. 
 *               math. comput., 27,124 (oct. 1973), 927 - 937. 
 * 
 * Return value: 
 **/

double rand_gennor (double av, double sd)
{
  return  sd * rand_snorm () + av;
}		

