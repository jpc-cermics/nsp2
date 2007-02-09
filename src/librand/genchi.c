#include "grand.h"


/**
 * rand_genchi:
 * @df: degrees of freedom of the chisquare (Must be positive)
 * 
 * Generates random deviate from the distribution of a chisquare 
 * with @df degrees of freedom random variable. 
 * Uses relation between chisquare and gamma.
 * 
 * Return value: a double  
 **/ 

double rand_genchi (double *df)
{
  return rand_sgamma(*df / 2.) * 2.;
}	



