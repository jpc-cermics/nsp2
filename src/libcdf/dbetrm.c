#include "cdf.h"



/**
 * cdf_dbetrm:
 * @a: One argument of the Beta
 * @b: The other argument of the Beta
 * 
 * double precision sterling remainder for complete beta function 
 * log(beta(a,b)) = lgamma(a) + lgamma(b) - lgamma(a+b) 
 * where lgamma is the log of the (complete) gamma function 
 * let zz be the approximation obtained if each log gamma is approximated 
 * by sterling's formula, i.e., 
 * sterling(z) = log( sqrt( 2*pi ) ) + ( z-0.5 ) * log( z ) - z 
 * returns log(beta(a,b)) - zz 
 * 
 * 
 * Returns: a double 
 **/

double cdf_dbetrm (double *a, double *b)
{
  double ret_val, d__1;
  /*     Try to sum from smallest to largest */
  d__1 = *a + *b;
  ret_val = -cdf_dstrem (d__1);
  d__1 = Max (*a, *b);
  ret_val += cdf_dstrem (d__1);
  d__1 = Min (*a, *b);
  ret_val += cdf_dstrem (d__1);
  return ret_val;
}

