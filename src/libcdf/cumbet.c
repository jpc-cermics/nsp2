#include "cdf.h"

/**
 * cdf_cumbet:
 * @x: Upper limit of integration. 
 * @y: 1 - @x
 * @a: First parameter of the beta distribution.
 * @b: Second parameter of the beta distribution. 
 * @cum: Cumulative incomplete beta distribution. 
 * @ccum: Compliment of Cumulative incomplete beta distribution. 
 * 
 * 
 *     Calculates the cdf to X of the incomplete beta distribution 
 *     with parameters a and b.  This is the integral from 0 to x 
 *     of (1/B(a,b))*f(t)) where f(t) = t**(a-1) * (1-t)**(b-1) 
 *     The cdf and its complement are returned in @cum and @ccum.
 * 
 *     Calls the routine BRATIO. 
 *     References 
 *     Didonato, Armido R. and Morris, Alfred H. Jr. (1992) Algorithm 
 *     708 Significant Digit Computation of the Incomplete Beta Function 
 *     Ratios. ACM ToMS, Vol.18, No. 3, Sept. 1992, 360-373. 
 *     COPYRIGHT ACM TOMS. 
 * 
 * Returns: 0
 **/

int cdf_cumbet (double *x, double *y, double *a, double *b, double *cum,double *ccum)
{
  int ierr;
  if (*x <= 0.0 )
    {
      *cum = 0.;
      *ccum = 1.;
      return 0;
    }
  if (*y <= 0.0)
    {
      *cum = 1.;
      *ccum = 0.;
      return 0;
    }
  cdf_bratio (*a, *b, *x, *y, cum, ccum, &ierr);
  return 0;
}

