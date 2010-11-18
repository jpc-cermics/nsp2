#include "cdf.h"

/**
 * cdf_cumchi:
 * @x: upper limit of integration of the chi-square distribution.
 * @df: degrees of freedom of the chi-square distribution.
 * @cum: cumulative chi-square distribution
 * @ccum: compliment of cumulative chi-square distribution.
 * 
 * calculates the cumulative chi-square distribution returned in 
 * @cum and its complement in @ccum. 
 * calls incomplete gamma function (cumgam)
 * 
 * Returns: OK or FAIL 
 **/

int cdf_cumchi (double *x, double *df, double *cum, double *ccum)
{
  double a;
  double xx;
  int retval;
  a = *df * .5;
  xx = *x * .5;
  retval = cdf_cumgam (&xx, &a, cum, ccum);
  return retval;
}

