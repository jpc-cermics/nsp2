#include "cdf.h"

/**
 * cdf_cumgam:
 * @x: the upper limit of integration of the incomplete gamma. 
 * @a: the shape parameter of the incomplete gamma. 
 * @cum:  cumulative incomplete gamma distribution
 * @ccum: compliment of cumulative incomplete gamma distribution
 * 
 * Computes the cumulative distribution of the incomplete gamma 
 * distribution, i.e., the integral from 0 to X of 
 *          (1/gam(a))*exp(-t)*t^(a-1) dt 
 * where gam(a) is the complete gamma function of a, i.e., 
 * gam(a) = integral from 0 to infinity of exp(-t)*t^(a-1) dt 
 * 
 * calls the routine gratio() to compute the result.
 * 
 * returns: 
 **/

int cdf_cumgam (double *x, double *a, double *cum, double *ccum)
{
  const int c__0 = 0;
  if (!(*x <= 0.))
    {
      goto L10;
    }
  *cum = 0.;
  *ccum = 1.;
  return 0;
 L10:
  cdf_gratio (a, x, cum, ccum, &c__0);
  return 0;
} 

