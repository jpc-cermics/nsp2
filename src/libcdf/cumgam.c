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
  if ( *x <= 0.0 )
    {
      *cum = 0.;
      *ccum = 1.;
    }
  else if ( *x > DBL_MAX ) /* gratio doesn't handle gracefully Inf (returning Nan in this case) */
    {                      /* note that -Inf are handled by the previous block (bruno, april 2010) */
      *cum = 1.;
      *ccum = 0.;
    }
  else                     /* main block (handle Nan also) */
    {
      const int c0 = 0;
      cdf_gratio (a, x, cum, ccum, &c0);
    }

  return 0;
} 

