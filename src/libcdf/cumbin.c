#include "cdf.h"

/**
 * cdf_cumbin:
 * @s: The upper limit of cumulation of the binomial distribution. 
 * @xn:  The number of binomial trials
 * @pr: The probability of success in each binomial trial. 
 * @ompr:  1 - @pr
 * @cum: returns the cumulative binomial distribution
 * @ccum: returns the compliment of cumulative binomial distribution. 
 * 
 * Cumulative binomial distribution. Returns the probability  of 0  to @s
 * successes in  @xn   binomial trials, each of which has a probability of 
 * success, @pr.
 * 
 *     Formula  26.5.24    of   Abramowitz  and    Stegun,  Handbook   of 
 *     Mathematical   Functions (1966) is   used  to reduce the  binomial 
 *     distribution  to  the  cumulative    beta distribution. 
 * 
 * Returns: 0
 **/

int cdf_cumbin (double *s, double *xn, double *pr, double *ompr, double *cum, double *ccum)
{
  if ( isnan(*s) )
    {
      *cum = *ccum = *s;
    }
  else if ( *s < 0.0 )
    {
      *cum = 0.0; *ccum = 1.0;
    }
  else if ( *s >= *xn )
    {
      *cum = 1.0; *ccum = 0.;
    }
  else
    {
      double d1, d2;
      d1 = *s + 1.0;
      d2 = *xn - *s;
      cdf_cumbet (pr, ompr, &d1, &d2, ccum, cum);
    }
  return 0;
}				
