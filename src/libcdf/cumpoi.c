#include "cdf.h"

/**
 * cdf_cumpoi:
 * @s: upper limit of cumulation of the poisson.
 * @xlam: mean of the poisson distribution.
 * @cum: cumulative poisson distribution.
 * @ccum:  compliment of cumulative poisson distribution.
 * 
 * 
 *     returns the  probability  of @s or fewer events in a poisson 
 *     distribution with mean @xlam. 
 *     uses formula  26.4.21   of   abramowitz and  stegun,  handbook  of 
 *     mathematical   functions  to reduce   the   cumulative poisson  to 
 *     the cumulative chi-square distribution. 
 *
 *     Change (Bruno Pincon sept 2010) : use directly the expression function of 
 *     the cumulative gamma.
 * Returns: 
 **/

int cdf_cumpoi (double *s, double *xlam, double *cum, double *ccum)
{
  if ( *xlam != 0.0 )
    {
      double a = *s + 1.0;
      cdf_cumgam (xlam, &a, ccum, cum);
    }
  else  /* xlam = 0: special case : this is the cdf of a constant random variable of value 0 */
    {
      if ( *s >= 0.0 )
	{
	  *cum = 1.0; *ccum = 0.0;
	}
      else if ( *s < 0.0 )
	{
	  *cum = 0.0; *ccum = 1.0;
	}
      else  /* *s is Nan */
	{
	  *cum = *ccum = *s;
	}
    }
  return 0;
}


