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
 *     the cumulative gamma plus handle special cases (s is Nan or xlam is 0)
 *
 * Returns: OK or FAIL 
 **/

int cdf_cumpoi (double *s, double *xlam, double *cum, double *ccum)
{
  int retval = OK;
  if ( isnan(*s) )
    {
      *cum = *ccum = *s;
    }
  else if ( *xlam != 0.0 )
    {
      double a = *s + 1.0;
      retval = cdf_cumgam (xlam, &a, ccum, cum);
    }
  else  /* xlam = 0: special case : this is the cdf of a constant random variable of value 0 */
    {
      if ( *s >= 0.0 )
	{
	  *cum = 1.0; *ccum = 0.0;
	}
      else
	{
	  *cum = 0.0; *ccum = 1.0;
	}
    }
  return retval;
}



