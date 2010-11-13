#include "cdf.h"

/**
 * cdf_cumnbn:
 * @s:  the number of failures 
 * @xn: the number of successes 
 * @pr: the probability of success in each binomial trial.
 * @ompr:  1 - pr 
 * @cum: cumulative negative binomial distribution. 
 * @ccum: compliment of cumulative negative binomial distribution. 
 * 
 * cumulative negative binomial distribution function 
 * Returns the probability that there will be @s or fewer failures 
 * before there are @xn successes, with each binomial trial having 
 * a probability of success @pr. 
 * prob(# failures = s | xn successes, pr)  = 
 *                        ( xn + s - 1 ) 
 *                        (            ) * pr^xn * (1-pr)^s 
 *                        (      s     ) 
 * 
 *     formula  26.5.26    of   abramowitz  and    stegun,  handbook   of 
 *     mathematical   functions (1966) is   used  to reduce the  negative 
 *     binomial distribution to the cumulative beta distribution. 
 * 
 * Returns: 0.
 **/

int cdf_cumnbn (double *s, double *xn, double *pr, double *ompr, double *cum,
		double *ccum)
{
  if ( isnan(*s) )
    {
      *cum = *ccum = *s;
    }
  else if ( *s < 0.0 )
    {
      *cum = 0.0; *ccum = 1.0;
    }
  else
    {
      double b = *s + 1.0;
      if ( b > DBL_MAX )  /* add this block because bratio doesn't handle Inf 
                             as its 2 th parameter (return Nan). Bruno april 2010 */
	{
	  *cum = 1.0; *ccum = 0.0;
	}
      else
	{
	  int ierr; /* FIXME: bratio can fails so... */
	  cdf_bratio (*xn, b, *pr, *ompr, cum, ccum, &ierr);
	}
    }
  return 0;
}

