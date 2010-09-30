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
      *cum = 0.0; *ccum = 1.0;
    }
  else if ( *x <=  DBL_MAX )  /* main block (could not handle x=Inf and also */
    {                         /* could not handle x=Nan in some rare cases) */
      const int c0 = 0;
      cdf_gratio (a, x, cum, ccum, &c0);
    }

  else if (isnan(*x))   /* x = Nan (bruno, sept 2010) */
    {                
      *cum = *ccum = *x;
    }
  else                    /* x = Inf  (bruno, april 2010) */
    {
      *cum = 1.0; *ccum = 0.0;
    }

  return 0;
} 

