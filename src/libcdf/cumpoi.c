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
 * Returns: 
 **/

int cdf_cumpoi (double *s, double *xlam, double *cum, double *ccum)
{
  double df;
  double chi;
  df = (*s + 1.) * 2.;
  chi = *xlam * 2.;
  cdf_cumchi (&chi, &df, ccum, cum);
  return 0;
}


