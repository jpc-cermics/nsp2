#include "cdf.h"

/**
 * cdf_cumt:
 * @t: upper limit of integration of the t-density.
 * @df: degrees of freedom of the t-distribution.
 * @cum: cumulative t-distribution.
 * @ccum:  compliment of cumulative t-distribution.
 * 
 * computes the integral from -infinity to t of the t-density.
 * 
 * formula 26.5.27   of     abramowitz  and   stegun,    handbook  of 
 * mathematical functions  is   used   to  reduce the  t-distribution 
 * to an incomplete beta. 
 * 
 * Returns: 
 **/

int cdf_cumt (double *t, double *df, double *cum, double *ccum)
{
  double a, dfptt, tt,  c_b2 = .5,  d__1, xx, yy, oma;
  tt = *t * *t;
  dfptt = *df + tt;
  xx = *df / dfptt;
  yy = tt / dfptt;
  d__1 = *df * .5;
  cdf_cumbet (&xx, &yy, &d__1, &c_b2, &a, &oma);
  if (!(*t <= 0.))
    {
      *ccum = a * .5;
      *cum = oma + *ccum;
    }
  else 
    {
      *cum = a * .5;
      *ccum = oma + *cum;
    }
  return 0;
}

