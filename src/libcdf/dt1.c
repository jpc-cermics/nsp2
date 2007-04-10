#include "cdf.h"

/**
 * cdf_dt1:
 * @p: the p-value whose inverse from the t distribution is desired.
 * @q: 1-p.
 * @df: degrees of freedom of the t distribution.
 * 
 * Used to initalize approximation to inverse of the cumulative t distribution 
 * function. 
 * Returns  the  inverse   of  the t   distribution   function, i.e., 
 * the integral from 0 to invt of the t density is p. this is an 
 * initial approximation. 
 * 
 * Returns: a double 
 **/

double cdf_dt1 (double *p, double *q, double *df)
{
  const double coef[20] /* was [5][4] */  =
    { 1., 1., 0., 0., 0., 3., 16., 5., 0., 0., -15., 17., 19., 3., 0., -945.,
      -1920., 1482., 776., 79. };
  const  int ideg[4] = { 2, 3, 4, 5 };
  const double denom[4] = { 4., 96., 384., 92160. };
  double denpow, d1,  sum, term,  x, xp, xx;
  int i;

  x = (d1 = cdf_dinvnr (p, q), Abs (d1));
  xx = x * x;
  sum = x;
  denpow = 1.;
  for (i = 1; i <= 4; ++i)
    {
      term = cdf_devlpl (&coef[i * 5 - 5], ideg[i - 1], xx) * x;
      denpow *= *df;
      sum += term / (denpow * denom[i - 1]);
    }
  if (!(*p >= .5))
    {
      xp = -sum;
    }
  else
    {
      xp = sum;
    }
  return  xp;
}

