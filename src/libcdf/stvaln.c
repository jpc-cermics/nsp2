#include "cdf.h"

/*
 * 
 *  gives a starting value for newton-raphson 
 *  calculation of normal distribution inverse. 
 *  Returns x  such that cumnor(x)=p, i.e., the  integral from 
 *  - infinity to x of (1/sqrt(2*pi)) exp(-u*u/2) du is p 
 *  p  the probability whose normal deviate is sought. 
 *    
 *  The  rational   function   on  page 95    of kennedy  and  gentle, 
 *     statistical computing, marcel dekker, ny , 1980. 
 */

double cdf_stvaln (double *p)
{
  const int c__5 = 5;
  const double xnum[5] = { -.322232431088, -1., -.342242088547, -.0204231210245, -4.53642210148e-5 };
  const double xden[5] = { .099348462606, .588581570495, .531103462366, .10353775285, .0038560700634 };
  double ret_val;
  double sign, y, z;
  if (!(*p <= .5))
    {
      sign = 1.;
      z = 1. - *p;
    }
  else 
    {
      sign = -1.;
      z = *p;
    }
  y = sqrt (log (z) * -2.);
  ret_val = y + cdf_devlpl (xnum, c__5, y) / cdf_devlpl (xden, c__5, y);
  ret_val = sign * ret_val;
  return ret_val;
}
