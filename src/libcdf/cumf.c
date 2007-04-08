#include "cdf.h"

/**
 * cdf_cumf:
 * @f: upper limit of integration of the F-density. 
 * @dfn: degrees of freedom of the numerator sum of squares. 
 * @dfd: degrees of freedom of the denominator sum of squares. 
 * @cum: cumulative f distribution. 
 * @ccum: compliment of cumulative f distribution. 
 * 
 *   cumulative F distribution function. computes the integral from 
 *   0  to  @f of  the F-density  with @dfn and @dfd degrees of freedom. 
 *   Formula  26.5.28 of  abramowitz and   stegun   is  used to  reduce 
 *   the cumulative f to a cumulative beta distribution. 
 *   If @f is less than or equal to 0, 0 is returned. 
 * 
 * Returns: @
 **/

int cdf_cumf (double *f, double *dfn, double *dfd, double *cum, double *ccum)
{
  const double half=0.5E0, done=1.0E0;
  double d__1, d__2, prod, dsum, xx, yy;
  int ierr;
  
  if (*f <= 0.) 
    {
      *cum = 0.;
      *ccum = 1.;
      return 0;
    }

  prod = *dfn * *f;
  /*     XX is such that the incomplete beta with parameters 
   *     DFD/2 and DFN/2 evaluated at XX is 1 - CUM or CCUM 
   *     YY is 1 - XX 
   *     Calculate the smaller of XX and YY accurately 
   */
  dsum = *dfd + prod;
  xx = *dfd / dsum;
  if (xx > half)
    {
      yy = prod / dsum;
      xx = done - yy;
    }
  else
    {
      yy = done - xx;
    }
  d__1 = *dfd * half;
  d__2 = *dfn * half;
  cdf_bratio (&d__1, &d__2, &xx, &yy, ccum, cum, &ierr);
  return 0;
}	

