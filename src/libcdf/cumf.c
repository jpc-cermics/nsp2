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
  if ( *f > 0.0 )
    {
      /*     XX is such that the incomplete beta with parameters 
       *     a=DFD/2 and b=DFN/2 evaluated at XX is 1 - CUM or CCUM 
       *     YY is 1 - XX 
       *     Calculate the smaller of XX and YY accurately 
       */
      double prod, sum, xx, yy, a = 0.5 * (*dfd), b = 0.5 * (*dfn);
      int ierr; 
      prod = *dfn * *f;
      sum = *dfd + prod;
      xx = *dfd / sum;
      if ( xx > 0.5 )
	{
	  yy = prod / sum;
	  xx = 1.0 - yy;
	}
      else
	yy = 1.0 - xx;

      cdf_bratio (a, b, xx, yy, ccum, cum, &ierr);
    }
  else if (*f <= 0.0) 
    {
      *cum = 0.0; *ccum = 1.0;
    }
  else  /* *f is Nan (it is not well handled in the main first bloc)  */
    {
      *cum = *f; *ccum = *f;
    }
  
  return 0;
}	

