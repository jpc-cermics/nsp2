#include <nsp/machine.h>
#include <nsp/math.h>
#include "cdf.h"

/*
 *     DOUBLE PRECISION FUNCTION DLANOR( X )
 *          Double precision Logarith of the Asymptotic Normal
 *                             Function
 *     Computes the logarithm of the cumulative normal distribution
 *     from abs( x ) to infinity for abs( x ) >= 5.
 *                             Arguments
 *     X --> Value at which cumulative normal to be evaluated
 *                             Method
 *     23 term expansion of formula 26.2.12 of Abramowitz and Stegun.
 *     The relative error at X = 5 is about 0.5E-5.
 *                             Note
 *     ABS(X) must be >= 5 else there is an error stop.
 */

double cdf_dlanor(double x)
{
  const double coef[12] = 
    {
      -1.0E0,3.0E0,-15.0E0,105.0E0,-945.0E0,10395.0E0,
      -135135.0E0,2027025.0E0,-34459425.0E0,654729075.0E0,
      -13749310575E0,316234143225.0E0
    };
  const int ncoeff=12;
  const double dlsqpi =0.91893853320467274177E0;
  double approx,correc,xx,xx2,invxx2;
  xx = Abs(x);
  if (xx < 5.0E0) 
    {
      Scierror("Argument too small in dlanor\n");
      /* status = -100; */
      return 0.0;
    }
  xx2 = xx*xx;
  approx = -dlsqpi - 0.5E0*xx2 - log(xx);
  invxx2= 1.0E0/xx2;
  correc = cdf_devlpl(coef,&ncoeff,&invxx2)/xx2;
  correc = cdf_dln1px(&correc);
  return  approx + correc;
}
