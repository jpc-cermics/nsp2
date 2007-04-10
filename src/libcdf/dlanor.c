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
 *
 *
 *  log(1-erf(abs(x)/sqrt(2))) - log(2)
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
  correc = cdf_devlpl(coef,ncoeff,invxx2)/xx2;
  correc = cdf_dln1px(correc);
  return  approx + correc;
}

/* GPL version; Copyright Jean-Philippe Chancelier 
 *
 * Computes the logarithm of the cumulative normal distribution
 * from abs(x) to infinity for abs(x) > 4.
 * It is also equal to  log(erfc(abs(x)/sqrt(2))) - log(2) but 
 * this formula numerically gives -Inf for x >= 38. 
 * 
 * Using  Journal of Inequalities in Pure and Applied  Mathematics 
 * Volume 3,  Issue 2, 2002, Article 20
 * monotonicity properties of the relative error of a Padé 
 * approximation for Mills' ratio
 * Iosif Pinelis (ipinelis@mtu.edu)
 *
 * The mills' ratio r(t)= int_t^\infty phi(u)du / phi(t)
 * where phi(t)= 1/sqrt(2*pi) exp(-t^2/2)
 * can be approximated by r8(t):
 * r8(t) = t(t^6 + 27*t^4 + 185* t^2 + 279)/
 *          (t^8 + 28*t^6+ 210*t^4 + 420*t^2 + 105)
 * the absolute relative error is less than 0.5 e-6 for all t > 4.
 * 
 * 
 *  x=0:0.1:30;y1=log(erfc(abs(x)/sqrt(2)))-log(2);
 *  y=cdf_dlanor1(x);
 *  plot(x,y-y1)
 */

double cdf_dlanor1(double x)
{
  /* log(sqrt(2*%pi)) */
  const double dlsqpi =0.91893853320467274177E0;
  double x2,mratio;
  x = Abs(x); 
  if (x <= 4 )  
    {
      x2= x/sqrt(2);
      /* Scierror("Argument too small in dlanor\n"); */
      return log(cdf_erfc(0,x2)/2);
    }
  x2 = x*x; 
  mratio = x*(279 + x2*( 185 + x2*(27 + x2)))
    / (105 + x2*(420+ x2*(210 + x2*(28+x2))));
  return  -dlsqpi - 0.5*x2 + log(mratio);
}


