/* Nsp
 * Copyright (C) 2007-2009 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Approximation of log(1-erf(abs(x)/sqrt(2))) - log(2)
 *
 */

#include <nsp/machine.h>
#include <nsp/math.h>
#include "cdf.h"

/**
 * cdf_dlanor:
 * @x: a double 
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
 * Returns: a double 
 **/

double cdf_dlanor(double x)
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



/**
 * cdf_dlanor_old:
 * @x: a double 
 * 
 *     Computes the logarithm of the cumulative normal distribution
 *     from abs( x ) to infinity for abs( x ) >= 5.
 *           log(1-erf(abs(x)/sqrt(2))) - log(2)
 *     23 term expansion of formula 26.2.12 of Abramowitz and Stegun.
 *     The relative error at X = 5 is about 0.5E-5.
 *     ABS(X) must be >= 5 else there is an error stop.
 *
 * Returns: a double 
 **/

double cdf_dlanor_old(double x)
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
