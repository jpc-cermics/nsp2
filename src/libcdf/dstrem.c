#include "cdf.h"



/**
 * cdf_dstrem:
 * @z: a positive double, value at which sterling remainder is calculated 
 *
 * Sterling remainder function, i.e returns  
 * log(gamma(z)) - sterling(z) where sterling(z) is 
 * sterling's approximation to log(gamma(z)) 
 * sterling(z) = log( sqrt( 2*pi ) ) + ( z-0.5 ) * log( z ) - z 
 * 
 * if z >= 6 uses 9 terms of series in bernoulli numbers 
 * (values calculated using maple) 
 * otherwise computes difference explicitly 
 * 
 * Returns: a double 
 **/

double cdf_dstrem (double z)
{
  const int c10 = 10;
  const double hln2pi=0.91893853320467274178E0; /* log(sqrt(2*pi)) */
  const double coef[10] = { 0., .0833333333333333333333333333333,
			    -.00277777777777777777777777777778, 7.93650793650793650793650793651e-4,
			    -5.95238095238095238095238095238e-4, 8.41750841750841750841750841751e-4,
			    -.00191752691752691752691752691753, .00641025641025641025641025641026,
			    -.0295506535947712418300653594771, .179644372368830573164938490016 };
  double   sterl, status;
  /*
   *    For information, here are the next 11 coefficients of the 
   *    remainder term in Sterling's formula 
   *            -1.39243221690590111642743221691 
   *            13.4028640441683919944789510007 
   *            -156.848284626002017306365132452 
   *            2193.10333333333333333333333333 
   *            -36108.7712537249893571732652192 
   *            691472.268851313067108395250776 
   *            -0.152382215394074161922833649589D8 
   *            0.382900751391414141414141414141D9 
   *            -0.108822660357843910890151491655D11 
   *            0.347320283765002252252252252252D12 
   *            -0.123696021422692744542517103493D14 
   */
  if (z <= 0.)
    {
      Scierror("Zero or negative argument in DSTREM\n");
      status = -100.;
      return 0;
    }
  if (!(z > 6.))
    {
      sterl = hln2pi + (z - 0.5) * log (z) - z;
      return cdf_dlngam (z) - sterl;
    }
  return cdf_devlpl (coef, c10,1./(z * z)) * z;
} 
