#include "cdf.h"

/* ********************************************************************** */
/*     DOUBLE PRECISION FUNCTION DSTREM( Z ) */
/*             Double precision Sterling Remainder */
/*                              Function */
/*     Returns   Log(Gamma(Z))  -  Sterling(Z)  where   Sterling(Z)  is */
/*     Sterling's Approximation to Log(Gamma(Z)) */
/*     Sterling(Z) = LOG( SQRT( 2*PI ) ) + ( Z-0.5 ) * LOG( Z ) - Z */
/*                              Arguments */
/*     Z --> Value at which Sterling remainder calculated */
/*           Must be positive. */
/*                  DOUBLE PRECISION Z */
/*                              Method */
/*     If Z >= 6 uses 9 terms of series in Bernoulli numbers */
/*     (Values calculated using Maple) */
/*     Otherwise computes difference explicitly */
/* ********************************************************************** */


double cdf_dstrem (double *z__)
{
  const int c__10 = 10;

  const double hln2pi=0.91893853320467274178E0;
  const double coef[10] =
    { 0., .0833333333333333333333333333333,
      -.00277777777777777777777777777778, 7.93650793650793650793650793651e-4,
      -5.95238095238095238095238095238e-4, 8.41750841750841750841750841751e-4,
      -.00191752691752691752691752691753, .00641025641025641025641025641026,
      -.0295506535947712418300653594771, .179644372368830573164938490016 };
  double ret_val, d__1, d__2;
  double sterl;
  double status;

/*      CONVERTPARAMETER( (hln2pi=0.91893853320467274178E0) */
/*      CONVERTPARAMETER( (ncoef=10) */
/*    For information, here are the next 11 coefficients of the */
/*    remainder term in Sterling's formula */
/*            -1.39243221690590111642743221691 */
/*            13.4028640441683919944789510007 */
/*            -156.848284626002017306365132452 */
/*            2193.10333333333333333333333333 */
/*            -36108.7712537249893571732652192 */
/*            691472.268851313067108395250776 */
/*            -0.152382215394074161922833649589D8 */
/*            0.382900751391414141414141414141D9 */
/*            -0.108822660357843910890151491655D11 */
/*            0.347320283765002252252252252252D12 */
/*            -0.123696021422692744542517103493D14 */
  if (*z__ <= 0.)
    {
      Scierror("Zero or negative argument in DSTREM\n");
      status = -100.;
      ret_val = 0.;
      return ret_val;
    }
  if (!(*z__ > 6.))
    {
      goto L10;
    }
  /* Computing 2nd power */
  d__2 = *z__;
  d__1 = 1. / (d__2 * d__2);
  ret_val = cdf_devlpl (coef, &c__10, &d__1) * *z__;
  goto L20;
L10:
  sterl = hln2pi + (*z__ - .5) * log (*z__) - *z__;
  ret_val = cdf_dlngam (z__) - sterl;
L20:
  return ret_val;
} 
