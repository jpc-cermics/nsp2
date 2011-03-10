/* Nsp
 * Copyright (C) 2011 Peter John Acklam, Bruno Pincon
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
 */

#include "cdf.h"

/**
 * nsp_cdf_dinvnr:
 * @p: The probability whose normal deviate is sought. 
 * @q: 1-p
 * 
 * p and q should verify p+q == 1 (in floating point)
 *
 * returns x  such that cumnor(x) = p,  i.e., the  integral from 
 * - infinity to x of (1/sqrt(2*pi)) exp(-u*u/2) du is p 
 * 
 *     use algorithm from Peter John Acklam 
 *     (http://home.online.no/~pjacklam/notes/invnorm/index.html)
 *     with a slight modification for the correction 
 * 
 * Returns: a double 
 **/

double nsp_cdf_dinvnr(double p, double q)
{
  double pp, x, r, rr, e, u;
  double const sqrt2pi = 2.506628274631000502415765285, invsqrt2 = 0.7071067811865475244008443621,
    sqrtpiover2 = 1.253314137315500251207882642; 
  int  porq;

  /* particular cases */
  if ( p == 0.0 )
    return -2.0*DBL_MAX;  /* -Inf */
  else if ( q == 0.0 ) 
    return  2.0*DBL_MAX;  /* Inf */
  else if ( isnan(p) || isnan(q) )
    return p+q;

  /*  use the minimum of p and q */
  porq = p <= q;
  pp = (porq) ? p : q;  /* so pp is in (0,0.5] */

  /* first approximation (relative accuracy of 1.15e-9) */
  if ( pp <= 0.02425 )
    {
      /* rational approximation for lower region */
      r = sqrt(-2.0*log(pp));
      x = (2.938163982698783 + r*(4.374664141464968 + r*(-2.549732539343734 
           + r*(-2.400758277161838 + r*(-0.3223964580411365 - r*7.784894002430293e-03)))))
	/ (1.0 + r*(3.754408661907416 + r*(2.445134137142996 + r*(0.3224671290700398 + r*7.784695709041462e-03))));
    }
  else
    {
      /* rational approximation for center region */
      r = pp - 0.5;
      rr = r*r;
      x = (r*(2.506628277459239+rr*(-30.66479806614716 + rr*(138.3577518672690 + rr*(-275.9285104469687
	    + rr*(220.9460984245205 - rr*39.69683028665376))))))
	/ (1.0 + rr*(-13.28068155288572 + rr*(66.80131188771972 + rr*(-155.6989798598866
	     + rr*(161.5858368580409 - rr*54.47609879822406)))));
    }

  /* correction (1 iteration of Halley 's method) */
  if ( x <= -0.9442 )
    {
      double P, Q;
      cdf_cumnor (&x, &P, &Q);
      e = P - pp;
      u = e*sqrt2pi*exp(0.5*x*x);
      x -= u/(1.0 + 0.5*x*u);
    }
  else
    {
      e = erf(invsqrt2*x) - 2*(pp-0.5);
      u = e*sqrtpiover2*exp(0.5*x*x);
      x -= u/(1.0 + 0.5*x*u);
    }

  return porq ? x : -x;
}	
