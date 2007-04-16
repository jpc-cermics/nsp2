/* Nsp
 * Copyright (C) 2006 Bruno Pincon Esial/Iecn
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

/*   
 *   algorithm PTRD of Wolfgang Hormann, for Poisson distribution
 */


#include "grand.h"
#include <math.h>

#define SMALL_MEAN 25
#define LOG_SQRT_2PI 0.9189385332046727417803297363
static double cumpr[SMALL_MEAN+4] = {0.0};
static double logfact[10] = {1., 1., 2., 6., 24., 120., 720., 5040., 40320., 362880.};

int poi_trd(double mu)
{
  int k;
  double f, pc, u, v, k_real, us;
  static double mu_save = -1.0, smu, a, aa, b, c, invalpha, vr, urvr, invvr;
  static int taille, K;

  if ( mu <= SMALL_MEAN )     /* INV algorithm with recording */
    {
      if ( mu != mu_save )    /* setup */
	{
	  K = 1; cumpr[1] = exp(-mu); taille = 3 + (int) mu;
	  mu_save = mu;
	}

      u = rand_ranf();
      k = K-1;
      if ( u <= cumpr[K] )
	while ( u < cumpr[k] ) k--;
      else
	{
	  f = cumpr[K] - cumpr[k]; pc = cumpr[K];
	  do
	    {
	      k++;
	      f *= mu/k;
	      pc += f;
	      if ( k <= taille )
		cumpr[++K] = pc;
	    }
	  while ( u > pc );
	}

      return k;
    }
 
  else                          /* PTRD algorithm */
    {

      if ( mu != mu_save )   /* setup */
	{
	  smu = sqrt(mu);
	  b = 0.931 + 2.53*smu;
	  a = -0.059 + 0.02483*b;
	  aa = 2.0*a;
	  c = mu + 0.445;
	  invalpha = 1.1239 + 1.1328/(b-3.4);
	  vr = 0.9277 - 3.6224/(b-2.0);
	  invvr = 1.0/vr;
	  urvr = 0.86*vr;
	  mu_save = mu;
	}

      while ( 1 )
	{
	  v = rand_ranf();
	  if ( v <= urvr )
	    {
	      u = v * invvr - 0.43;
	      return (int) ((aa/(0.5 - fabs(u)) + b)*u + c);
	    }
	  
	  if ( v >= vr )
	    u = rand_ranf() - 0.5;
	  else
	    {
	      u = v * invvr - 0.93;
	      u = u >= 0 ? 0.5 - u : -0.5 - u;
	      v = vr*rand_ranf();
	    }
	  us = 0.5 - fabs(u);

	  if ( (k_real = (aa/us + b)*u + c) >= 0.0 )
	    {
	      k = k_real;
	      v = v*invalpha/(a/(us*us) + b);

	      if ( k >= 10 )
		{
		  if ( log(v*smu) <= (k+0.5)*log(mu/k) - mu - LOG_SQRT_2PI
		       + k - (1./12. - 1./(360.*k*k))/k )
		    return k;
		}
	      else
		{
		  if  ( log(v) <= k*log(mu) - mu - logfact[k] )
		    return k;
		}
	    }
	}
    }
}

