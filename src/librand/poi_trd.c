/* Nsp
 * Copyright (C) 2006-2007 Bruno Pincon Esial/Iecn
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

#include "grand.h"
#include <math.h>


#define LOG_SQRT_2PI 0.9189385332046727417803297363
static double logfact[10] = {1., 1., 2., 6., 24., 120., 720., 5040., 40320., 362880.};


/**
 * nsp_rand_poisson_init:
 * @mu: parameter of the Poisson distribution
 * @P: a pointer to an allocated #PoissonStruct
 * 
 * initialize the struct @P for random generation
 * with #nsp_rand_poisson
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_poisson_init(double mu, PoissonStruct *P)
{
  if ( mu < 0.0 ) 
    return FAIL;

  P->mu = mu;
  if ( mu <= SMALL_MEAN_POISSON )   /* setup INV algorithm with recording */
    {
      P->cumpr[0] = 0.0;
      P->K = 1; P->cumpr[1] = exp(-mu); P->taille = 3 + (int) mu;
    }
  else                              /* setup TRD algorithm */
    {
      P->smu = sqrt(mu);
      P->b = 0.931 + 2.53 * P->smu;
      P->a = -0.059 + 0.02483 * P->b;
      P->aa = 2.0 * P->a;
      P->c = P->mu + 0.445;
      P->invalpha = 1.1239 + 1.1328/(P->b - 3.4);
      P->vr = 0.9277 - 3.6224/(P->b - 2.0);
      P->invvr = 1.0 / P->vr;
      P->urvr = 0.86 * P->vr;
    }
  return OK;
}

/**
 * nsp_rand_poisson:
 * @P: a pointer to an initialized #PoissonStruct
 * 
 * generates a random number from the Poisson distribution.
 * @P must be initilized with #nsp_rand_poisson_init. This routine 
 * must be used when several P(mu) deviates with the same
 * fixed parameter mu are needed. Otherwise uses 
 * #nsp_rand_poisson_direct.
 *
 * method: for small parameter mu the code uses inv algorithm with
 * partial recording of the cumulative probabilities
 * otherwise it uses algorithm TRD of Wolfgang Hormann described in :
 * W. Hormann, "The transformed rejection method for generating 
 * Poisson random variables", Insurance: Mathematics and Economics 
 * 12, 39-45 (1993)
 * see http://statmath.wu-wien.ac.at/papers/92-04-13.wh.abs.html 
 * to download the preprint paper.
 *
 * Returns an int
 **/
int nsp_rand_poisson(PoissonStruct *P)
{
  int k;
  double f, pc, u, v, k_real, us;

  if ( P->mu <= SMALL_MEAN_POISSON )     /* INV algorithm with recording */
    {
      u = rand_ranf();
      k = P->K - 1;
      if ( u <= P->cumpr[P->K] )
	while ( u < P->cumpr[k] ) k--;
      else
	{
	  f = P->cumpr[P->K] - P->cumpr[k]; pc = P->cumpr[P->K];
	  do
	    {
	      k++;
	      f *= P->mu / k;
	      pc += f;
	      if ( k < P->taille )
		{
		  P->K++;
		  P->cumpr[P->K] = pc;
		}
	    }
	  while ( u > pc );
	}
      return k;
    }
  else                                   /* PTRD algorithm */
    {
      while ( 1 )
	{
	  v = rand_ranf();
	  if ( v <= P->urvr )
	    {
	      u = v * P->invvr - 0.43;
	      return (int) ((P->aa/(0.5 - fabs(u)) + P->b)*u + P->c);
	    }
	  
	  if ( v >= P->vr )
	    u = rand_ranf() - 0.5;
	  else
	    {
	      u = v * P->invvr - 0.93;
	      u = u >= 0 ? 0.5 - u : -0.5 - u;
	      v = P->vr*rand_ranf();
	    }
	  us = 0.5 - fabs(u);

	  if ( (us >= 0.013  ||  v <= us)  && (k_real = (P->aa/us + P->b)*u + P->c) >= 0 )
	    {
	      k = (int) k_real;
	      v = v*P->invalpha/(P->a/(us*us) + P->b);

	      if ( k >= 10 )
		{
		  if ( log(v*P->smu) <= (k+0.5)*log(P->mu/k) - P->mu - LOG_SQRT_2PI
		       + k - (1./12. - 1./(360.*k*k))/k )
		    return k;
		}
	      else
		{
		  if  ( log(v) <= k*log(P->mu) - P->mu - logfact[k] )
		    return k;
		}
	    }
	}
    }
}


/**
 * nsp_rand_poisson_direct:
 * @mu: parameter of the Poisson distribution
 * 
 * generates a random number from the Poisson distribution.
 * When several P(mu) random deviates with the same fixed parameter 
 * mu are needed, it is faster to use #nsp_rand_poisson.
 *
 * method: for small parameter mu the code uses inv algorithm
 * otherwise it uses algorithm TRD of Wolfgang Hormann described in :
 * W. Hormann, "The transformed rejection method for generating 
 * Poisson random variables", Insurance: Mathematics and Economics 
 * 12, 39-45 (1993)
 * see http://statmath.wu-wien.ac.at/papers/92-04-13.wh.abs.html 
 * to download the preprint paper.
 *
 * Returns an int
 **/
int nsp_rand_poisson_direct(double mu)
{
  int k;
  double s, p, u, v, us, smu, a, b, invalpha, vr, k_real;

  if ( mu <= SMALL_MEAN_POISSON_DIRECT_ALGO ) /* INV algorithm */
    {
      u = rand_ranf();
      k = 0;
      p = exp(-mu);
      s = p;
      while ( u > s )
	{
	  k++;
	  p = mu*p / k;
	  s += p;
	}
      return k;
    }
   else                           /* PTRD algorithm */
    {
      smu = sqrt(mu);
      b = 0.931 + 2.53*smu;
      a = -0.059 + 0.02483*b;
      invalpha = 1.1239 + 1.1328/(b-3.4);
      vr = 0.9277 - 3.6224/(b-2.0);

      while ( 1 )
	{
	  v = rand_ranf();
	  if ( v <= 0.86*vr )
	    {
	      u = v/vr - 0.43;
	      return (int) ((2.0*a/(0.5 - fabs(u)) + b)*u + mu + 0.445);
	    }
	  
	  if ( v >= vr )
	    u = rand_ranf() - 0.5;
	  else
	    {
	      u = v/vr - 0.93;
	      u = u >= 0 ? 0.5 - u : -0.5 - u;
	      v = vr*rand_ranf();
	    }
	  us = 0.5 - fabs(u);

	  if ( (us >= 0.013 || v <= us) && (k_real = (2.0*a/us + b)*u + mu + 0.445) >= 0 )
	    {
	      k = (int) k_real;
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
