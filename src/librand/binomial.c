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

#include "grand.h"
#include <math.h>

#define FAR_FROM_MODE_LIMIT 20

#define PNBINOM(k,n,r) ((k+0.5)*log((k+1)/(r*(n+1-k)))+stirling_error(k)+stirling_error(n-k))


#define C1 0.08333333333333333333333333333  /* 1/12 */
#define C2 0.03333333333333333333333333333  /* 1/30 */
#define C3 0.2523809523809523809523809523   /* 53/210 */ 
#define C4 0.5256064690026954177897574123   /* 195/371 */
#define C5 1.011523068126841711747372124    /* 22999/22737 */ 
#define ONE_THIRD 0.3333333333333333333333333333 /* 1/3 */
#define ONE_SIXTH 0.1666666666666666666666666666 /* 1/6 */

/*                                                                  
 *  error e(k) = ln(k!) - {0.5*log(2*Pi) + (k+0.5)*log(k+1) - (k+1)} 
 *  approximated using Stieltjes formula                             
 *  see http://www.luschny.de/math/factorial/approx/SimpleCases.html
 *
 *  the different cut levels (114,32,15,10) have been computed with pari-gp
 *  to get 1e-16 relative accuracy (in exact arithmetic)
 */
static double stirling_error(int k)
{
  static const double stir_er[11] = {
    0.08106146679532725821967026361,
    0.04134069595540929409382208142,
    0.02767792568499833914878929276,
    0.02079067210376509311152277177,
    0.01664469118982119216319486537,
    0.01387612882307074799874572700,
    0.01189670994589177009505572413,
    0.01041126526197209649747856695,
    0.009255462182712732917728636556,
    0.008330563433362871256469318658,
    0.007573675487951840794972024033};

  double N;
  if ( k <= 10 )
    return stir_er[k];

  N = (double) k+1;

  if ( k > 114 )
    return C1/(N + C2/N);
  if ( k > 32 )
    return C1/(N+ C2/(N + C3/N));
  if ( k > 15 )
    return C1/(N+ C2/(N + C3/( N + C4/N )));
  else
    return C1/(N+ C2/(N + C3/( N + C4/( N + C5/N ))));
}

/**
 * nsp_rand_binomial_init:
 * @n: first parameter of the binomial distribution (number of Bernoulli trials)
 * @p: second parameter of the binomial distribution (probability of success of a Bernoulli event)
 * @B: a pointer to an allocated #BinomialStruct
 * 
 * initialize the struct @B for random generation
 * with #nsp_rand_binomial
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_binomial_init(int n, double p, BinomialStruct *B)
{
  double q;

  if ( ! ( n > 0 && 0.0 <= p && p <= 1.0 ) )
    return FAIL;

  B->n = n;
  B->flipped = p > 0.5;

  if ( B->flipped ) 
    { q = p; B->p = 1 - p;}
  else 
    { q = 1 - p; B->p = p;}

  B->r = B->p / q;

  if ( n*B->p <= SMALL_MEAN_BINOMIAL )
    {
      B->K = 1; 
      B->cumpr[0] = 0.0;
      B->cumpr[1] = pow(q, n); 
      B->taille = 2 + (int) n*B->p;
    }
  else
    {
      B->m = (int) (n+1)*B->p;
      B->npq = n*B->p*q;
      B->b = 1.15 + 2.53*sqrt(B->npq);
      B->a = -0.0873 + 0.0248*B->b + 0.01*B->p; 
      B->aa = 2.0*B->a;
      B->c = n*B->p + 0.5;
      B->alpha = (2.83 + 5.1/B->b)*sqrt(B->npq);
      B->vr = 0.92 - 4.2/B->b; 
      B->invvr = 1.0/B->vr;
      B->urvr = 0.86*B->vr;
      B->h = PNBINOM(B->m,n,B->r);
    }  

  return OK;
}

/**
 * nsp_rand_binomial:
 * @B: a pointer to an initialized #BinomialStruct
 * 
 * generates a random number from the binomial distribution.
 * @B must be initilized with nsp_#nsp_rand_binomial_init. This routine 
 * must be used when several B(n,p) deviates with the same
 * fixed parameters n and p are needed. Otherwise uses 
 * #nsp_rand_binomial_direct.
 *
 * method: for small mean n*p the code uses inv algorithm with
 * partial recording of the cumulative probabilities
 * otherwise it uses algorithm TRD of Wolfgang Hormann described in :
 * Wolfgang Hormann, "The generation of Binomial Random Variates", 
 * J. Statist. Comput. Simul., Vol 46, pp. 101-110.
 * see http://statmath.wu-wien.ac.at/papers/92-04-07.wh.ps
 * to download the preprint paper.
 *
 * Returns an int
 **/
int nsp_rand_binomial(BinomialStruct *B)
{
  int k, n = B->n;
  double u, p = B->p, r = B->r, f;

  if ( n*p <= SMALL_MEAN_BINOMIAL )     /* BINV algorithm with recording */
    {
      double pc;
      u = rand_ranf();
      k = B->K-1;
      if ( u <= B->cumpr[B->K] )
	while ( u < B->cumpr[k] ) k--;
      else
	{
	  f = B->cumpr[B->K] - B->cumpr[k]; pc = B->cumpr[B->K];
	  do
	    {
	      k++;
	      if ( k == B->n ) break;  /* security in case of floating point errors */
	      f *= (r*(n+1-k))/k;
	      pc += f;
	      if ( k < B->taille )
		B->cumpr[++B->K] = pc;
	    }
	  while ( u > pc );
	}
    }
  else                          /* BTRD algorithm */
    {
      double v, us, k_real;
      int km, m = B->m;
      while ( 1 )
	{
	  v = rand_ranf();
	  if ( v <= B->urvr )
	    {
	      u = v * B->invvr - 0.43;
	      k = ((B->aa/(0.5 - fabs(u)) + B->b)*u + B->c);
	      break;
	    }
	  
	  if ( v >= B->vr )
	    u = rand_ranf() - 0.5;
	  else
	    {
	      u = v * B->invvr - 0.93;
	      u = u >= 0 ? 0.5 - u : -0.5 - u;
	      v = B->vr*rand_ranf();
	    }
	  us = 0.5 - fabs(u);

	  if ( (k_real = (B->aa/us + B->b)*u + B->c) >= 0 && (k = (int) k_real) <= n ) 
	    {
	      v = v*B->alpha/(B->a/(us*us) + B->b);
	      km = abs(k - m);

	      /* now we must know if  v <= f(k)/f(m) (k accepted) or not (k rejected)  */

	      if ( km <= FAR_FROM_MODE_LIMIT )  /*  recursive evaluation of f(k)        */
		{                               /*  (for k not too far from the mode m) */
		  int i;
		  f = 1.0;
		  if ( m < k )
		    for ( i = m+1 ; i <= k ; i++ ) f *= (r*(n+1-i))/i;
		  else if ( m > k )
		    for ( i = k+1 ; i <= m ; i++ ) v *= (r*(n+1-i))/i;
		  if ( v <= f )
		    break;
		}
	      else   /* in this case the test is in the form log(v) <= log(f(k)) - log(f(m)) */
		{
		  double kmr = fabs(-0.5 + (double)(k-m));
		  v = log(v);
		  if ( kmr < 0.5*B->npq - 1.0) /* try Kachitvichyanukul and Schmeiser squeeze */
                                               /* (based on Laplace-DeMoivre) */
		    {                          /* to speed up acceptation or rejection */
		      double rho, t, vv = v*(B->npq/kmr);
		      rho = ((ONE_THIRD*kmr + 0.625)*kmr + ONE_SIXTH)/B->npq + 0.5;
		      t = -0.5*kmr;
		      if ( vv < t - rho )
			break;
		      else if ( vv > t + rho )
			continue;
		    }

		  /* the previous squeeze couldn't be applied or have not concluded  */
                  /* so we really test if  log(v) <= log(f(k)) - log(f(m))           */
		  if ( v <= B->h + (n+1)*log((n+1-m)/((double) (n+1-k))) -  PNBINOM(k,n,r) )
		    break;
		}
	    }
	}
    }

  if ( B->flipped )
    return n - k;
  else
    return k;
}

/**
 * nsp_rand_binomial_direct:
 * @n: first parameter of the binomial distribution
 * @p: second parameter of the binomial distribution
 * 
 * generates a random number from the binomial distribution.
 * When several B(n,p) random deviates with the same fixed parameters 
 * n and p are needed, it is faster to use #nsp_rand_binomial.
 *
 * method: for small mean n*p the code uses inv algorithm
 * otherwise it uses algorithm TRD of Wolfgang Hormann described in :
 * Wolfgang Hormann, "The generation of Binomial Random Variates", 
 * J. Statist. Comput. Simul., Vol 46, pp. 101-110.
 * see http://statmath.wu-wien.ac.at/papers/92-04-07.wh.ps
 * to download the preprint paper.
 *
 * Returns an int
 **/
int nsp_rand_binomial_direct(int n, double p)
{
  int flipped = p > 0.5;
  int k;
  double q, r;

  if ( flipped ) 
    { q = p; p = 1 - p; }
  else 
    { q = 1 - p;}

  if ( n*p <= SMALL_MEAN_BINOMIAL_DIRECT_ALGO )     /* BINV algorithm */
    {
      double f, pc, u;
      r = p/q;
      f = pow(q, n);
      pc = f;
      u = rand_ranf();
      k = 0;
      while ( u > pc )
	{
	  if ( k == n ) break;  /* security in case of floating point errors */
	  k++;
	  f *= (r*(n+1-k))/k;
	  pc += f;
	}
    }
 
  else                          /* BTRD algorithm */
    {
      int m, km;
      double npq, b, a, c, alpha, vr, u, v, us, k_real;

      /* setup */
      m = (int) (n+1)*p;
      r = p/q;
      npq = n*p*q;
      b = 1.15 + 2.53*sqrt(npq);
      a = -0.0873 + 0.0248*b + 0.01*p;
      c = n*p + 0.5;
      alpha = (2.83 + 5.1/b)*sqrt(npq);
      vr = 0.92 - 4.2/b;

      while ( 1 )
	{
	  v = rand_ranf();
	  if ( v <=  0.86*vr )
	    {
	      u = v/vr - 0.43;
	      k = ((2*a/(0.5 - fabs(u)) + b)*u + c);
	      break;
	    }
	  
	  if ( v >= vr )
	    u = rand_ranf() - 0.5;
	  else
	    {
	      u = v / vr - 0.93;
	      u = u >= 0 ? 0.5 - u : -0.5 - u;
	      v = vr*rand_ranf();
	    }
	  us = 0.5 - fabs(u);

	  if ( (k_real = (2.0*a/us + b)*u + c) >= 0.0 && (k = (int) k_real) <= n ) 
	    {
	      v = v*alpha/(a/(us*us) + b);
	      km = abs(k - m);

	      /* now we must know if  v <= f(k)/f(m) (k accepted) or not (k rejected)  */

	      if ( km <= FAR_FROM_MODE_LIMIT )  /*  recursive evaluation of f(k)        */
		{                               /*  (for k not too far from the mode m) */
		  int i;
		  double f = 1.0;
		  if ( m < k )
		    for ( i = m+1 ; i <= k ; i++ ) f *= (r*(n+1-i))/i;
		  else if ( m > k )
		    for ( i = k+1 ; i <= m ; i++ ) v *= (r*(n+1-i))/i;
		  if ( v <= f )
		    break;
		}
	      else   /* in this case the test is in the form log(v) <= log(f(k)) - log(f(m)) */
		{
		  double kmr = fabs(-0.5 + (double)(k-m));
		  v = log(v);
		  if ( kmr < 0.5*npq - 1.0)   /* try Kachitvichyanukul and Schmeiser squeeze */
                                              /* (based on Laplace-DeMoivre) */
		    {                         /* to speed up acceptation or rejection */
		      double rho, t, vv = v*(npq/kmr);
		      rho = ((ONE_THIRD*kmr + 0.625)*kmr + ONE_SIXTH)/npq + 0.5;
		      t = -0.5*kmr;
		      if ( vv < t - rho )
			break;
		      else if ( vv > t + rho )
			continue;
		    }

		  /* the previous squeeze couldn't be applied or have not concluded  */
                  /* so we really test if  log(v) <= log(f(k)) - log(f(m))           */
		  if ( v <= PNBINOM(m,n,r) + (n+1)*log((n+1-m)/((double) (n+1-k))) -  PNBINOM(k,n,r) )
		    break;
		}
	    }
	}
    }

  if ( flipped )
    return n - k;
  else
    return k;
}
