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
 *   algorithm BTRD of Wolfgang Hormann, described in "The generation 
 *   of Binomial Random Variates", J. Statist. Comput. Simul., Vol 46, 
 *   pp. 101-110.
 */


#include "grand.h"
#include <math.h>

#define FAR_FROM_MODE_LIMIT 20
#define SMALL_MEAN 15

#define PNBINOM(k,n,r) ((k+0.5)*log((k+1)/(r*(n+1-k)))+stirling_error(k)+stirling_error(n-k))

static double cumpr[SMALL_MEAN+3] = {0.0};


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
double stirling_error(int k)
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



int bin_trd(int n, double p)
{
  int flipped = p > 0.5, k, km;
  double q, u, v, us, k_real, f, pc;
  static int n_save = -1, nn_save = -1;
  static double p_save = -1.0, pp_save = -1.0;
  static int m, taille, K;
  static double r, npq, b, a, c, alpha, vr, urvr, h, aa, invvr;

  if ( flipped ) 
    { q = p; p = 1 - p; }
  else 
    { q = 1 - p;}

  if ( n*p <= SMALL_MEAN )     /* BINV algorithm with recording */
    {
      if ( n != n_save || p != p_save )   /* setup */
	{
	  r = p/q;
	  K = 1; cumpr[1] = pow(q, n); taille = 2 + (int) n*p;
	  p_save = p;
	  n_save = n;
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
	      if ( k == n ) break;  /* security in case of floating point errors */
	      f *= (r*(n+1-k))/k;
	      pc += f;
	      if ( k <= taille )
		cumpr[++K] = pc;
	    }
	  while ( u > pc );
	}
    }
 
  else                          /* BTRD algorithm */
    {

      if ( n != n_save || p != p_save )   /* setup */
	{
	  m = (int) (n+1)*p;
	  r = p/q;
	  npq = n*p*q;
	  b = 1.15 + 2.53*sqrt(npq);
	  a = -0.0873 + 0.0248*b + 0.01*p; aa = 2.0*a;
	  c = n*p + 0.5;
	  alpha = (2.83 + 5.1/b)*sqrt(npq);
	  vr = 0.92 - 4.2/b; invvr = 1.0/vr;
	  urvr = 0.86*vr;
	  p_save = p;
	  n_save = n;
	}

      while ( 1 )
	{

	  /* 	  u = rand_ranf() - 0.5; */
	  /* 	  v = rand_ranf(); */
	  /* 	  us = 0.5 - fabs(u); */
	  /* 	  if ( us >= 0.07 && v <= vr ) */
	  /* 	    { */
	  /* 	      k = (2*a/us + b)*u + c; */
	  /* 	      break; */
	  /* 	    } */

	  v = rand_ranf();
	  if ( v <= urvr )
	    {
	      /* 	      u = v / vr - 0.43; */
	      u = v * invvr - 0.43;
	      /* 	      k = ((2*a/(0.5 - fabs(u)) + b)*u + c); */
	      k = ((aa/(0.5 - fabs(u)) + b)*u + c);
	      break;
	    }
	  
	  if ( v >= vr )
	    u = rand_ranf() - 0.5;
	  else
	    {
	      /* 	      u = v / vr - 0.93; */
	      u = v * invvr - 0.93;
	      u = u >= 0 ? 0.5 - u : -0.5 - u;
	      v = vr*rand_ranf();
	    }
	  us = 0.5 - fabs(u);

	  /* 	  if ( (k_real = (2*a/us + b)*u + c) >= 0 && (k = (int) k_real) <= n )  */
	  if ( (k_real = (aa/us + b)*u + c) >= 0 && (k = (int) k_real) <= n ) 
	    {
	      v = v*alpha/(a/(us*us) + b);
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
		  if ( n != nn_save || p != pp_save )
		    {
		      h = PNBINOM(m,n,r);
		      nn_save = n; pp_save = p;
		    }
		  if ( v <= h + (n+1)*log((n+1-m)/((double) (n+1-k))) -  PNBINOM(k,n,r) )
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
