/* Nsp
 * Copyright (C) 2010 Bruno Pincon Esial/Iecn
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

#include <nsp/spmf.h>
#include <nsp/sciio.h>
#include "cdf.h"


/**
 * nsp_cdf_cumfnc:
 * @f: upper limit of integration of noncentral f in equation 
 * @dfn: degrees of freedom of numerator 
 * @dfd: degrees of freedom of denominator 
 * @pnonc: noncentrality parameter
 * @cum: cumulative noncentral f distribution 
 * @ccum: compliment of cummulative 
 * 
 * computes noncentral f distribution with dfn and dfd 
 * degrees of freedom and noncentrality parameter pnonc 
 * 
 *  This code uses the same method than the Dcdflib cumchn but with various
 *  improvments:
 *
 *   - we compute at the same time P and Q (cum and ccum), P by the serie
 *     26.6.20 (AS) and Q by the twin serie:
 *
 *        P(f) = sum_{j=0}^{+oo}  exp(-lambda) lambda^j / j!   Ixx(nu1+j,nu2)
 *        Q(f) = sum_{j=0}^{+oo}  exp(-lambda) lambda^j / j!   I(1-xx)(nu2,nu1+j)
 *
 *      where:
 *          xx = f*dfn / (dfd + f*dfn); yy = 1-xx = dfd / (dfd + f*dfn)
 *          nu1 = dfn/2; nu2 = dfd/2; lambda = pnonc/2
 *
 *      Noting psum and qsum the approximation got, we choose the smaller 
 *      as result and set :
 *         if psum <= qsum
 *            cum = psum; ccum = 1 - psum;
 *         else
 *            ccum = qsum; cum = 1 - qsum;
 *
 *      The dcdflib method consists in starting the serie at jmode index which
 *      corresponds to the max Poisson probability, then sum backward (j -> 0)
 *      then sum forward (j ->+oo) using  formula 26.5.16 (AS) to compute bratio
 *      Ix(nu1+j,nu2) recursively.
 *
 *      The use of the serie for Q provides additionnal accuracy over the dcdflib 
 *      code (which uses only 26.6.20) for big f arguments (that is when p -> 1, q -> 0).
 *
 *   - initials Poisson probabilities are computed with the C. Loader 
 *     dpois_raw function, initial correction terms (for computing bratio 
 *     probabilities with formula 26.5.16 (AS)) use  C. Loader nsp_dbinom_raw 
 *     function.  
 *
 *   - stopping test is slightly different, the new code don't use sum < 1e-20
 *     to stop the iterations. We add an iteration counter and stop if too much
 *     iterations are needed and display a warning message to prevent possible
 *     loss of accuracy. 
 *
 *   - Nan are handled explicitely
 *
 *   - for small non centrality parameter we use an approximation by central
 *     F (and not directly the central F), see  32.4 of Hand-book on STATISTICAL
 *     DISTRIBUTIONS for experimentalists by Christian Walck Particle Physics Group
 *     Fysikum University of Stockholm.
 *
 *   - the new code is more readable than the old one (at least for me).
 *
 * Returns: 0
 **/

int nsp_cdf_cumfnc (double f, double dfn, double dfd, double pnonc, double *cum, double *ccum)
{
  if ( isnan(f) )    /* handled nan */
    {
      *cum = *ccum = f;
    }

  else if ( f <= 0.0 )
    {
      *cum = 0.0; *ccum = 1.0;
    }

  else if ( f > DBL_MAX )
    {
      *cum = 1.0; *ccum = 0.0;
    }

  else if ( pnonc < 1e-7 )   /* approximate by a central F */ 
    {
      double xc = (dfn/(dfn + pnonc))* f, 
	     nu = dfn + (pnonc*pnonc)/(dfn + 2.0*pnonc);
      cdf_cumf (&xc, &nu, &dfd, cum, ccum);
      return 0;
    }

  else   
    {
      /*    xx = f*dfn / (dfd + f*dfn); yy = 1-xx = dfd / (dfd + f*dfn)
       *    
       *    nu1 = dfn/2; nu2 = dfd/2; lambda = pnonc/2
       *
       *    P(x) = sum_{j=0}^{+oo}  exp(-lambda) lambda^j / j!   Ixx(nu1+j,nu2)
       *    Q(x) = sum_{j=0}^{+oo}  exp(-lambda) lambda^j / j!   I(1-xx)(nu2,nu1+j)
       *
       */
      const double nu1 = 0.5*dfn, lambda = 0.5*pnonc, eps = 1e-14;
      double nu2 = 0.5*dfd, xx, yy, nu1pj, j, jmode, poi_wgh, pterm, qterm, 
	pbratio, qbratio;
      double current_poi_wgh, current_pbratio, current_qbratio, cor, psum, qsum;
      int ierr, iter, warning = 0;
      const int itermax = 10000;

      /* calculate max Poisson weight term (j = jmode) */
      jmode = Max( ceil(lambda) - 1.0 , 1.0 );
      poi_wgh = nsp_dpois_raw(jmode,lambda,0);

      /* calculate xx,yy */
      yy =  1.0 / ( 1.0 + (f*dfn)/dfd );
      if ( yy < 0.5 )
	xx = 1.0 - yy;
      else
	{
	  xx = 1.0 / ( 1.0 + dfd/(f*dfn) );
	  yy = 1.0 - xx;
	}

      /* calculate bratio for j=jmode */
      nu1pj = nu1 + jmode;
      cdf_bratio (&nu1pj, &nu2, &xx, &yy, &pbratio, &qbratio, &ierr);

      /* calculate first term */
      psum = poi_wgh * pbratio;
      qsum = poi_wgh * qbratio;


      /*** sum toward 0 (until cvg or j=0) ***/

      /* calculate correction to compute next bratio terms toward 0 */
      cor = nsp_dbinom_raw(nu1 + (jmode - 1.0), nu1 + nu2 + (jmode - 1.0), xx, yy, 0)/(1.0 + (nu1 + (jmode - 1.0))/nu2);

      /* add first jmode-1 terms */
      j = jmode-1.0;
      current_poi_wgh = poi_wgh*(jmode/lambda);
      current_pbratio = pbratio + cor;
      current_qbratio = qbratio - cor;
      pterm = current_poi_wgh * current_pbratio;
      qterm = current_poi_wgh * current_qbratio;
      psum += pterm; qsum += qterm;
      
      /* add jmode-2, jmode-3, .... terms */
      iter = 1;
      while ( j > 0.0 && ( pterm > eps*psum || qterm > eps*qsum ) && iter <= itermax )
	{
	  current_poi_wgh *= (j/lambda);   /* update poisson weight term */
	  cor /= (( 1.0 + (nu2 - 1.0)/(nu1 + j) )*xx); /* update correction for bratio */
	  current_pbratio += cor; current_qbratio -= cor; /* update bratio terms */
	  pterm = current_poi_wgh * current_pbratio;
	  qterm = current_poi_wgh * current_qbratio;
	  psum += pterm; qsum += qterm;
	  j--; iter++;
	}
	  
      if ( j > 0.0 && ( pterm > eps*psum  || qterm > eps*qsum ) ) 
	warning = 1;
	  

      /*** sum toward +oo (until cvg) ***/

      /* calculate correction to compute next bratio terms toward +oo */
      cor = nsp_dbinom_raw(nu1pj, nu1 + nu2 + jmode, xx, yy, 0)/(1.0 + (nu1 + jmode)/nu2);

      /* add first jmode+1 terms */
      j = jmode+1.0;
      current_poi_wgh = poi_wgh*(lambda/j);
      current_pbratio = pbratio - cor;
      current_qbratio = qbratio + cor;
      pterm = current_poi_wgh * current_pbratio;
      qterm = current_poi_wgh * current_qbratio;
      psum += pterm; qsum += qterm;
      
      /* add jmode+2, jmode+3, .... terms */
      iter = 1;
      while ( (pterm > eps*(psum) || qterm > eps*(qsum)) && iter <= itermax )
	{
	  current_poi_wgh *= (lambda/(j+1.0));   /* update poisson weight term */
	  cor *= (1.0 + (nu2 - 1.0)/(nu1 + j) )*xx; /* update correction for bratio */
	  current_pbratio -= cor; current_qbratio += cor; /* update bratio terms */
	  pterm = current_poi_wgh * current_pbratio;
	  qterm = current_poi_wgh * current_qbratio;
	  psum += pterm; qsum += qterm;
	  j++;
	}

      if ( pterm > eps*psum  || qterm > eps*qsum ) 
	warning = 1;

      if ( warning )
	{
	  Sciprintf("Warning: possible lost of accuracy in cumfnc\n");
	}

      if ( qsum < psum )
	{
	  *ccum = qsum; *cum = 1.0 - qsum;
	}
      else
	{
	  *cum = psum; *ccum = 1.0 - psum;
	}
    }

  return 0;
}		
