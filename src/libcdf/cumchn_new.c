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
 * cdf_cumchn_new:
 * @X: upper limit of integration of the non-central chi-square distribution.
 * @df: degrees of freedom of the non-central chi-square distribution. 
 * @pnonc: non-centrality parameter of the non-central chi-square distribution.
 * @cum: cumulative non-central chi-square distribution. 
 * @ccum:  compliment of cumulative non-central chi-square distribution
 * 
 *     calculates     the       cumulative      non-central    chi-square 
 *     distribution, i.e.,  the probability   that  a   random   variable 
 *     which    follows  the  non-central chi-square  distribution,  with 
 *     non-centrality  parameter    pnonc  and   continuous  degrees   of 
 *     freedom df, is less than or equal to x. 
 * 
 *  This code uses the same method than the Dcdflib cumchn but with various
 *  improvments:
 *
 *   - for large x (in fact x larger than the mean) we compute Q = 1 - P in
 *     place of P with the corresponding  26.4.25 serie for Q (see comments 
 *     inside the code). So we have a better accuracy (at least for Q) and P 
 *     goes to 1 smoothly (in the old code for very large X, P was stick at 
 *     a value under 1).  This lets the inverse function to be computed more 
 *     accurately for P near 1 (that is Q near 0)
 *
 *   - initials terms of the serie which are of Poisson probabilities
 *     are computed with the C. Loader dpois_raw function.
 *
 *   - stopping test is slightly different, the new code don't use sum < 1e-20
 *     to stop the iterations (the test  sum < 1e-20 leads to some inacuracy 
 *     in the relative error for small P)
 *
 *   - for x far enough the mean (we choose mean +- 50*std) set directly
 *     P,Q to (1,0) and (0,1)
 *
 *   - Nan are handled explicitely
 *
 *   - for small non centrality parameter we use 26.4.27 of Abramowitz and Stegun
 *     in place of approximate non central chisquare directly by the central 
 *     chisquare. 
 *
 *   - add display of a warning message in the nsp console if the serie
 *     is stopped by itermax instead of relative accuracy test (itermax
 *     is also changed to 10000, was 1000 in the old code). 
 *
 *   - the new code is more readable than the old one (at least for me).
 *
 *  This code have tested with the pari-gp software and seems to be quite
 *  accurate in a very large domain for both x, df and pnonc parameters
 *  (relative accuracy under 1e-13 and most of the time about 1e-14).
 * 
 **/

int cdf_cumchn_new (double *X, double *df, double *pnonc, double *cum, double *ccum)
{
  double x = *X, nu = *df, lambda = *pnonc;
  double mean = nu + lambda, sigma = sqrt(2.0*(nu + 2.0*lambda));

  if ( x <= Max(0.0, mean - 50.0*sigma ) )  /* FIXME: 50 was choosen arbitrarily... */
    {
      *cum = 0.0;
      *ccum = 1.0;
    }

  else if ( x > mean + 50.0*sigma )   /* FIXME: 50 was choosen arbitrarily... */
    {
      *cum = 1.0;
      *ccum = 0.0;
    }

  else if ( isnan(x) )
    {
      *cum = x;
      *ccum = x;
    }

  else if ( lambda <= 1e-9 )   /*  use formula  26.4.27 of Abramowitz and Stegun */
    {
      double a = nu + lambda;
      double b = lambda / a;
      double xx = x/(1.0 + b);
      double nu_bis = a/(1.0 + b);
      cdf_cumchi (&xx, &nu_bis, cum, ccum);
    }

  else
    {
      /* use formula  26.4.25
       *
       *  (1)  Pnc(x,nu,lambda) = \sum_{j=0}^{\infty} exp(-lambda/2) ( (lambda/2)^j / j! )  Pc(x, nu + 2*j) 
       *
       * and its complementary formula (P + Q = 1):
       *
       *  (2)  Qnc(x,nu,lambda) = \sum_{j=0}^{\infty} exp(-lambda/2) ( (lambda/2)^j / j! )  Qc(x, nu + 2*j) 
       *
       * where:  Pnc(x,nu,lambda) = cdf of the non central chi2 distribution
       *         Pc(x, nu) = cdf of the central chi2 distribution
       *
       * and also the recurrence formula:
       *
       *      Qc(x, nu + 2) =  Qc(x, nu ) + (x/2)^{nu/2} exp(-x/2) / Gamma(nu/2+1)  (3)
       *  <=> Pc(x, nu + 2) =  Pc(x, nu ) - (x/2)^{nu/2} exp(-x/2) / Gamma(nu/2+1)  (4)    
       *
       *   starting the sum from term in which the poisson weight (= exp(-lambda/2) ( (lambda/2)^j / j! )
       *   is greatest (j=jmode) and goes backward then forward
       *
       *   for x <= nu + lambda choose (1) and otherwise (2)
       *
       *   Using formula (3) or (4) to update the needed chisquare probability is done this way :
       *
       *   Starting with   Pc(x, nu + 2*jmode), we have using (4):
       *
       *     Pc(x, nu + 2*(jmode+1)) =  Pc(x, nu + 2*jmode) - (x/2)^{nu/2+jmode} exp(-x/2) / Gamma(nu/2+jmode+1)
       *
       *    So we need the term  cor := (x/2)^{nu/2+jmode} exp(-x/2) / Gamma(nu/2+jmode+1) := cor(jmode->jmode+1)
       *
       *    but other corrections terms are got from cor with an easy recurrence:
       * 
       *      cor(j -> j+1) = (x/2)/(nu/2+j+1) cor(j-1 -> j)
       */

      double poi_wgh, current_poi_wgh, cor, current_cor, df, term, pchi2, qchi2;
      const double lhalf = 0.5*lambda, xhalf = 0.5*x, nuhalf = 0.5*nu; 
      const int iter_max = 10000;
      const double eps = 1e-14;
      double sum, j, jmode;
      int iter, warning = 0;

      jmode = floor(lhalf);
      if (jmode == 0.0) jmode = 1.0;

      /*  Calculate Poisson weight term (j = jmode) */
      poi_wgh = nsp_dpois_raw(jmode,lhalf,0);

      /*  Calculate central chi-square */
      df = nu + 2.0*jmode;
      cdf_cumchi (&x, &df, &pchi2, &qchi2);

      /*  calculate cor */
      cor =  nsp_dpois_raw(nuhalf+jmode, xhalf, 0);

      if ( x <= nu + lambda )   /* compute P then Q by Q = 1 - P */
	{
	  double current_pchi2;

	  /* init */
	  sum = poi_wgh*pchi2; 

	  /* Sum backward from the central term towards zero */
	  current_poi_wgh = poi_wgh;
	  current_pchi2 = pchi2; 
	  current_cor = cor;
	  j = jmode-1.0; iter = 0;
	  do
	    {
	      current_poi_wgh *= (j+1.0)/lhalf;         /* update poisson weight */
	      current_cor *= (nuhalf + (j + 1.0))/xhalf;/* update correction term */
	      current_pchi2 += current_cor;             /* update chi2 probability */
	      term = current_poi_wgh * current_pchi2;   /* then next serie term */
	      sum += term; 	                        /* then sum */
	      iter++; j--;                          
	    }
	  while ( j >= 0.0 && term > eps*sum && iter <= iter_max ); 

	  if ( j >= 0.0 && term > eps*sum ) warning = 1;

	  /* Sum forward from the central term towards infinity */
	  current_poi_wgh = poi_wgh;
	  current_pchi2 = pchi2; 
	  current_cor = cor;
	  j = jmode+1.0; iter = 0;
	  do
	    {
	      current_poi_wgh *= lhalf/j; 	       /* update poisson weight */
	      current_pchi2 -= current_cor;            /* then chi2 probability */
	      term = current_poi_wgh * current_pchi2;  /* then next serie term */
	      sum += term;                             /* then sum */
	      current_cor *= xhalf/(nuhalf + j);       /* update correction term for next iteration */
	      iter++; j++;	                    
	    }
	  while ( term > eps*sum && iter <= iter_max ); 

	  if ( term > eps*sum ) warning = 1;

	  *cum = sum;
	  *ccum = 1.0 - sum;

	}
      else        /* compute Q then P by P = 1 - Q */
	{
	  double current_qchi2;

	  /* init */
	  sum = poi_wgh*qchi2; 

	  /* Sum backward from the central term towards zero */
	  current_poi_wgh = poi_wgh;
	  current_qchi2 = qchi2; 
	  current_cor = cor;
	  j = jmode-1.0; iter = 0;
	  do
	    {
	      current_poi_wgh *= (j+1.0)/lhalf;          /* update poisson weight */
	      current_cor *= (nuhalf + (j + 1.0))/xhalf; /* update correction term */
	      current_qchi2 -= current_cor;              /* then chi2 probability */
	      term = current_poi_wgh * current_qchi2;    /* then next serie term */
	      sum += term;                               /* then sum */
	      iter++; j--;
	    }
	  while ( j >= 0.0 && term > eps*sum && iter <= iter_max ); 

	  if ( j >= 0.0 && term > eps*sum ) warning = 1;

	  /* Sum forward from the central term towards infinity */
	  current_poi_wgh = poi_wgh;
	  current_qchi2 = qchi2; 
	  current_cor = cor;
	  j = jmode+1.0; iter = 0;
	  do
	    {
	      current_poi_wgh *= lhalf/j;              /* update poisson weight */
	      current_qchi2 += current_cor;            /* then chi2 probability */
	      term = current_poi_wgh * current_qchi2;  /* then next serie term */
	      sum += term;                             /* then sum */
	      current_cor *= xhalf/(nuhalf + j);       /* update correction term for next iteration */
	      iter++; j++;
	    }
	  while ( term > eps*sum && iter <= iter_max ); 

	  if ( term > eps*sum ) warning = 1;

	  *ccum = sum;
	  *cum = 1.0 - sum;
	}

      if ( warning )
	{
	  Sciprintf("Warning: possible lost of accuracy in cdf('nch',..) or cdfchn('PQ',..) \n");
	}
    }

  return 0;
}


