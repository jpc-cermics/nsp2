#include "cdf.h"
#include <nsp/spmf.h>

/*********************************************************************** 
 * 
 *    SUBROUTINE CUMTNC(T,DF,PNONC,CUM,CCUM) 
 * 
 *                CUMulative Non-Central T-distribution 
 * 
 * 
 *                             Function 
 * 
 * 
 *    Computes the integral from -infinity to T of the non-central 
 *    t-density. 
 *                             Arguments 
 *    T --> Upper limit of integration of the non-central t-density. 
 *                                                 T is DOUBLE PRECISION 
 *    DF --> Degrees of freedom of the non-central t-distribution. 
 *                                                 DF is DOUBLE PRECISIO 
 *    PNONC --> Non-centrality parameter of the non-central t distibutio 
 *                                                 PNONC is DOUBLE PRECI 
 *    CUM <-- Cumulative t-distribution. 
 *                                                 CCUM is DOUBLE PRECIS 
 *    CCUM <-- Compliment of Cumulative t-distribution. 
 *                                                 CCUM is DOUBLE PRECIS 
 *                             Method 
 *    Upper tail    of  the  cumulative  noncentral t   using 
 *    formulae from page 532  of Johnson, Kotz,  Balakrishnan, Coninuous 
 *    Univariate Distributions, Vol 2, 2nd Edition.  Wiley (1995) 
 * 
 *    This implementation starts the calculation at i = lambda, 
 *    which is near the largest Di.  It then sums forward and backward. 
 ************************************************************************ 
 */

int cdf_cumtnc_new (double *t, double *df, double *pnonc, double *cum,
		double *ccum)
{
  int ierr, qrevs;
  double d__1, d__2, cent, term, twoi, b, d__, e, s, x, bcent, dcent, ecent;
  double scent, t2, pnonc2, bb, lambda, halfdf, bbcent, xi, ss, tt;
  double dpnonc, sscent;
  double omx, dum1, dum2;
  double eps=1e-14; /* add a parameter to stop summation (1e-7 was hardcoded). Bruno, april, 5, 2010 */

  if ( isnan(*t) )    /* nan was not well handled (Bruno, sept 2010) */
    {
      *cum = *ccum = *t;
      return 0;
    }

  if ( fabs(*pnonc) <= 1e-10)
    {
      cdf_cumt (t, df, cum, ccum);
      return 0;
    }

  qrevs = *t < 0.;
  if (qrevs)
    {
      tt = -(*t);
      dpnonc = -(*pnonc);
    }
  else
    {
      tt = *t;
      dpnonc = *pnonc;
    }

  pnonc2 = dpnonc * dpnonc;
  t2 = tt * tt;
  if ( fabs(tt) <= 1e-10)
    {
      double d = -(*pnonc);
      cdf_cumnor (&d, cum, ccum);
      return 0;
    }

  lambda = pnonc2 * 0.5;
  x = *df / (*df + t2);
  omx =t2 / (*df + t2);   /* this is 1.0 - x; */
  halfdf = *df * 0.5;

  /*    ******************** Case i = lambda  */
  cent = ceil(lambda) - 1.0; 
  if ( cent < 1.0 ) cent = 1.0;
  /*    Compute d=T(2i)   */
  dcent = nsp_dpois_raw(cent, lambda, 0);
  /*    Compute e=t(2i+1) */
  ecent =  nsp_dpois_raw(cent+0.5, lambda, 0);

  if (dpnonc < 0.0)
    ecent = -ecent;

  d__1 = cent + .5;
  cdf_bratio (&halfdf, &d__1, &x, &omx, &bcent, &dum1, &ierr);
  /*    compute bbcent=B(2*cent+1) 
   */
  d__1 = cent + 1.;
  cdf_bratio (&halfdf, &d__1, &x, &omx, &bbcent, &dum2, &ierr);
  /*    Case bcent and bbcent are essentially zero 
   *    Thus t is effectively infinite 
   */
  if (bcent + bbcent < 1e-20)  /* this threshold was changed (old value: 1e-10), bruno, may 2010 */
                               /* such that p grows more smoothly to 1 (and q to 0)  */
    {
      if (qrevs)
	{
	  *cum = 0.0; *ccum = 1.0;
	}
      else
	{
	  *cum = 1.0; *ccum = 0.0;
	}
      return 0;
    }
  /*    Case bcent and bbcent are essentially one 
   *    Thus t is effectively zero 
   */
  if (dum1 + dum2 < 1e-20)  /* this threshold was changed (old value: 1e-10), bruno, may 2010 */  
    {
      d__1 = -(*pnonc);
      cdf_cumnor (&d__1, cum, ccum);
      return 0;
    }

  /*    First term in ccum is D*B + E*BB    */
  *ccum = dcent * bcent + ecent * bbcent;

  /*    compute s(cent) = B(2*(cent+1)) - B(2*cent))  */
  d__1 = halfdf + cent + .5;
  scent = (halfdf / d__1)*nsp_dbinom_raw(halfdf, d__1, x, omx, 0);

  /*    compute ss(cent) = B(2*cent+3) - B(2*cent+1)  */
/*   sscent = sqrt(omx)*halfdf*nsp_dbinom_raw(halfdf,  halfdf + cent, x, omx, 0)/(cent+1.0); */

  /*    compute ss(cent) = B(2*cent+3) - B(2*cent+1)  */
  d__1 = halfdf + cent + 1.;
  d__2 = cent + 2.;
  sscent = exp(lgamma(d__1) - lgamma(d__2) - lgamma(halfdf) + halfdf * log(x) + (cent + 1.) * log(omx));

  /*    ******************** Sum Forward    */
  xi = cent + 1.0;
  twoi = xi * 2.0;
  d__ = dcent;
  e = ecent;
  b = bcent;
  bb = bbcent;
  s = scent;
  ss = sscent;

  do
    {
      b += s;
      bb += ss;
      d__ = lambda / xi * d__;
      e = lambda / (xi + 0.5) * e;
      term = d__ * b + e * bb;
      *ccum += term;
      s = s * omx * (*df + twoi - 1.0) / (twoi + 1.0);
      ss = ss * omx * (*df + twoi) / (twoi + 2.0);
      xi += 1.0;
      twoi = xi * 2.0;
    } 
  while ( fabs(term) > *ccum * eps);


  /*    ******************** Sum Backward    */
  xi = cent;
  twoi = xi * 2.0;
  d__ = dcent;
  e = ecent;
  b = bcent;
  bb = bbcent;
  s = scent * (twoi + 1.0) / ((*df + twoi - 1.0) * omx);
  ss = sscent * (twoi + 2.0) / ((*df + twoi) * omx);
 
  do
    {
      b -= s;
      bb -= ss;
      d__ *= xi / lambda;
      e *= (xi + 0.5) / lambda;
      term = d__ * b + e * bb;
      *ccum += term;
      xi += -1.0;
      if (xi < 0.5) break;
      twoi = xi * 2.0;
      s = s * (twoi + 1.0) / ((*df + twoi - 1.0) * omx);
      ss = ss * (twoi + 2.0) / ((*df + twoi) * omx);
    }
  while ( fabs(term) > *ccum * eps);

  if (qrevs)
    {
      *cum = *ccum * 0.5; *ccum = 1.0 - *cum;
    }
  else
    {
      *ccum *= 0.5; *cum = 1.0 - *ccum;
    }

  /*    Due to roundoff error the answer may not lie between zero and one.  Force it to do so. */ 
  d__1 = Min (*cum, 1.0);
  *cum = Max (d__1, 0.0);
  d__1 = Min (*ccum, 1.);
  *ccum = Max (d__1, 0.0);
  return 0;
}				/* cumtnc_ */
