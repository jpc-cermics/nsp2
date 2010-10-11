#include "cdf.h"

/**
 * cdf_cumfnc:
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
 *     uses formula 26.6.20 of reference for infinite series. 
 *     series is calculated backward and forward from j = lambda/2 
 *     (this is the term with the largest poisson weight) until 
 *     the convergence criterion is met. 
 *     for speed, the incomplete beta functions are evaluated 
 *     by formula 26.5.16. 
 *               reference 
 *     handbood of mathematical functions 
 *     edited by milton abramowitz and irene a. stegun 
 *     national bureau of standards applied matematics series - 55 
 *     march 1965 
 *     p 947, equations 26.6.17, 26.6.18 
 *                              note 
 *     the sum continues until a succeeding term is less than eps 
 *     times the sum (or the sum is less than 1.0e-20).  eps is 
 *     set to 1.0e-4 in a data statement which can be changed. 
 * 
 * Returns: 0
 **/

int cdf_cumfnc (double *f, double *dfn, double *dfd, double *pnonc, double *cum, double *ccum)
{
  const double half=0.5E0, done=1.0E0, eps = 1e-14;
  double d__1, d__2, prod, dsum, b, betdn, betup, xnonc, dummy, xmult;
  double xx, yy, dnterm, centwt, upterm, adn, aup, sum;
  int ierr,  i__ ,  icent;

  if ( isnan(*f) )    /* nan was not well handled (Bruno, oct 2010) */
    {
      *cum = *ccum = *f;
      return 0;
    }

  if (*f <= 0.0)
    {
      *cum = 0.;
      *ccum = 1.;
      return 0;
    }

  if (*pnonc < 1e-10) 
    {
      /*     Handle case in which the non-centrality parameter is */
      /*     (essentially) zero. */
      cdf_cumf (f, dfn, dfd, cum, ccum);
      return 0;
    }

  xnonc = *pnonc / 2.;
  /*     Calculate the central term of the poisson weighting factor. */
  icent = (int) xnonc;
  if (icent == 0)
    {
      icent = 1;
    }
  /*     Compute central weight term */
  d__1 = (double) (icent + 1);
  centwt = exp (-xnonc + icent * log (xnonc) - cdf_alngam (d__1));
  /*     Compute central incomplete beta term */
  /*     Assure that minimum of arg to beta and 1 - arg is computed */
  /*          accurately. */
  prod = *dfn * *f;
  dsum = *dfd + prod;
  yy = *dfd / dsum;
  if (yy > half)
    {
      xx = prod / dsum;
      yy = done - xx;
    }
  else
    {
      xx = done - yy;
    }
  d__1 = *dfn * half + (double) icent;
  d__2 = *dfd * half;
  cdf_bratio (&d__1, &d__2, &xx, &yy, &betdn, &dummy, &ierr);
  adn = *dfn / 2. + (double) icent;
  aup = adn;
  b = *dfd / 2.;
  betup = betdn;
  sum = centwt * betdn;
  /*     Now sum terms backward from icent until convergence or all done */
  xmult = centwt;
  i__ = icent;
  d__1 = adn + b;
  d__2 = adn + 1.;
  dnterm =
    exp (cdf_alngam (d__1) - cdf_alngam (d__2) - cdf_alngam (b) +
	 adn * log (xx) + b * log (yy));
L30:
  d__1 = xmult * betdn;
  if (sum < 1e-20 || d__1 < eps * sum || i__ <= 0)
    {
      goto L40;
    }
  xmult *= i__ / xnonc;
  --i__;
  adn += -1;
  dnterm = (adn + 1) / ((adn + b) * xx) * dnterm;
  betdn += dnterm;
  sum += xmult * betdn;
  goto L30;
L40:
  i__ = icent + 1;
  /*     Now sum forwards until convergence */
  xmult = centwt;
  if (aup - 1 + b == 0.)
    {
      upterm =
	exp (-cdf_alngam (aup) - cdf_alngam (b) + (aup - 1) * log (xx) +
	     b * log (yy));
    }
  else
    {
      d__1 = aup - 1 + b;
      upterm =
	exp (cdf_alngam (d__1) - cdf_alngam (aup) - cdf_alngam (b) +
	     (aup - 1) * log (xx) + b * log (yy));
    }
  goto L60;
L50:
  d__1 = xmult * betup;
  if (sum < 1e-20 || d__1 < eps * sum)
    {
      goto L70;
    }
L60:
  xmult *= xnonc / i__;
  ++i__;
  aup += 1;
  upterm = (aup + b - 2.) * xx / (aup - 1) * upterm;
  betup -= upterm;
  sum += xmult * betup;
  goto L50;
L70:
  *cum = sum;
  *ccum = .5 - *cum + .5;
  return 0;
}		
