/* ********************************************************************** */
/*               F -NON- -C-ENTRAL F DISTRIBUTION */
/*                              Function */
/*     COMPUTES NONCENTRAL F DISTRIBUTION WITH DFN AND DFD */
/*     DEGREES OF FREEDOM AND NONCENTRALITY PARAMETER PNONC */
/*                              Arguments */
/*     X --> UPPER LIMIT OF INTEGRATION OF NONCENTRAL F IN EQUATION */
/*     DFN --> DEGREES OF FREEDOM OF NUMERATOR */
/*     DFD -->  DEGREES OF FREEDOM OF DENOMINATOR */
/*     PNONC --> NONCENTRALITY PARAMETER. */
/*     CUM <-- CUMULATIVE NONCENTRAL F DISTRIBUTION */
/*     CCUM <-- COMPLIMENT OF CUMMULATIVE */
/*                              Method */
/*     USES FORMULA 26.6.20 OF REFERENCE FOR INFINITE SERIES. */
/*     SERIES IS CALCULATED BACKWARD AND FORWARD FROM J = LAMBDA/2 */
/*     (THIS IS THE TERM WITH THE LARGEST POISSON WEIGHT) UNTIL */
/*     THE CONVERGENCE CRITERION IS MET. */
/*     FOR SPEED, THE INCOMPLETE BETA FUNCTIONS ARE EVALUATED */
/*     BY FORMULA 26.5.16. */
/*               REFERENCE */
/*     HANDBOOD OF MATHEMATICAL FUNCTIONS */
/*     EDITED BY MILTON ABRAMOWITZ AND IRENE A. STEGUN */
/*     NATIONAL BUREAU OF STANDARDS APPLIED MATEMATICS SERIES - 55 */
/*     MARCH 1965 */
/*     P 947, EQUATIONS 26.6.17, 26.6.18 */
/*                              Note */
/*     THE SUM CONTINUES UNTIL A SUCCEEDING TERM IS LESS THAN EPS */
/*     TIMES THE SUM (OR THE SUM IS LESS THAN 1.0E-20).  EPS IS */
/*     SET TO 1.0E-4 IN A DATA STATEMENT WHICH CAN BE CHANGED. */
/* ********************************************************************** */

#include "cdf.h"

int
cdf_cumfnc (double *f, double *dfn, double *dfd, double *pnonc, double *cum,
	    double *ccum)
{
  const double half=0.5E0, done=1.0E0;
  static double eps = 1e-4;
  double d__1, d__2;
  int ierr;
  double prod, dsum, b;
  int i__;
  double betdn;
  int icent;
  double betup, xnonc, dummy, xmult;
  double xx;
  double yy, dnterm, centwt, upterm, adn, aup, sum;

  if (!(*f <= 0.))
    {
      goto L10;
    }
  *cum = 0.;
  *ccum = 1.;
  return 0;
L10:
  if (!(*pnonc < 1e-10))
    {
      goto L20;
    }

/*     Handle case in which the non-centrality parameter is */
/*     (essentially) zero. */
  cdf_cumf (f, dfn, dfd, cum, ccum);
  return 0;
L20:
  xnonc = *pnonc / 2.;
/*     Calculate the central term of the poisson weighting factor. */
  icent = (int) xnonc;
  if (icent == 0)
    {
      icent = 1;
    }
/*     Compute central weight term */
  d__1 = (double) (icent + 1);
  centwt = exp (-xnonc + icent * log (xnonc) - cdf_alngam (&d__1));
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
    exp (cdf_alngam (&d__1) - cdf_alngam (&d__2) - cdf_alngam (&b) +
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
	exp (-cdf_alngam (&aup) - cdf_alngam (&b) + (aup - 1) * log (xx) +
	     b * log (yy));
    }
  else
    {
      d__1 = aup - 1 + b;
      upterm =
	exp (cdf_alngam (&d__1) - cdf_alngam (&aup) - cdf_alngam (&b) +
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
}				/* cumfnc_ */
