#include "cdf.h"


/* ********************************************************************** */
/*     SUBROUTINE CUMBIN(S,XN,PBIN,OMPR,CUM,CCUM) */
/*                    CUmulative BINomial distribution */
/*                              Function */
/*     Returns the probability   of 0  to  S  successes in  XN   binomial */
/*     trials, each of which has a probability of success, PBIN. */
/*                              Arguments */
/*     S --> The upper limit of cumulation of the binomial distribution. */
/*                                                  S is DOUBLE PRECISION */
/*     XN --> The number of binomial trials. */
/*                                                  XN is DOUBLE PRECISIO */
/*     PBIN --> The probability of success in each binomial trial. */
/*                                                  PBIN is DOUBLE PRECIS */
/*     OMPR --> 1 - PBIN */
/*                                                  OMPR is DOUBLE PRECIS */
/*     CUM <-- Cumulative binomial distribution. */
/*                                                  CUM is DOUBLE PRECISI */
/*     CCUM <-- Compliment of Cumulative binomial distribution. */
/*                                                  CCUM is DOUBLE PRECIS */
/*                              Method */
/*     Formula  26.5.24    of   Abramowitz  and    Stegun,  Handbook   of */
/*     Mathematical   Functions (1966) is   used  to reduce the  binomial */
/*     distribution  to  the  cumulative    beta distribution. */


int cdf_cumbin (double *s, double *xn, double *pr, double *ompr, double *cum,
		double *ccum)
{
  double d__1, d__2;

  if (!(*s < *xn))
    {
      goto L10;
    }
  d__1 = *s + 1.;
  d__2 = *xn - *s;
  cdf_cumbet (pr, ompr, &d__1, &d__2, ccum, cum);
  goto L20;
L10:
  *cum = 1.;
  *ccum = 0.;
L20:
  return 0;
}				/* cumbin_ */
