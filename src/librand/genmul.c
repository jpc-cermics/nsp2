/* genmul.f -- translated by f2c (version 19961017).
   
	
*/

#include "grand.h"

/* Subroutine */ int
rand_genmul (int *n, double *p, int *ncat, int *ix)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int icat;
  double prob;
  int ntot, i__;
  double sum;

/* ********************************************************************** */

/*            SUBROUTINE GENMUL( N, P, NCAT, IX ) */
/*     GENerate an observation from the MULtinomial distribution */


/*                              Arguments */


/*     N --> Number of events that will be classified into one of */
/*           the categories 1..NCAT */
/*                         INT N */

/*     P --> Vector of probabilities.  P(i) is the probability that */
/*           an event will be classified into category i.  Thus, P(i) */
/*           must be [0,1]. Only the first NCAT-1 P(i) must be defined */
/*           since P(NCAT) is 1.0 minus the sum of the first */
/*           NCAT-1 P(i). */
/*                         DOUBLE PRECISION P(NCAT-1) */

/*     NCAT --> Number of categories.  Length of P and IX. */
/*                         INT NCAT */

/*     IX <-- Observation from multinomial distribution.  All IX(i) */
/*            will be nonnegative and their sum will be N. */
/*                         INT IX(NCAT) */


/*                              Method */


/*     Algorithm from page 559 of */

/*     Devroye, Luc */

/*     Non-Uniform Random Variate Generation.  Springer-Verlag, */
/*     New York, 1986. */

/* ********************************************************************** */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
/*     Check Arguments */
/*     see Rand.c */
/*     Initialize variables */
  /* Parameter adjustments */
  --ix;
  --p;

  /* Function Body */
  ntot = *n;
  sum = 1.;
  i__1 = *ncat;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ix[i__] = 0;
/* L20: */
    }
/*     Generate the observation */
  i__1 = *ncat - 1;
  for (icat = 1; icat <= i__1; ++icat)
    {
      prob = p[icat] / sum;
      ix[icat] = rand_ignbin (&ntot, &prob);
      ntot -= ix[icat];
      if (ntot <= 0)
	{
	  return 0;
	}
      sum -= p[icat];
/* L30: */
    }
  ix[*ncat] = ntot;
/*     Finished */
  return 0;
}				/* genmul_ */
