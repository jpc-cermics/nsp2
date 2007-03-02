#include "grand.h"


/*     INT FUNCTION IGNNBN( N, P ) */
/*                GENerate Negative BiNomial random deviate */
/*                              Function */
/*     Generates a single random deviate from a negative binomial */
/*     distribution. */
/*                              Arguments */
/*     N  --> Required number of events. */
/*                              INT N */
/*     JJV                      (N > 0) */
/*     P  --> The probability of an event during a Bernoulli trial. */
/*                              DOUBLE PRECISION P */
/*     JJV                      (0.0 < P < 1.0) */
/*                              Method */
/*     Algorithm from page 480 of */
/*     Devroye, Luc */
/*     Non-Uniform Random Variate Generation.  Springer-Verlag, */
/*     New York, 1986. */

int rand_ignnbn (int *n, double *p)
{
  int ret_val;
  double a, r__, y;
  /*     JJV changed to call SGAMMA directly */
  /*     JJV changed argumnet checker to abort if N <= 0 */
  /*     See Rand,c */
  /*     Generate Y, a random gamma (n,(1-p)/p) variable */
  /*     JJV Note: the above parametrization is consistent with Devroye, */
  /*     JJV       but gamma (p/(1-p),n) is the equivalent in our code */
  r__ = (double) (*n);
  a = *p / (1. - *p);
  /*      y = gengam(a,r) */
  y = rand_sgamma (&r__) / a;
  /*     Generate a random Poisson(y) variable */
  ret_val = rand_ignpoi (&y);
  return ret_val;
}	

