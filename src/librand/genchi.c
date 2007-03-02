#include "grand.h"

/* ********************************************************************** */
/*     DOUBLE PRECISION FUNCTION GENCHI( DF ) */
/*                Generate random value of CHIsquare variable */
/*                              Function */
/*     Generates random deviate from the distribution of a chisquare */
/*     with DF degrees of freedom random variable. */
/*                              Arguments */
/*     DF --> Degrees of freedom of the chisquare */
/*            (Must be positive) */
/*                         DOUBLE PRECISION DF */
/*                              Method */
/*     Uses relation between chisquare and gamma. */

double rand_genchi (double *df)
{
  /* System generated locals */
  double ret_val, d__1;
  /*     JJV changed this to call sgamma directly */
  /*   10 genchi = 2.0*gengam(1.0,df/2.0) */
  d__1 = *df / 2.;
  ret_val = rand_sgamma (&d__1) * 2.;
  return ret_val;
}	


