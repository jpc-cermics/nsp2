/* genunf.f -- translated by f2c (version 19961017).
   
	
*/

#include "grand.h"

double
rand_genunf (double *low, double *high)
{
  /* System generated locals */
  double ret_val;

  /* Local variables */

/* ********************************************************************** */

/*     DOUBLE PRECISION FUNCTION GENUNF( LOW, HIGH ) */

/*               GeNerate Uniform DOUBLE PRECISION between LOW and HIGH */


/*                              Function */


/*     Generates a DOUBLE PRECISION uniformly distributed between LOW and HIGH. */


/*                              Arguments */


/*     LOW --> Low bound (exclusive) on DOUBLE PRECISION value to be generated */
/*                         DOUBLE PRECISION LOW */

/*     HIGH --> High bound (exclusive) on DOUBLE PRECISION value to be generated */
/*                         DOUBLE PRECISION HIGH */

/* ********************************************************************** */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Executable Statements .. */
/* L10: */
  ret_val = *low + (*high - *low) * rand_ranf ();
  return ret_val;
}				/* genunf_ */
