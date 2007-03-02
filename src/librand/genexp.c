/* genexp.f -- translated by f2c (version 19961017).
   
	
*/

#include "grand.h"

double
rand_genexp (double *av)
{
  /* System generated locals */
  double ret_val;

  /* Local variables */

/* ********************************************************************** */

/*     DOUBLE PRECISION FUNCTION GENEXP( AV ) */

/*                    GENerate EXPonential random deviate */


/*                              Function */


/*     Generates a single random deviate from an exponential */
/*     distribution with mean AV. */


/*                              Arguments */


/*     AV --> The mean of the exponential distribution from which */
/*            a random deviate is to be generated. */
/*                              DOUBLE PRECISION AV */
/*     JJV                      (AV >= 0) */

/*     GENEXP <-- The random deviate. */
/*                              DOUBLE PRECISION GENEXP */


/*                              Method */


/*     Renames SEXPO from TOMS as slightly modified by BWB to use RANF */
/*     instead of SUNIF. */

/*     For details see: */

/*               Ahrens, J.H. and Dieter, U. */
/*               Computer Methods for Sampling From the */
/*               Exponential and Normal Distributions. */
/*               Comm. ACM, 15,10 (Oct. 1972), 873 - 882. */

/* ********************************************************************** */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Executable Statements .. */
/*     JJV added check to ensure AV >= 0.0 */
/* L10: */
  ret_val = rand_sexpo () * *av;
  return ret_val;
}				/* genexp_ */
