/* gennch.f -- translated by f2c (version 19961017).
   
	
*/

#include "grand.h"

double
rand_gennch (double *df, double *xnonc)
{
  double ret_val, d__1, d__2;
  /* Local variables */

/* ********************************************************************** */

/*     DOUBLE PRECISION FUNCTION GENNCH( DF, XNONC ) */
/*           Generate random value of Noncentral CHIsquare variable */


/*                              Function */


/*     Generates random deviate  from the  distribution  of a  noncentral */
/*     chisquare with DF degrees  of freedom and noncentrality  parameter */
/*     XNONC. */


/*                              Arguments */


/*     DF --> Degrees of freedom of the chisquare */
/*            (Must be >= 1.0) */
/*                         DOUBLE PRECISION DF */

/*     XNONC --> Noncentrality parameter of the chisquare */
/*               (Must be >= 0.0) */
/*                         DOUBLE PRECISION XNONC */


/*                              Method */


/*     Uses fact that  noncentral chisquare  is  the  sum of a  chisquare */
/*     deviate with DF-1  degrees of freedom plus the  square of a normal */
/*     deviate with mean sqrt(XNONC) and standard deviation 1. */

/* ********************************************************************** */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. External Functions .. */
/*     JJV changed these to call SGAMMA and SNORM directly */
/*      DOUBLE PRECISION genchi,gennor */
/*      EXTERNAL genchi,gennor */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     JJV changed abort to df < 1, and added case: df = 1 */
/*     .. Executable Statements .. */
/*     JJV changed this to call SGAMMA and SNORM directly */
/*      gennch = genchi(df-1.0) + gennor(sqrt(xnonc),1.0)**2 */
/* L10: */
  if (*df >= 1.000001)
    {
      goto L20;
    }
/*     JJV case DF = 1.0 */
/* Computing 2nd power */
  d__1 = rand_snorm () + sqrt (*xnonc);
  ret_val = d__1 * d__1;
  goto L30;
/*     JJV case DF > 1.0 */
L20:
  d__1 = (*df - 1.) / 2.;
/* Computing 2nd power */
  d__2 = rand_snorm () + sqrt (*xnonc);
  ret_val = rand_sgamma (d__1) * 2. + d__2 * d__2;
L30:
  return ret_val;
}				/* gennch_ */
