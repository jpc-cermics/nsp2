#include "grand.h"


/*     DOUBLE PRECISION FUNCTION GENNCH( DF, XNONC ) 
 *           Generate random value of Noncentral CHIsquare variable 
 *                              Function 
 *     Generates random deviate  from the  distribution  of a  noncentral 
 *     chisquare with DF degrees  of freedom and noncentrality  parameter 
 *     XNONC. 
 *                              Arguments 
 *     DF --> Degrees of freedom of the chisquare 
 *            (Must be >= 1.0) 
 *                         DOUBLE PRECISION DF 
 *     XNONC --> Noncentrality parameter of the chisquare 
 *               (Must be >= 0.0) 
 *                         DOUBLE PRECISION XNONC 
 *                              Method 
 *     Uses fact that  noncentral chisquare  is  the  sum of a  chisquare 
 *     deviate with DF-1  degrees of freedom plus the  square of a normal 
 *     deviate with mean sqrt(XNONC) and standard deviation 1. 
 */

double rand_gennch (double *df, double *xnonc)
{
  double d__1, d__2;
  /*     JJV changed these to call SGAMMA and SNORM directly */
  /*     JJV changed abort to df < 1, and added case: df = 1 */
  /*     JJV changed this to call SGAMMA and SNORM directly */
  /*      gennch = genchi(df-1.0) + gennor(sqrt(xnonc),1.0)**2 */
  
  if (*df >= 1.000001)
    {
      d__1 = (*df - 1.) / 2.;
      /* Computing 2nd power */
      d__2 = rand_snorm () + sqrt (*xnonc);
      return rand_sgamma (d__1) * 2. + d__2 * d__2;
    }
  else 
    {
      /* JJV case DF = 1.0 */
      /* Computing 2nd power */
      d__1 = rand_snorm () + sqrt (*xnonc);
      return  d__1 * d__1;
    }
}

