
#include <nsp/machine.h> 
#include "grand.h"

/* 
 *
 *   DOUBLE PRECISION FUNCTION GENF( DFN, DFD ) 
 *              GENerate random deviate from the F distribution 
 *                              Function 
 *     Generates a random deviate from the F (variance ratio) 
 *   distribution with DFN degrees of freedom in the numerator 
 *   and DFD degrees of freedom in the denominator. 
 *                              Arguments 
 *     DFN --> Numerator degrees of freedom 
 *           (Must be positive) 
 *                            DOUBLE PRECISION DFN 
 *    DFD --> Denominator degrees of freedom 
 *           (Must be positive) 
 *                            DOUBLE PRECISION DFD 
 *                              Method 
 *     Directly generates ratio of chisquare variates 
 */

double rand_genf(double *dfn, double *dfd)
{
  double d1, xden, xnum;
  /*     JJV changed this code to call sgamma directly */
  d1 = *dfn / 2.0E0;
  xnum = rand_sgamma(&d1) * 2.0E0 / *dfn;
  d1 = *dfd / 2.0E0;
  xden = rand_sgamma(&d1) * 2.0E0 / *dfd;
  /*     JJV changed constant so that it will not underflow at compile time */
  /*     JJV while not slowing generator by using double precision or logs. */
  /*      IF (.NOT. (xden.LE. (1.0E-38*xnum))) GO TO 20 */
  if (! (xden <= xnum * 1e-37))
    {
      return  xnum / xden;
    }
  Sciprintf("Warning: F generated numbers would cause overflow returning 1.0E37");
  /*     JJV next 2 lines changed to maintain truncation of large deviates. */
  /*      genf = 1.0E38 */
  return  1e37;
} 


