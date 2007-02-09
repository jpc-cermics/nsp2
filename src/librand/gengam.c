#include "grand.h"

/*     DOUBLE PRECISION FUNCTION GENGAM( A, R ) 
 *           GENerates random deviates from GAMma distribution 
 *                              Function 
 *     Generates random deviates from the gamma distribution whose 
 *     density is 
 *          (A**R)/Gamma(R) * X**(R-1) * Exp(-A*X) 
 *                              Arguments 
 *     JJV added the argument ranges supported 
 *     A --> Location parameter of Gamma distribution 
 *                              DOUBLE PRECISION A ( A > 0 ) 
 *     R --> Shape parameter of Gamma distribution 
 *                              DOUBLE PRECISION R ( R > 0 ) 
 *                              Method 
 *     Renames SGAMMA from TOMS as slightly modified by BWB to use RANF 
 *     instead of SUNIF. 
 *     For details see: 
 *               (Case R >= 1.0) 
 *               Ahrens, J.H. and Dieter, U. 
 *               Generating Gamma Variates by a 
 *               Modified Rejection Technique. 
 *               Comm. ACM, 25,1 (Jan. 1982), 47 - 54. 
 *     Algorithm GD 
 *     JJV altered the following to reflect sgamma argument ranges 
 *               (Case 0.0 < R < 1.0) 
 *               Ahrens, J.H. and Dieter, U. 
 *               Computer Methods for Sampling from Gamma, 
 *               Beta, Poisson and Binomial Distributions. 
 *               Computing, 12 (1974), 223-246/ 
 *     Adapted algorithm GS. 
 */

double rand_gengam (double a, double r)
{
  return  rand_sgamma (r) / a;
}	

