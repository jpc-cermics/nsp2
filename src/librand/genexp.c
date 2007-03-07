#include "grand.h"

/*     DOUBLE PRECISION FUNCTION GENEXP( AV ) 
 *                    GENerate EXPonential random deviate 
 *                              Function 
 *     Generates a single random deviate from an exponential 
 *     distribution with mean AV. 
 *                              Arguments 
 *     AV --> The mean of the exponential distribution from which 
 *            a random deviate is to be generated. 
 *                              DOUBLE PRECISION AV 
 *     JJV                      (AV >= 0) 
 *     GENEXP <-- The random deviate. 
 *                              DOUBLE PRECISION GENEXP 
 *                              Method 
 *     Renames SEXPO from TOMS as slightly modified by BWB to use RANF 
 *     instead of SUNIF. 
 *     For details see: 
 *               Ahrens, J.H. and Dieter, U. 
 *               Computer Methods for Sampling From the 
 *               Exponential and Normal Distributions. 
 *               Comm. ACM, 15,10 (Oct. 1972), 873 - 882. 
 */

double rand_genexp (double av)
{
  return  rand_sexpo () * av;
}

