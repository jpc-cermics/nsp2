/* coshin.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*/MEMBR ADD NAME=COSHIN,SSI=0 
 * 
 *fonction:  coshin 
 *fonction cosinus hyperbolique inverse de x 
 *en double precision 
 *acheve le 05/12/85 
 *ecrit par philippe touron 
 * 
 * 
 *sous programmes appeles: aucun 
 * 
 * 
 *    Copyright INRIA 
 */
double nsp_calpack_coshin (double *x)
{
  /* System generated locals */
  double ret_val;

  ret_val = log (*x + sqrt (*x * *x - 1.));
  return ret_val;
}				/* coshin_ */
