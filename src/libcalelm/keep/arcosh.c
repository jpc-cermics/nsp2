/* arcosh.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

double nsp_calpack_arcosh (double *x)
{
  /* System generated locals */
  double ret_val;

  /*!but 
   *calcule l'arcosinus hyperbolique d'un double precision 
   *!liste d'appel 
   *    double precision function arcosh(x) 
   *    double precision x 
   *! 
   *    Copyright INRIA 
   */
  if (*x < 1.)
    {
      goto L10;
    }
  ret_val = log (*x + sqrt (*x * *x - 1.));
  return ret_val;
L10:
  ret_val = 0.;
  return ret_val;
}				/* arcosh_ */

double nsp_calpack_arsinh (double *x)
{
  /* System generated locals */
  double ret_val;
  /*!but 
   *calcule l'arcsinus hyperbolique d'un double precision 
   *!liste d'appel 
   *    double precision function arsinh(x) 
   *    double precision x 
   *! 
   * 
   */
  ret_val = log (*x + sqrt (*x * *x + 1.));
  return ret_val;
}				/* arsinh_ */
