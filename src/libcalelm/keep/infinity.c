/* infinity.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

double nsp_calpack_infinity (double *x)
{
  /* System generated locals */
  double ret_val;

  /*Purpose 
   *    Return infinity value 
   *Calling sequence 
   *    a = infinity(0.0d) 
   *    Copyright INRIA 
   */
  ret_val = 1. / *x;
  return ret_val;
}				/* infinity_ */
