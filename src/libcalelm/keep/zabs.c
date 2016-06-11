/* zabs.f -- translated by f2c (version 19961017). */

#include "calpack.h"

/* 
 *    ZABS COMPUTES THE ABSOLUTE VALUE OR MAGNITUDE OF A DOUBLE 
 *    PRECISION COMPLEX VARIABLE CMPLX(ZR,ZI) 
 */

double nsp_calpack_zabs (double *zr, double *zi)
{
  double ret_val;
  double q, s, u, v;
  u = Abs (*zr);
  v = Abs (*zi);
  s = u + v;
  /*----------------------------------------------------------------------- 
   *    S*1.0D0 MAKES AN UNNORMALIZED UNDERFLOW ON CDC MACHINES INTO A 
   *    TRUE FLOATING ZERO 
   *----------------------------------------------------------------------- 
   */
  s *= 1.;
  if (s == 0.)
    {
      goto L20;
    }
  if (u > v)
    {
      goto L10;
    }
  q = u / v;
  ret_val = v * sqrt (q * q + 1.);
  return ret_val;
 L10:
  q = v / u;
  ret_val = u * sqrt (q * q + 1.);
  return ret_val;
 L20:
  ret_val = 0.;
  return ret_val;
}

