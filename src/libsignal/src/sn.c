/* sn.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/* Table of constant values */

static double c_b2 = .25;

/*/MEMBR ADD NAME=SN,SSI=0 
 */
double
signal_sn (double *y, double *a, double *ak1, double *ak3)
{
  /* System generated locals */
  int i__1, i__2;
  double ret_val, d__1, d__2;

  /* Local variables */
  int i__, n;
  double q, x, pi;
  int ns;
  double den, sup;

  /*! 
   * 
   */
  pi = 3.1415926535;
  ns = (int) (sqrt (*ak1 * 50. / (pi * *ak3)) + 2);
  x = *y * .5 / *ak1;
  q = exp (-pi * *ak3 / *ak1);
  sup = pow_dd (&q, &c_b2) * 2. * sin (pi * x);
  den = 1.;
  i__ = -2;
  n = 1;
 L1:
  /*Computing 2nd power 
   */
  d__2 = n + .5;
  d__1 = d__2 * d__2;
  sup += i__ * pow_dd (&q, &d__1) * sin (((n << 1) + 1) * pi * x);
  /*Computing 2nd power 
   */
  i__2 = n;
  i__1 = i__2 * i__2;
  den += i__ * nsp_pow_di (q, i__1) * cos (n * 2. * pi * x);
  i__ = -i__;
  ++n;
  if (n <= ns)
    {
      goto L1;
    }
  ret_val = sup / (den * sqrt (*a));
  return ret_val;
}				/* sn_ */
