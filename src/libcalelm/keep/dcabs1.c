/* dcabs1.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

double C2F (dcabs1) (doubleC * z__)
{
  /* System generated locals */
  double ret_val;
  static doubleC equiv_0[1];

  /* Local variables */
#define t ((double *)equiv_0)
#define zz (equiv_0)

  zz->r = z__->r, zz->i = z__->i;
  ret_val = Abs (t[0]) + Abs (t[1]);
  return ret_val;
}				/* dcabs1_ */

#undef zz
#undef t
