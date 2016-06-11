/* nextk.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Common Block Declarations */

struct
{
  double p[101], qp[101], k[101], qk[101], svk[101], sr, si, u, v, a, b, c__,
    d__, a1, a2, a3, a6, a7, e, f, g, h__, szr, szi, lzr, lzi, eta, are, mre;
  int n, nn;
} gloglo_;

#define gloglo_1 gloglo_

int nsp_ctrlpack_nextk (int *type__)
{
  int i__1;
  double temp;
  int i__;

  /*computes the next k polynomials using scalars 
   *computed in calcsc 
   */
  if (*type__ == 3)
    {
      goto L40;
    }
  temp = gloglo_1.a;
  if (*type__ == 1)
    {
      temp = gloglo_1.b;
    }
  if (Abs (gloglo_1.a1) > Abs (temp) * gloglo_1.eta * 10.)
    {
      goto L20;
    }
  /*if a1 is nearly zero then use a special form of the 
   *recurrence 
   */
  gloglo_1.k[0] = 0.;
  gloglo_1.k[1] = -gloglo_1.a7 * gloglo_1.qp[0];
  i__1 = gloglo_1.n;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      gloglo_1.k[i__ - 1] =
	gloglo_1.a3 * gloglo_1.qk[i__ - 3] - gloglo_1.a7 * gloglo_1.qp[i__ -
								       2];
      /* L10: */
    }
  return 0;
  /*use scaled form of the recurrence 
   */
 L20:
  gloglo_1.a7 /= gloglo_1.a1;
  gloglo_1.a3 /= gloglo_1.a1;
  gloglo_1.k[0] = gloglo_1.qp[0];
  gloglo_1.k[1] = gloglo_1.qp[1] - gloglo_1.a7 * gloglo_1.qp[0];
  i__1 = gloglo_1.n;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      gloglo_1.k[i__ - 1] =
	gloglo_1.a3 * gloglo_1.qk[i__ - 3] - gloglo_1.a7 * gloglo_1.qp[i__ -
								       2] +
	gloglo_1.qp[i__ - 1];
      /* L30: */
    }
  return 0;
  /*use unscaled form of the recurrence if type is 3 
   */
 L40:
  gloglo_1.k[0] = 0.;
  gloglo_1.k[1] = 0.;
  i__1 = gloglo_1.n;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      gloglo_1.k[i__ - 1] = gloglo_1.qk[i__ - 3];
      /* L50: */
    }
  return 0;
}				/* nextk_ */
