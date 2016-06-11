/* newest.f -- translated by f2c (version 19961017).
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

int nsp_ctrlpack_newest (int *type__, double *uu, double *vv)
{
  double temp, a4, a5, b1, b2, c1, c2, c3, c4;

  /*compute new estimates of quadratic coefficients 
   *using the scalars computed in calcsc. 
   *use formulas appropriate to setting of type. 
   */
  if (*type__ == 3)
    {
      goto L30;
    }
  if (*type__ == 2)
    {
      goto L10;
    }
  a4 = gloglo_1.a + gloglo_1.u * gloglo_1.b + gloglo_1.h__ * gloglo_1.f;
  a5 = gloglo_1.c__ + (gloglo_1.u + gloglo_1.v * gloglo_1.f) * gloglo_1.d__;
  goto L20;
 L10:
  a4 = (gloglo_1.a + gloglo_1.g) * gloglo_1.f + gloglo_1.h__;
  a5 = (gloglo_1.f + gloglo_1.u) * gloglo_1.c__ + gloglo_1.v * gloglo_1.d__;
  /*evaluate new quadratic coefficients. 
   */
 L20:
  b1 = -gloglo_1.k[gloglo_1.n - 1] / gloglo_1.p[gloglo_1.nn - 1];
  b2 =
    -(gloglo_1.k[gloglo_1.n - 2] +
      b1 * gloglo_1.p[gloglo_1.n - 1]) / gloglo_1.p[gloglo_1.nn - 1];
  c1 = gloglo_1.v * b2 * gloglo_1.a1;
  c2 = b1 * gloglo_1.a7;
  c3 = b1 * b1 * gloglo_1.a3;
  c4 = c1 - c2 - c3;
  temp = a5 + b1 * a4 - c4;
  if (temp == 0.)
    {
      goto L30;
    }
  *uu =
    gloglo_1.u - (gloglo_1.u * (c3 + c2) +
		  gloglo_1.v * (b1 * gloglo_1.a1 + b2 * gloglo_1.a7)) / temp;
  *vv = gloglo_1.v * (c4 / temp + 1.);
  return 0;
  /*if type=3 the quadratic is zeroed 
   */
 L30:
  *uu = 0.;
  *vv = 0.;
  return 0;
}				/* newest_ */
