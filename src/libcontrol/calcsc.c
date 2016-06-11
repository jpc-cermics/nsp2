/* calcsc.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*this routine calculates scalar quantities used to 
 *compute the next k polynomial and new estimates of 
 *the quadratic coefficients. 
 *type - int variable set here indicating how the 
 *calculations are normalized to avoid overflow 
 *synthetic division of k by the quadratic 1,u,v 
 */

struct
{
  double p[101], qp[101], k[101], qk[101], svk[101], sr, si, u, v, a, b, c__,
    d__, a1, a2, a3, a6, a7, e, f, g, h__, szr, szi, lzr, lzi, eta, are, mre;
  int n, nn;
} gloglo_;

#define gloglo_1 gloglo_

int nsp_ctrlpack_calcsc (int *type__)
{
  double d__1;

  nsp_ctrlpack_quadsd (&gloglo_1.n, &gloglo_1.u, &gloglo_1.v, gloglo_1.k,
		       gloglo_1.qk, &gloglo_1.c__, &gloglo_1.d__);
  if (Abs (gloglo_1.c__) >
      (d__1 = gloglo_1.k[gloglo_1.n - 1], Abs (d__1)) * 100. * gloglo_1.eta)
    {
      goto L10;
    }
  if (Abs (gloglo_1.d__) >
      (d__1 = gloglo_1.k[gloglo_1.n - 2], Abs (d__1)) * 100. * gloglo_1.eta)
    {
      goto L10;
    }
  *type__ = 3;
  /*type=3 indicates the quadratic is almost a factor 
   *of k 
   */
  return 0;
 L10:
  if (Abs (gloglo_1.d__) < Abs (gloglo_1.c__))
    {
      goto L20;
    }
  *type__ = 2;
  /*type=2 indicates that all formulas are divided by d 
   */
  gloglo_1.e = gloglo_1.a / gloglo_1.d__;
  gloglo_1.f = gloglo_1.c__ / gloglo_1.d__;
  gloglo_1.g = gloglo_1.u * gloglo_1.b;
  gloglo_1.h__ = gloglo_1.v * gloglo_1.b;
  gloglo_1.a3 =
    (gloglo_1.a + gloglo_1.g) * gloglo_1.e +
    gloglo_1.h__ * (gloglo_1.b / gloglo_1.d__);
  gloglo_1.a1 = gloglo_1.b * gloglo_1.f - gloglo_1.a;
  gloglo_1.a7 = (gloglo_1.f + gloglo_1.u) * gloglo_1.a + gloglo_1.h__;
  return 0;
 L20:
  *type__ = 1;
  /*type=1 indicates that all formulas are divided by c 
   */
  gloglo_1.e = gloglo_1.a / gloglo_1.c__;
  gloglo_1.f = gloglo_1.d__ / gloglo_1.c__;
  gloglo_1.g = gloglo_1.u * gloglo_1.e;
  gloglo_1.h__ = gloglo_1.v * gloglo_1.b;
  gloglo_1.a3 =
    gloglo_1.a * gloglo_1.e + (gloglo_1.h__ / gloglo_1.c__ +
			       gloglo_1.g) * gloglo_1.b;
  gloglo_1.a1 = gloglo_1.b - gloglo_1.a * (gloglo_1.d__ / gloglo_1.c__);
  gloglo_1.a7 =
    gloglo_1.a + gloglo_1.g * gloglo_1.d__ + gloglo_1.h__ * gloglo_1.f;
  return 0;
}

