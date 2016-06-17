/* quadit.f -- translated by f2c (version 19961017).  */

#include "ctrlpack.h"

struct
{
  double p[101], qp[101], k[101], qk[101], svk[101], sr, si, u, v, a, b, c__,
    d__, a1, a2, a3, a6, a7, e, f, g, h__, szr, szi, lzr, lzi, eta, are, mre;
  int n, nn;
} gloglo_;

#define gloglo_1 gloglo_

static double c_b3 = 1.;

  /*variable-shift k-polynomial iteration for a 
   *quadratic factor converges only if the zeros are 
   *equimodular or nearly so. 
   *uu,vv - coefficients of starting quadratic 
   *nz - number of zero found 
   */

int nsp_ctrlpack_quadit (double *uu, double *vv, int *nz)
{
  int i__1;
  double d__1, d__2;
  int type__, i__, j;
  double t;
  int tried;
  double ee;
  double ui, vi, mp, zm;
  double relstp = 0, omp = 0 ;

  *nz = 0;
  tried = FALSE;
  gloglo_1.u = *uu;
  gloglo_1.v = *vv;
  j = 0;
  /*main loop 
   */
 L10:
  nsp_ctrlpack_quad (&c_b3, &gloglo_1.u, &gloglo_1.v, &gloglo_1.szr,
		     &gloglo_1.szi, &gloglo_1.lzr, &gloglo_1.lzi);
  /*return if roots of the quadratic are real and not 
   *close to multiple or nearly equal and  of opposite 
   *sign 
   */
  if ((d__1 =
       Abs (gloglo_1.szr) - Abs (gloglo_1.lzr),
       Abs (d__1)) > Abs (gloglo_1.lzr) * .01)
    {
      return 0;
    }
  /*evaluate polynomial by quadratic synthetic division 
   */
  nsp_ctrlpack_quadsd (&gloglo_1.nn, &gloglo_1.u, &gloglo_1.v, gloglo_1.p,
		       gloglo_1.qp, &gloglo_1.a, &gloglo_1.b);
  mp = (d__1 = gloglo_1.a - gloglo_1.szr * gloglo_1.b, Abs (d__1)) + (d__2 =
								      gloglo_1.szi
								      *
								      gloglo_1.b,
								      Abs
								      (d__2));
  /*compute a rigorous  bound on the rounding error in 
   *evaluting p 
   */
  zm = sqrt ((Abs (gloglo_1.v)));
  ee = Abs (gloglo_1.qp[0]) * 2.;
  t = -gloglo_1.szr * gloglo_1.b;
  i__1 = gloglo_1.n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      ee = ee * zm + (d__1 = gloglo_1.qp[i__ - 1], Abs (d__1));
      /* L20: */
    }
  ee = ee * zm + (d__1 = gloglo_1.a + t, Abs (d__1));
  ee =
    (gloglo_1.mre * 5. + gloglo_1.are * 4.) * ee - (gloglo_1.mre * 5. +
						    gloglo_1.are * 2.) *
    ((d__1 =
      gloglo_1.a + t,
      Abs (d__1)) + Abs (gloglo_1.b) * zm) + gloglo_1.are * 2. * Abs (t);
  /*iteration has converged sufficienty if the 
   *polynomial value is less than 20 times this bound 
   */
  if (mp > ee * 20.)
    {
      goto L30;
    }
  *nz = 2;
  return 0;
 L30:
  ++j;
  /*stop iteration after 20 steps 
   */
  if (j > 20)
    {
      return 0;
    }
  if (j < 2)
    {
      goto L50;
    }
  if (relstp > .01 || mp < omp || tried)
    {
      goto L50;
    }
  /*a cluster appears to be stalling the convergence. 
   *five fixed shift steps are taken with a u,v close 
   *to the cluster 
   */
  if (relstp < gloglo_1.eta)
    {
      relstp = gloglo_1.eta;
    }
  relstp = sqrt (relstp);
  gloglo_1.u -= gloglo_1.u * relstp;
  gloglo_1.v += gloglo_1.v * relstp;
  nsp_ctrlpack_quadsd (&gloglo_1.nn, &gloglo_1.u, &gloglo_1.v, gloglo_1.p,
		       gloglo_1.qp, &gloglo_1.a, &gloglo_1.b);
  for (i__ = 1; i__ <= 5; ++i__)
    {
      nsp_ctrlpack_calcsc (&type__);
      nsp_ctrlpack_nextk (&type__);
      /* L40: */
    }
  tried = TRUE;
  j = 0;
 L50:
  omp = mp;
  /*calculate next k polynomial and new u and v 
   */
  nsp_ctrlpack_calcsc (&type__);
  nsp_ctrlpack_nextk (&type__);
  nsp_ctrlpack_calcsc (&type__);
  nsp_ctrlpack_newest (&type__, &ui, &vi);
  /*if vi is zero the iteration is not converging 
   */
  if (vi == 0.)
    {
      return 0;
    }
  relstp = (d__1 = (vi - gloglo_1.v) / vi, Abs (d__1));
  gloglo_1.u = ui;
  gloglo_1.v = vi;
  goto L10;
}				/* quadit_ */
