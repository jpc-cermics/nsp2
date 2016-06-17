/* realit.f -- translated by f2c (version 19961017).
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

/*variable-shift h polynomial iteration for a real 
 *zero. 
 *sss   - starting iterate 
 *nz    - number of zero found 
 *iflag - flag to indicate a pair of zero near real 
 *        axis. 
 */

int nsp_ctrlpack_realit (double *sss, int *nz, int *iflag)
{
  int i__1;
  double d__1;
  int i__, j;
  double s, t =0 , ee, mp, ms, kv, pv;
  double omp = 0;
  /* nm1 = gloglo_1.n - 1; */
  *nz = 0;
  s = *sss;
  *iflag = 0;
  j = 0;
  /*main loop 
   */
 L10:
  pv = gloglo_1.p[0];
  /*evaluate p at s 
   */
  gloglo_1.qp[0] = pv;
  i__1 = gloglo_1.nn;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      pv = pv * s + gloglo_1.p[i__ - 1];
      gloglo_1.qp[i__ - 1] = pv;
      /* L20: */
    }
  mp = Abs (pv);
  /*compute a rigorous bound on the error in evaluating 
   *p 
   */
  ms = Abs (s);
  ee = gloglo_1.mre / (gloglo_1.are + gloglo_1.mre) * Abs (gloglo_1.qp[0]);
  i__1 = gloglo_1.nn;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      ee = ee * ms + (d__1 = gloglo_1.qp[i__ - 1], Abs (d__1));
      /* L30: */
    }
  /*iteration has converges sufficiently if the 
   *polynomial value is less yhan 20 times this bound 
   */
  if (mp > ((gloglo_1.are + gloglo_1.mre) * ee - gloglo_1.mre * mp) * 20.)
    {
      goto L40;
    }
  *nz = 1;
  gloglo_1.szr = s;
  gloglo_1.szi = 0.;
  return 0;
 L40:
  ++j;
  /*stop iteration after 10 steps 
   */
  if (j > 10)
    {
      return 0;
    }
  if (j < 2)
    {
      goto L50;
    }
  if (Abs (t) > (d__1 = s - t, Abs (d__1)) * .001 || mp <= omp)
    {
      goto L50;
    }
  /*a cluster of zeros near the real axis has been 
   *encountered return with iflag set to initiate a 
   *quadratic iteration 
   */
  *iflag = 1;
  *sss = s;
  return 0;
  /*return if the polynomial value has increased 
   *significantly 
   */
 L50:
  omp = mp;
  /*compute t, the next polynomial, and the new iterate 
   */
  kv = gloglo_1.k[0];
  gloglo_1.qk[0] = kv;
  i__1 = gloglo_1.n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      kv = kv * s + gloglo_1.k[i__ - 1];
      gloglo_1.qk[i__ - 1] = kv;
      /* L60: */
    }
  if (Abs (kv) <=
      (d__1 = gloglo_1.k[gloglo_1.n - 1], Abs (d__1)) * 10. * gloglo_1.eta)
    {
      goto L80;
    }
  /*use the scaled form of the recurrence if the value 
   *of k at s is nonzero 
   */
  t = -pv / kv;
  gloglo_1.k[0] = gloglo_1.qp[0];
  i__1 = gloglo_1.n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      gloglo_1.k[i__ - 1] = t * gloglo_1.qk[i__ - 2] + gloglo_1.qp[i__ - 1];
      /* L70: */
    }
  goto L100;
  /*use unscaled form 
   */
 L80:
  gloglo_1.k[0] = 0.;
  i__1 = gloglo_1.n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      gloglo_1.k[i__ - 1] = gloglo_1.qk[i__ - 2];
      /* L90: */
    }
 L100:
  kv = gloglo_1.k[0];
  i__1 = gloglo_1.n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      kv = kv * s + gloglo_1.k[i__ - 1];
      /* L110: */
    }
  t = 0.;
  if (Abs (kv) >
      (d__1 = gloglo_1.k[gloglo_1.n - 1], Abs (d__1)) * 10. * gloglo_1.eta)
    {
      t = -pv / kv;
    }
  s += t;
  goto L10;
}				/* realit_ */
