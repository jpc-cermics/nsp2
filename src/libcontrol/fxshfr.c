/* fxshfr.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"


/*computes up to  l2  fixed shift k-polynomials, 
 *testing for convergence in the linear or quadratic 
 *case. initiates one of the variable shift 
 *iterations and returns with the number of zeros 
 *found. 
 *l2 - limit of fixed shift steps 
 *nz - number of zeros found 
 */

struct
{
  double p[101], qp[101], k[101], qk[101], svk[101], sr, si, u, v, a, b, c__,
    d__, a1, a2, a3, a6, a7, e, f, g, h__, szr, szi, lzr, lzi, eta, are, mre;
  int n, nn;
} gloglo_;

#define gloglo_1 gloglo_

int nsp_ctrlpack_fxshfr (int *l2, int *nz)
{
  /* System generated locals */
  int i__1, i__2;
  double d__1;

  /* Local variables */
  int type__;
  int stry, vtry;
  int i__, j, iflag;
  double s, betas, betav;
  int spass;
  int vpass;
  double ui, vi, ss, ts, tv, vv;
  double oss, ots, otv, tss, ovv, svu, svv, tvv;

  *nz = 0;
  betav = .25;
  betas = .25;
  oss = gloglo_1.sr;
  ovv = gloglo_1.v;
  /*evaluate polynomial by synthetic division 
   */
  nsp_ctrlpack_quadsd (&gloglo_1.nn, &gloglo_1.u, &gloglo_1.v, gloglo_1.p,
		       gloglo_1.qp, &gloglo_1.a, &gloglo_1.b);
  nsp_ctrlpack_calcsc (&type__);
  i__1 = *l2;
  for (j = 1; j <= i__1; ++j)
    {
      /*calculate next k polynomial and estimate v 
       */
      nsp_ctrlpack_nextk (&type__);
      nsp_ctrlpack_calcsc (&type__);
      nsp_ctrlpack_newest (&type__, &ui, &vi);
      vv = vi;
      /*estimate s 
       */
      ss = 0.;
      if (gloglo_1.k[gloglo_1.n - 1] != 0.)
	{
	  ss = -gloglo_1.p[gloglo_1.nn - 1] / gloglo_1.k[gloglo_1.n - 1];
	}
      tv = 1.;
      ts = 1.;
      if (j == 1 || type__ == 3)
	{
	  goto L70;
	}
      /*compute relative measures of convergence of s and v 
       *sequences 
       */
      if (vv != 0.)
	{
	  tv = (d__1 = (vv - ovv) / vv, Abs (d__1));
	}
      if (ss != 0.)
	{
	  ts = (d__1 = (ss - oss) / ss, Abs (d__1));
	}
      /*if decreasing, multiply two most recent 
       *convergence measures 
       */
      tvv = 1.;
      if (tv < otv)
	{
	  tvv = tv * otv;
	}
      tss = 1.;
      if (ts < ots)
	{
	  tss = ts * ots;
	}
      /*compare with convergence criteria 
       */
      vpass = tvv < betav;
      spass = tss < betas;
      if (!(spass || vpass))
	{
	  goto L70;
	}
      /*at least one sequence has passed the convergence 
       *test. store variables before iterating 
       */
      svu = gloglo_1.u;
      svv = gloglo_1.v;
      i__2 = gloglo_1.n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  gloglo_1.svk[i__ - 1] = gloglo_1.k[i__ - 1];
	  /* L10: */
	}
      s = ss;
      /*choose iteration according to the fastest 
       *converging sequence 
       */
      vtry = FALSE;
      stry = FALSE;
      if (spass && (!vpass || tss < tvv))
	{
	  goto L40;
	}
    L20:
      nsp_ctrlpack_quadit (&ui, &vi, nz);
      if (*nz > 0)
	{
	  return 0;
	}
      /*quadratic iteration has failed. flag that it has 
       *been tried and decrease the convergence criterion. 
       */
      vtry = TRUE;
      betav *= .25;
      /*try linear iteration if it has not been tried and 
       *the s sequence is converging 
       */
      if (stry || !spass)
	{
	  goto L50;
	}
      i__2 = gloglo_1.n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  gloglo_1.k[i__ - 1] = gloglo_1.svk[i__ - 1];
	  /* L30: */
	}
    L40:
      nsp_ctrlpack_realit (&s, nz, &iflag);
      if (*nz > 0)
	{
	  return 0;
	}
      /*linear iteration has failed. flag that it has been 
       *tried and decrease the convergence criterion 
       */
      stry = TRUE;
      betas *= .25;
      if (iflag == 0)
	{
	  goto L50;
	}
      /*if linear iteration signals an almost double real 
       *zero attempt quadratic interation 
       */
      ui = -(s + s);
      vi = s * s;
      goto L20;
      /*restore variables 
       */
    L50:
      gloglo_1.u = svu;
      gloglo_1.v = svv;
      i__2 = gloglo_1.n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  gloglo_1.k[i__ - 1] = gloglo_1.svk[i__ - 1];
	  /* L60: */
	}
      /*try quadratic iteration if it has not been tried 
       *and the v sequence is converging 
       */
      if (vpass && !vtry)
	{
	  goto L20;
	}
      /*recompute qp and scalar values to continue the 
       *second stage 
       */
      nsp_ctrlpack_quadsd (&gloglo_1.nn, &gloglo_1.u, &gloglo_1.v, gloglo_1.p,
			   gloglo_1.qp, &gloglo_1.a, &gloglo_1.b);
      nsp_ctrlpack_calcsc (&type__);
    L70:
      ovv = vv;
      oss = ss;
      otv = tv;
      ots = ts;
      /* L80: */
    }
  return 0;
}				/* fxshfr_ */
