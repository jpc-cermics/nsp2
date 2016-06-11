/* rpoly.f -- translated by f2c (version 19961017).
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

/* 
 * finds the zeros of a real polynomial 
 * 
 * op  - double precision vector of coefficients in 
 *       order of decreasing powers. 
 * degree   - int degree of polynomial. 
 * zeror, zeroi - output double precision vectors of 
 *                real and imaginary parts of the 
 *                zeros. 
 * fail  - output parameter, 
 *        2 if  leading coefficient is zero 
 *        1 for non convergence or if rpoly 
 *         has found fewer than degree zeros. 
 *         in the latter case degree is reset to 
 *         the number of zeros found. 
 *        3 if degree>100 
 * Remark:
 * 
 * to change the size of polynomials which can be 
 * solved, reset the dimensions of the arrays in the 
 * common area and in the following declarations. 
 * the subroutine uses single precision calculations 
 * for scaling, bounds and error calculations. all 
 * calculations for the iterations are done in double 
 * precision. 
 */

int nsp_ctrlpack_rpoly (const double *op, int *degree, double *zeror, double *zeroi, int *fail)
{
  int i__1;
  double d__1;

  double base;
  double mini, maxi, temp[101], cosr, sinr;
  int i__, j, l;
  double t, x, infin;
  int zerok;
  double aa, bb, cc, df, ff;
  int jj;
  double sc;
  double lo, dx, xm;
  int nz;
  double factor, xx, yy, smalno;
  int nm1;
  double bnd;
  int cnt;
  double ptt[101], xxx;

  /* Parameter adjustments */
  --zeroi;
  --zeror;
  --op;

  /* Function Body */
  if (*degree > 100)
    {
      goto L300;
    }
  /*the following statements set machine constants used 
   *in various parts of the program. the meaning of the 
   *four constants are... 
   *eta     the maximum relative representation error 
   *        which can be described as the smallest 
   *        positive floating point number such that 
   *        1.do+eta is greater than 1. 
   *infiny  the largest floating-point number. 
   *smalno  the smallest positive floating-point number 
   *        if the exponent range differs in single and 
   *        double precision then smalno and infin 
   *        should indicate the smaller range. 
   *base    the base of the floating-point number 
   *        system used. 
   */
  smalno = C2F (slamch) ("u", 1L);
  infin = C2F (slamch) ("o", 1L);
  base = C2F (slamch) ("b", 1L);
  gloglo_1.eta = nsp_dlamch ("p");
  /*are and mre refer to the unit error in + and * 
   *respectively. they are assumed to be the same as 
   *eta. 
   */
  gloglo_1.are = gloglo_1.eta;
  gloglo_1.mre = gloglo_1.eta;
  lo = smalno / gloglo_1.eta;
  /*initialization of constants for shift rotation 
   */
  xx = .70710678;
  yy = -xx;
  cosr = -.069756474;
  sinr = .99756405;
  *fail = 0;
  gloglo_1.n = *degree;
  gloglo_1.nn = gloglo_1.n + 1;
  /*algorithm fails if the leading coefficient is zero. 
   */
  if (op[1] != 0.)
    {
      goto L10;
    }
  *fail = 2;
  *degree = 0;
  return 0;
  /*make a copy of the coefficients 
   */
 L10:
  i__1 = gloglo_1.nn;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      gloglo_1.p[i__ - 1] = op[i__];
      /* L20: */
    }
  /*remove the zeros at the origin if any 
   */
 L30:
  if (gloglo_1.p[gloglo_1.nn - 1] != 0.)
    {
      goto L40;
    }
  j = *degree - gloglo_1.n + 1;
  zeror[j] = 0.;
  zeroi[j] = 0.;
  --gloglo_1.nn;
  --gloglo_1.n;
  goto L30;
  /*start the algorithm for one zero 
   */
 L40:
  if (gloglo_1.n > 2)
    {
      goto L60;
    }
  if (gloglo_1.n < 1)
    {
      return 0;
    }
  /*calculate the final zero or pair zeros 
   */
  if (gloglo_1.n == 2)
    {
      goto L50;
    }
  zeror[*degree] = -gloglo_1.p[1] / gloglo_1.p[0];
  zeroi[*degree] = 0.;
  return 0;
 L50:
  nsp_ctrlpack_quad (gloglo_1.p, &gloglo_1.p[1], &gloglo_1.p[2],
		     &zeror[*degree - 1], &zeroi[*degree - 1],
		     &zeror[*degree], &zeroi[*degree]);
  return 0;
  /*find largest and smallest moduli of coefficients. 
   */
 L60:
  maxi = 0.;
  mini = infin;
  i__1 = gloglo_1.nn;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      x = (d__1 = gloglo_1.p[i__ - 1], Abs (d__1));
      if (x > maxi)
	{
	  maxi = x;
	}
      if (x != 0. && x < mini)
	{
	  mini = x;
	}
      /* L70: */
    }
  /*     maxi=min(infin,maxi) bug "f77 -mieee-with-inexact" 
   */
  if (infin < maxi)
    {
      maxi = infin;
    }
  /*scale if there are large or very small coefficients 
   *computes a scale factor to multiply the 
   *coefficients of the polynomial. the scaling is done 
   *to avoid overflow and to avoid undetected underflow 
   *interfering with the convergence criterion. 
   *the factor is a power of the base 
   */
  sc = lo / mini;
  if (sc > 1.)
    {
      goto L80;
    }
  if (maxi < 10.)
    {
      goto L110;
    }
  if (sc == 0.)
    {
      sc = smalno;
    }
  goto L90;
 L80:
  if (infin / sc < maxi)
    {
      goto L110;
    }
 L90:
  l = (int) (log (sc) / log (base) + .5);
  d__1 = base * 1.;
  factor = nsp_pow_di (d__1, l);
  if (factor == 1.)
    {
      goto L110;
    }
  i__1 = gloglo_1.nn;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      gloglo_1.p[i__ - 1] = factor * gloglo_1.p[i__ - 1];
      /* L100: */
    }
  /*compute lower bound on moduli of zeros. 
   */
 L110:
  i__1 = gloglo_1.nn;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /*       ptt(i) = Min(infin,abs(real(p(i)))) bug "f77 -mieee-with-inexact" 
       */
      ptt[i__ - 1] = (d__1 = gloglo_1.p[i__ - 1], Abs (d__1));
      if (infin < (d__1 = gloglo_1.p[i__ - 1], Abs (d__1)))
	{
	  ptt[i__ - 1] = infin;
	}
      /* L120: */
    }
  ptt[gloglo_1.nn - 1] = -ptt[gloglo_1.nn - 1];
  /*compute upper estimate of bound 
   */
  x =
    exp ((log (-ptt[gloglo_1.nn - 1]) - log (ptt[0])) / (double) gloglo_1.n);
  if (ptt[gloglo_1.n - 1] == 0.)
    {
      goto L130;
    }
  /*if newton step at the origin is better, use it. 
   */
  xm = -ptt[gloglo_1.nn - 1] / ptt[gloglo_1.n - 1];
  if (xm < x)
    {
      x = xm;
    }
  /*chop the interval (0,x) until ff .le. 0 
   */
 L130:
  xm = x * .1;
  ff = ptt[0];
  i__1 = gloglo_1.nn;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      ff = ff * xm + ptt[i__ - 1];
      /* L140: */
    }
  if (ff <= 0.)
    {
      goto L150;
    }
  if (ff > infin)
    {
      goto L310;
    }
  x = xm;
  goto L130;
 L150:
  dx = x;
  /*do newton iteration until x converges to two 
   *decimal places 
   */
 L160:
  if ((d__1 = dx / x, Abs (d__1)) <= .005)
    {
      goto L180;
    }
  ff = ptt[0];
  df = ff;
  i__1 = gloglo_1.n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      ff = ff * x + ptt[i__ - 1];
      df = df * x + ff;
      /* L170: */
    }
  ff = ff * x + ptt[gloglo_1.nn - 1];
  if (ff > infin)
    {
      goto L310;
    }
  dx = ff / df;
  x -= dx;
  goto L160;
 L180:
  bnd = x;
  /*compute the derivative as the intial k polynomial 
   *and do 5 steps with no shift 
   */
  nm1 = gloglo_1.n - 1;
  i__1 = gloglo_1.n;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      gloglo_1.k[i__ - 1] =
	(double) (gloglo_1.nn - i__) * gloglo_1.p[i__ -
						  1] / (double) gloglo_1.n;
      /* L190: */
    }
  gloglo_1.k[0] = gloglo_1.p[0];
  aa = gloglo_1.p[gloglo_1.nn - 1];
  bb = gloglo_1.p[gloglo_1.n - 1];
  zerok = gloglo_1.k[gloglo_1.n - 1] == 0.;
  for (jj = 1; jj <= 5; ++jj)
    {
      cc = gloglo_1.k[gloglo_1.n - 1];
      if (zerok)
	{
	  goto L210;
	}
      /*use scaled form of recurrence if value of k at 0 is 
       *nonzero 
       */
      t = -aa / cc;
      i__1 = nm1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  j = gloglo_1.nn - i__;
	  gloglo_1.k[j - 1] = t * gloglo_1.k[j - 2] + gloglo_1.p[j - 1];
	  /* L200: */
	}
      gloglo_1.k[0] = gloglo_1.p[0];
      zerok = (d__1 =
	       gloglo_1.k[gloglo_1.n - 1],
	       Abs (d__1)) <= Abs (bb) * gloglo_1.eta * 10.;
      goto L230;
      /*use unscaled form form of recurrence 
       */
    L210:
      i__1 = nm1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  j = gloglo_1.nn - i__;
	  gloglo_1.k[j - 1] = gloglo_1.k[j - 2];
	  /* L220: */
	}
      gloglo_1.k[0] = 0.;
      zerok = gloglo_1.k[gloglo_1.n - 1] == 0.;
    L230:
      ;
    }
  /*save k for restarts with new shifts 
   */
  i__1 = gloglo_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      temp[i__ - 1] = gloglo_1.k[i__ - 1];
      /* L240: */
    }
  /*loop to select the quadratic  corresponding to each 
   *new shift 
   */
  for (cnt = 1; cnt <= 20; ++cnt)
    {
      /*quadratic corresponds to a double shift to a 
       *non-real point and its complex conjugate. the point 
       *has modulus bnd and amplitude rotated by 94 degrees 
       *from the previous shift 
       */
      xxx = cosr * xx - sinr * yy;
      yy = sinr * xx + cosr * yy;
      xx = xxx;
      gloglo_1.sr = bnd * xx;
      gloglo_1.si = bnd * yy;
      gloglo_1.u = gloglo_1.sr * -2.;
      gloglo_1.v = bnd;
      /*second stage calculation, fixed quadratic 
       */
      i__1 = cnt * 20;
      nsp_ctrlpack_fxshfr (&i__1, &nz);
      if (nz == 0)
	{
	  goto L260;
	}
      /*the second stage jumps directly to one of the third 
       *stage iterations and returns here if successful. 
       *deflate the polynomial, store the zero or zeros and 
       *return to the main algorithm. 
       */
      j = *degree - gloglo_1.n + 1;
      zeror[j] = gloglo_1.szr;
      zeroi[j] = gloglo_1.szi;
      gloglo_1.nn -= nz;
      gloglo_1.n = gloglo_1.nn - 1;
      i__1 = gloglo_1.nn;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  gloglo_1.p[i__ - 1] = gloglo_1.qp[i__ - 1];
	  /* L250: */
	}
      if (nz == 1)
	{
	  goto L40;
	}
      zeror[j + 1] = gloglo_1.lzr;
      zeroi[j + 1] = gloglo_1.lzi;
      goto L40;
      /*if the iteration is unsuccessful another quadratic 
       *is chosen after restoring k 
       */
    L260:
      i__1 = gloglo_1.n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  gloglo_1.k[i__ - 1] = temp[i__ - 1];
	  /* L270: */
	}
      /* L280: */
    }
  /*return with failure if no convergence with 20 
   *shifts 
   */
  *fail = 1;
  *degree -= gloglo_1.n;
  return 0;
 L300:
  *fail = 3;
  return 0;
 L310:
  *fail = 1;
  return 0;
}				/* rpoly_ */
