

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <stdint.h>

#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/matint.h> 

#include "nsp/cnumeric.h"
#include "nsp/matutil.h"
#include "nsp/gsort-p.h"
#include "nsp/nsp_lapack.h"
#include "nsp/lapack-c.h"
#include "../librand/grand.h"

#include <nsp/blas.h>
#include <nsp/matutil.h>
#include "fftpack.h"

/* 
 *    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * 
 *                      VERSION 4  APRIL 1985 
 * 
 *                        A TEST DRIVER FOR 
 *         A PACKAGE OF FORTRAN SUBPROGRAMS FOR THE FAST FOURIER 
 *          TRANSFORM OF PERIODIC AND OTHER SYMMETRIC SEQUENCES 
 * 
 *                             BY 
 * 
 *                      PAUL N SWARZTRAUBER 
 * 
 *      NATIONAL CENTER FOR ATMOSPHERIC RESEARCH  BOULDER,COLORADO 80307 
 * 
 *       WHICH IS SPONSORED BY THE NATIONAL SCIENCE FOUNDATION 
 * 
 *    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 * 
 * 
 *            THIS PROGRAM TESTS THE PACKAGE OF FAST FOURIER 
 *    TRANSFORMS FOR BOTH COMPLEX AND REAL PERIODIC SEQUENCES AND 
 *    CERTIAN OTHER SYMMETRIC SEQUENCES THAT ARE LISTED BELOW. 
 * 
 *    1.   RFFTI     INITIALIZE  RFFTF AND RFFTB 
 *    2.   RFFTF     FORWARD TRANSFORM OF A REAL PERIODIC SEQUENCE 
 *    3.   RFFTB     BACKWARD TRANSFORM OF A REAL COEFFICIENT ARRAY 
 * 
 *    4.   EZFFTI    INITIALIZE EZFFTF AND EZFFTB 
 *    5.   EZFFTF    A SIMPLIFIED REAL PERIODIC FORWARD TRANSFORM 
 *    6.   EZFFTB    A SIMPLIFIED REAL PERIODIC BACKWARD TRANSFORM 
 * 
 *    7.   SINTI     INITIALIZE SINT 
 *    8.   SINT      SINE TRANSFORM OF A REAL ODD SEQUENCE 
 * 
 *    9.   COSTI     INITIALIZE COST 
 *    10.  COST      COSINE TRANSFORM OF A REAL EVEN SEQUENCE 
 * 
 *    11.  SINQI     INITIALIZE SINQF AND SINQB 
 *    12.  SINQF     FORWARD SINE TRANSFORM WITH ODD WAVE NUMBERS 
 *    13.  SINQB     UNNORMALIZED INVERSE OF SINQF 
 * 
 *    14.  COSQI     INITIALIZE COSQF AND COSQB 
 *    15.  COSQF     FORWARD COSINE TRANSFORM WITH ODD WAVE NUMBERS 
 *    16.  COSQB     UNNORMALIZED INVERSE OF COSQF 
 * 
 *    17.  CFFTI     INITIALIZE CFFTF AND CFFTB 
 *    18.  CFFTF     FORWARD TRANSFORM OF A COMPLEX PERIODIC SEQUENCE 
 *    19.  CFFTB     UNNORMALIZED INVERSE OF CFFTF 
 *    *** HACKED BY HCP FOR THE DOUBLE PREC. VERSION NOVEMEMBER 1999 
 * 
 */

static int fftpack_dsinqb (int *n, double *x, double *wsave);
static int fftpack_dsinqf (int *n, double *x, double *wsave);
static int fftpack_dsinqi (int *n, double *wsave);
static int fftpack_dsint (int *n, double *x, double *wsave);
static int fftpack_dsinti (int *n, double *wsave);
static int fftpack_dcosqb (int *n, double *x, double *wsave);
static int fftpack_dcosqf (int *n, double *x, double *wsave);
static int fftpack_dcosqi (int *n, double *wsave);
static int fftpack_dcost (int *n, double *x, double *wsave);
static int fftpack_dcosti (int *n, double *wsave);

/* Table of constant values */

static int c_n1 = -1;

int test_fftpack (void)
{
  /* Initialized data */

  static int nd[10] = { 120, 54, 49, 32, 4, 3, 2 };

  int i__1, i__2, i__3, i__4, i__5, i__6;
  double d__1, d__2, d__3, d__4;
  doubleC z__1, z__2, z__3;
  int modn;
  double rftb, rftf, dezb1, dezf1, a[100], b[100];
  int i__, j;
  double sqrt2;
  int k, n;
  double w[2000], x[200], y[200];
  double dezfb;
  double rftfb;
  double azero;
  double costt, sintt, ah[100], bh[100], cf, fn, dt, pi, dcfftb;
  doubleC cx[200], cy[200];
  double dcfftf, xh[200];
  int nz;
  double cosqfb;
  double costfb;
  double sinqfb;
  double sintfb;
  double cosqbt, azeroh;
  double cosqft, sinqbt, sinqft;
  int nm1, np1, ns2;
  double arg, tpi;
  int nns;
  double sum, arg1, arg2;
  int ns2m;
  double sum1, sum2, dcfb;

  sqrt2 = sqrt (2.);
  nns = 7;
  i__1 = nns;
  for (nz = 1; nz <= i__1; ++nz)
    {
      n = nd[nz - 1];
      modn = n % 2;
      fn = (double) n;
      /* tfn = fn + fn; */
      np1 = n + 1;
      nm1 = n - 1;
      i__2 = np1;
      for (j = 1; j <= i__2; ++j)
	{
	  x[j - 1] = sin ((double) j * sqrt2);
	  y[j - 1] = x[j - 1];
	  xh[j - 1] = x[j - 1];
	  /* L101: */
	}
      /* 
       *    TEST SUBROUTINES RFFTI,RFFTF AND RFFTB 
       * 
       */
      fftpack_dffti (&n, w);
      pi = 3.14159265358979323846;
      dt = (pi + pi) / fn;
      ns2 = (n + 1) / 2;
      if (ns2 < 2)
	{
	  goto L104;
	}
      i__2 = ns2;
      for (k = 2; k <= i__2; ++k)
	{
	  sum1 = 0.;
	  sum2 = 0.;
	  arg = (double) (k - 1) * dt;
	  i__3 = n;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      arg1 = (double) (i__ - 1) * arg;
	      sum1 += x[i__ - 1] * cos (arg1);
	      sum2 += x[i__ - 1] * sin (arg1);
	      /* L102: */
	    }
	  y[(k << 1) - 3] = sum1;
	  y[(k << 1) - 2] = -sum2;
	  /* L103: */
	}
    L104:
      sum1 = 0.;
      sum2 = 0.;
      i__2 = nm1;
      for (i__ = 1; i__ <= i__2; i__ += 2)
	{
	  sum1 += x[i__ - 1];
	  sum2 += x[i__];
	  /* L105: */
	}
      if (modn == 1)
	{
	  sum1 += x[n - 1];
	}
      y[0] = sum1 + sum2;
      if (modn == 0)
	{
	  y[n - 1] = sum1 - sum2;
	}
      fftpack_dfftf (&n, x, w);
      rftf = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = rftf, d__3 = (d__1 = x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  rftf = Max (d__2, d__3);
	  x[i__ - 1] = xh[i__ - 1];
	  /* L106: */
	}
      rftf /= fn;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  sum = x[0] * .5;
	  arg = (double) (i__ - 1) * dt;
	  if (ns2 < 2)
	    {
	      goto L108;
	    }
	  i__3 = ns2;
	  for (k = 2; k <= i__3; ++k)
	    {
	      arg1 = (double) (k - 1) * arg;
	      sum =
		sum + x[(k << 1) - 3] * cos (arg1) - x[(k << 1) -
						       2] * sin (arg1);
	      /* L107: */
	    }
	L108:
	  if (modn == 0)
	    {
	      i__3 = i__ - 1;
	      sum += (double) nsp_pow_ii (c_n1, i__3) * .5 * x[n - 1];
	    }
	  y[i__ - 1] = sum + sum;
	  /* L109: */
	}
      fftpack_dfftb (&n, x, w);
      rftb = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = rftb, d__3 = (d__1 = x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  rftb = Max (d__2, d__3);
	  x[i__ - 1] = xh[i__ - 1];
	  y[i__ - 1] = xh[i__ - 1];
	  /* L110: */
	}
      fftpack_dfftb (&n, y, w);
      fftpack_dfftf (&n, y, w);
      cf = 1. / fn;
      rftfb = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = rftfb, d__3 = (d__1 =
				cf * y[i__ - 1] - x[i__ - 1], Abs (d__1));
	  rftfb = Max (d__2, d__3);
	  /* L111: */
	}
      /* 
       *    TEST SUBROUTINES DSINTI AND DSINT 
       * 
       */
      dt = pi / fn;
      i__2 = nm1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  x[i__ - 1] = xh[i__ - 1];
	  /* L112: */
	}
      i__2 = nm1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  y[i__ - 1] = 0.;
	  arg1 = (double) i__ *dt;
	  i__3 = nm1;
	  for (k = 1; k <= i__3; ++k)
	    {
	      y[i__ - 1] += x[k - 1] * sin ((double) k * arg1);
	      /* L113: */
	    }
	  y[i__ - 1] += y[i__ - 1];
	  /* L114: */
	}
      fftpack_dsinti (&nm1, w);
      fftpack_dsint (&nm1, x, w);
      cf = .5 / fn;
      sintt = 0.;
      i__2 = nm1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = sintt, d__3 = (d__1 = x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  sintt = Max (d__2, d__3);
	  x[i__ - 1] = xh[i__ - 1];
	  y[i__ - 1] = x[i__ - 1];
	  /* L115: */
	}
      sintt = cf * sintt;
      fftpack_dsint (&nm1, x, w);
      fftpack_dsint (&nm1, x, w);
      sintfb = 0.;
      i__2 = nm1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = sintfb, d__3 = (d__1 =
				 cf * x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  sintfb = Max (d__2, d__3);
	  /* L116: */
	}
      /* 
       *    TEST SUBROUTINES COSTI AND COST 
       * 
       */
      i__2 = np1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  x[i__ - 1] = xh[i__ - 1];
	  /* L117: */
	}
      i__2 = np1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  i__3 = i__ + 1;
	  y[i__ - 1] = (x[0] + (double) nsp_pow_ii (c_n1, i__3) * x[n]) * .5;
	  arg = (double) (i__ - 1) * dt;
	  i__3 = n;
	  for (k = 2; k <= i__3; ++k)
	    {
	      y[i__ - 1] += x[k - 1] * cos ((double) (k - 1) * arg);
	      /* L118: */
	    }
	  y[i__ - 1] += y[i__ - 1];
	  /* L119: */
	}
      fftpack_dcosti (&np1, w);
      fftpack_dcost (&np1, x, w);
      costt = 0.;
      i__2 = np1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = costt, d__3 = (d__1 = x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  costt = Max (d__2, d__3);
	  x[i__ - 1] = xh[i__ - 1];
	  y[i__ - 1] = xh[i__ - 1];
	  /* L120: */
	}
      costt = cf * costt;
      fftpack_dcost (&np1, x, w);
      fftpack_dcost (&np1, x, w);
      costfb = 0.;
      i__2 = np1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = costfb, d__3 = (d__1 =
				 cf * x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  costfb = Max (d__2, d__3);
	  /* L121: */
	}
      /* 
       *    TEST SUBROUTINES SINQI,SINQF AND SINQB 
       * 
       */
      cf = .25 / fn;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  y[i__ - 1] = xh[i__ - 1];
	  /* L122: */
	}
      dt = pi / (fn + fn);
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  x[i__ - 1] = 0.;
	  arg = dt * (double) i__;
	  i__3 = n;
	  for (k = 1; k <= i__3; ++k)
	    {
	      x[i__ - 1] += y[k - 1] * sin ((double) (k + k - 1) * arg);
	      /* L123: */
	    }
	  x[i__ - 1] *= 4.;
	  /* L124: */
	}
      fftpack_dsinqi (&n, w);
      fftpack_dsinqb (&n, y, w);
      sinqbt = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = sinqbt, d__3 = (d__1 = y[i__ - 1] - x[i__ - 1], Abs (d__1));
	  sinqbt = Max (d__2, d__3);
	  x[i__ - 1] = xh[i__ - 1];
	  /* L125: */
	}
      sinqbt = cf * sinqbt;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  arg = (double) (i__ + i__ - 1) * dt;
	  i__3 = i__ + 1;
	  y[i__ - 1] = (double) nsp_pow_ii (c_n1, i__3) * .5 * x[n - 1];
	  i__3 = nm1;
	  for (k = 1; k <= i__3; ++k)
	    {
	      y[i__ - 1] += x[k - 1] * sin ((double) k * arg);
	      /* L126: */
	    }
	  y[i__ - 1] += y[i__ - 1];
	  /* L127: */
	}
      fftpack_dsinqf (&n, x, w);
      sinqft = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = sinqft, d__3 = (d__1 = x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  sinqft = Max (d__2, d__3);
	  y[i__ - 1] = xh[i__ - 1];
	  x[i__ - 1] = xh[i__ - 1];
	  /* L128: */
	}
      fftpack_dsinqf (&n, y, w);
      fftpack_dsinqb (&n, y, w);
      sinqfb = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = sinqfb, d__3 = (d__1 =
				 cf * y[i__ - 1] - x[i__ - 1], Abs (d__1));
	  sinqfb = Max (d__2, d__3);
	  /* L129: */
	}
      /* 
       *    TEST SUBROUTINES COSQI,COSQF AND COSQB 
       * 
       */
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  y[i__ - 1] = xh[i__ - 1];
	  /* L130: */
	}
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  x[i__ - 1] = 0.;
	  arg = (double) (i__ - 1) * dt;
	  i__3 = n;
	  for (k = 1; k <= i__3; ++k)
	    {
	      x[i__ - 1] += y[k - 1] * cos ((double) (k + k - 1) * arg);
	      /* L131: */
	    }
	  x[i__ - 1] *= 4.;
	  /* L132: */
	}
      fftpack_dcosqi (&n, w);
      fftpack_dcosqb (&n, y, w);
      cosqbt = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = cosqbt, d__3 = (d__1 = x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  cosqbt = Max (d__2, d__3);
	  x[i__ - 1] = xh[i__ - 1];
	  /* L133: */
	}
      cosqbt = cf * cosqbt;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  y[i__ - 1] = x[0] * .5;
	  arg = (double) (i__ + i__ - 1) * dt;
	  i__3 = n;
	  for (k = 2; k <= i__3; ++k)
	    {
	      y[i__ - 1] += x[k - 1] * cos ((double) (k - 1) * arg);
	      /* L134: */
	    }
	  y[i__ - 1] += y[i__ - 1];
	  /* L135: */
	}
      fftpack_dcosqf (&n, x, w);
      cosqft = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = cosqft, d__3 = (d__1 = y[i__ - 1] - x[i__ - 1], Abs (d__1));
	  cosqft = Max (d__2, d__3);
	  x[i__ - 1] = xh[i__ - 1];
	  y[i__ - 1] = xh[i__ - 1];
	  /* L136: */
	}
      cosqft = cf * cosqft;
      fftpack_dcosqb (&n, x, w);
      fftpack_dcosqf (&n, x, w);
      cosqfb = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = cosqfb, d__3 = (d__1 =
				 cf * x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  cosqfb = Max (d__2, d__3);
	  /* L137: */
	}
      /* 
       *    TEST PROGRAMS EZFFTI,EZFFTF,EZFFTB 
       * 
       */
      fftpack_dzffti (&n, w);
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  x[i__ - 1] = xh[i__ - 1];
	  /* L138: */
	}
      tpi = atan (1.) * 8.;
      dt = tpi / (double) n;
      ns2 = (n + 1) / 2;
      cf = 2. / (double) n;
      ns2m = ns2 - 1;
      if (ns2m <= 0)
	{
	  goto L141;
	}
      i__2 = ns2m;
      for (k = 1; k <= i__2; ++k)
	{
	  sum1 = 0.;
	  sum2 = 0.;
	  arg = (double) k *dt;
	  i__3 = n;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      arg1 = (double) (i__ - 1) * arg;
	      sum1 += x[i__ - 1] * cos (arg1);
	      sum2 += x[i__ - 1] * sin (arg1);
	      /* L139: */
	    }
	  a[k - 1] = cf * sum1;
	  b[k - 1] = cf * sum2;
	  /* L140: */
	}
    L141:
      nm1 = n - 1;
      sum1 = 0.;
      sum2 = 0.;
      i__2 = nm1;
      for (i__ = 1; i__ <= i__2; i__ += 2)
	{
	  sum1 += x[i__ - 1];
	  sum2 += x[i__];
	  /* L142: */
	}
      if (modn == 1)
	{
	  sum1 += x[n - 1];
	}
      azero = cf * .5 * (sum1 + sum2);
      if (modn == 0)
	{
	  a[ns2 - 1] = cf * .5 * (sum1 - sum2);
	}
      fftpack_dzfftf (&n, x, &azeroh, ah, bh, w);
      dezf1 = (d__1 = azeroh - azero, Abs (d__1));
      if (modn == 0)
	{
	  /*Computing MAX 
	   */
	  d__2 = dezf1, d__3 = (d__1 = a[ns2 - 1] - ah[ns2 - 1], Abs (d__1));
	  dezf1 = Max (d__2, d__3);
	}
      if (ns2m <= 0)
	{
	  goto L144;
	}
      i__2 = ns2m;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__3 = dezf1, d__4 = (d__1 =
				ah[i__ - 1] - a[i__ - 1], Abs (d__1)), d__3 =
	    Max (d__3, d__4), d__4 = (d__2 =
				      bh[i__ - 1] - b[i__ - 1], Abs (d__2));
	  dezf1 = Max (d__3, d__4);
	  /* L143: */
	}
    L144:
      ns2 = n / 2;
      if (modn == 0)
	{
	  b[ns2 - 1] = 0.;
	}
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  sum = azero;
	  arg1 = (double) (i__ - 1) * dt;
	  i__3 = ns2;
	  for (k = 1; k <= i__3; ++k)
	    {
	      arg2 = (double) k *arg1;
	      sum = sum + a[k - 1] * cos (arg2) + b[k - 1] * sin (arg2);
	      /* L145: */
	    }
	  x[i__ - 1] = sum;
	  /* L146: */
	}
      fftpack_dzfftb (&n, y, &azero, a, b, w);
      dezb1 = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = dezb1, d__3 = (d__1 = x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  dezb1 = Max (d__2, d__3);
	  x[i__ - 1] = xh[i__ - 1];
	  /* L147: */
	}
      fftpack_dzfftf (&n, x, &azero, a, b, w);
      fftpack_dzfftb (&n, y, &azero, a, b, w);
      dezfb = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  d__2 = dezfb, d__3 = (d__1 = x[i__ - 1] - y[i__ - 1], Abs (d__1));
	  dezfb = Max (d__2, d__3);
	  /* L148: */
	}
      /* 
       *    TEST  CFFTI,CFFTF,CFFTB 
       * 
       */
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  i__3 = i__ - 1;
	  d__1 = cos (sqrt2 * (double) i__);
	  d__2 = sin (sqrt2 * (double) (i__ * i__));
	  z__1.r = d__1, z__1.i = d__2;
	  cx[i__3].r = z__1.r, cx[i__3].i = z__1.i;
	  /* L149: */
	}
      dt = (pi + pi) / fn;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  arg1 = -((double) (i__ - 1)) * dt;
	  i__3 = i__ - 1;
	  cy[i__3].r = 0., cy[i__3].i = 0.;
	  i__3 = n;
	  for (k = 1; k <= i__3; ++k)
	    {
	      arg2 = (double) (k - 1) * arg1;
	      i__4 = i__ - 1;
	      i__5 = i__ - 1;
	      d__1 = cos (arg2);
	      d__2 = sin (arg2);
	      z__3.r = d__1, z__3.i = d__2;
	      i__6 = k - 1;
	      z__2.r = z__3.r * cx[i__6].r - z__3.i * cx[i__6].i, z__2.i =
		z__3.r * cx[i__6].i + z__3.i * cx[i__6].r;
	      z__1.r = cy[i__5].r + z__2.r, z__1.i = cy[i__5].i + z__2.i;
	      cy[i__4].r = z__1.r, cy[i__4].i = z__1.i;
	      /* L150: */
	    }
	  /* L151: */
	}
      fftpack_zffti (&n, w);
      fftpack_zfftf (&n, (double *) cx, w);
      dcfftf = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  i__3 = i__ - 1;
	  i__4 = i__ - 1;
	  z__1.r = cx[i__3].r - cy[i__4].r, z__1.i = cx[i__3].i - cy[i__4].i;
	  d__1 = dcfftf, d__2 = nsp_abs_c (&z__1);
	  dcfftf = Max (d__1, d__2);
	  i__3 = i__ - 1;
	  i__4 = i__ - 1;
	  z__1.r = cx[i__4].r / fn, z__1.i = cx[i__4].i / fn;
	  cx[i__3].r = z__1.r, cx[i__3].i = z__1.i;
	  /* L152: */
	}
      dcfftf /= fn;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  arg1 = (double) (i__ - 1) * dt;
	  i__3 = i__ - 1;
	  cy[i__3].r = 0., cy[i__3].i = 0.;
	  i__3 = n;
	  for (k = 1; k <= i__3; ++k)
	    {
	      arg2 = (double) (k - 1) * arg1;
	      i__4 = i__ - 1;
	      i__5 = i__ - 1;
	      d__1 = cos (arg2);
	      d__2 = sin (arg2);
	      z__3.r = d__1, z__3.i = d__2;
	      i__6 = k - 1;
	      z__2.r = z__3.r * cx[i__6].r - z__3.i * cx[i__6].i, z__2.i =
		z__3.r * cx[i__6].i + z__3.i * cx[i__6].r;
	      z__1.r = cy[i__5].r + z__2.r, z__1.i = cy[i__5].i + z__2.i;
	      cy[i__4].r = z__1.r, cy[i__4].i = z__1.i;
	      /* L153: */
	    }
	  /* L154: */
	}
      fftpack_zfftb (&n, (double *) cx, w);
      dcfftb = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  i__3 = i__ - 1;
	  i__4 = i__ - 1;
	  z__1.r = cx[i__3].r - cy[i__4].r, z__1.i = cx[i__3].i - cy[i__4].i;
	  d__1 = dcfftb, d__2 = nsp_abs_c (&z__1);
	  dcfftb = Max (d__1, d__2);
	  i__3 = i__ - 1;
	  i__4 = i__ - 1;
	  cx[i__3].r = cy[i__4].r, cx[i__3].i = cy[i__4].i;
	  /* L155: */
	}
      cf = 1. / fn;
      fftpack_zfftf (&n, (double *) cx, w);
      fftpack_zfftb (&n, (double *)cx, w);
      dcfb = 0.;
      i__2 = n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /*Computing MAX 
	   */
	  i__3 = i__ - 1;
	  z__2.r = cf * cx[i__3].r, z__2.i = cf * cx[i__3].i;
	  i__4 = i__ - 1;
	  z__1.r = z__2.r - cy[i__4].r, z__1.i = z__2.i - cy[i__4].i;
	  d__1 = dcfb, d__2 = nsp_abs_c (&z__1);
	  dcfb = Max (d__1, d__2);
	  /* L156: */
	}
      /* L157: */
    }
  return 0;
}

/* Subroutine */ int
fftpack_dsinqb (int *n, double *x, double *wsave)
{
  int i__1;
  int k;
  double xhold;
  int kc;
  int ns2;

  --wsave;
  --x;

  if (*n > 1)
    {
      goto L101;
    }
  x[1] *= 4.;
  return 0;
 L101:
  ns2 = *n / 2;
  i__1 = *n;
  for (k = 2; k <= i__1; k += 2)
    {
      x[k] = -x[k];
      /* L102: */
    }
  fftpack_dcosqb (n, &x[1], &wsave[1]);
  i__1 = ns2;
  for (k = 1; k <= i__1; ++k)
    {
      kc = *n - k;
      xhold = x[k];
      x[k] = x[kc + 1];
      x[kc + 1] = xhold;
      /* L103: */
    }
  return 0;
}				/* dsinqb_ */


/* Subroutine */ int
fftpack_dsinqf (int *n, double *x, double *wsave)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int k;
  double xhold;
  int kc;
  int ns2;

  /* Parameter adjustments */
  --wsave;
  --x;

  /* Function Body */
  if (*n == 1)
    {
      return 0;
    }
  ns2 = *n / 2;
  i__1 = ns2;
  for (k = 1; k <= i__1; ++k)
    {
      kc = *n - k;
      xhold = x[k];
      x[k] = x[kc + 1];
      x[kc + 1] = xhold;
      /* L101: */
    }
  fftpack_dcosqf (n, &x[1], &wsave[1]);
  i__1 = *n;
  for (k = 2; k <= i__1; k += 2)
    {
      x[k] = -x[k];
      /* L102: */
    }
  return 0;
}				/* dsinqf_ */


/* Subroutine */ int
fftpack_dsinqi (int *n, double *wsave)
{

  /* Parameter adjustments */
  --wsave;

  /* Function Body */
  fftpack_dcosqi (n, &wsave[1]);
  return 0;
}				/* dsinqi_ */

/* Subroutine */ int
fftpack_dcosqb (int *n, double *x, double *wsave)
{
  /* Initialized data */

  static double tsqrt2 = 2.8284271247461900976;

  /* System generated locals */
  int i__1;

  /* Local variables */
  double x1;

  /* Parameter adjustments */
  --wsave;
  --x;

  /* Function Body */
  if ((i__1 = *n - 2) < 0)
    {
      goto L101;
    }
  else if (i__1 == 0)
    {
      goto L102;
    }
  else
    {
      goto L103;
    }
 L101:
  x[1] *= 4.;
  return 0;
 L102:
  x1 = (x[1] + x[2]) * 4.;
  x[2] = tsqrt2 * (x[1] - x[2]);
  x[1] = x1;
  return 0;
 L103:
  fftpack_cosqb1 (n, &x[1], &wsave[1], &wsave[*n + 1]);
  return 0;
}				/* dcosqb_ */

/* Subroutine */ int
fftpack_dcosqf (int *n, double *x, double *wsave)
{
  /* Initialized data */

  static double sqrt2 = 1.4142135623730950488;

  /* System generated locals */
  int i__1;

  /* Local variables */
  double tsqx;

  /* Parameter adjustments */
  --wsave;
  --x;

  /* Function Body */
  if ((i__1 = *n - 2) < 0)
    {
      goto L102;
    }
  else if (i__1 == 0)
    {
      goto L101;
    }
  else
    {
      goto L103;
    }
 L101:
  tsqx = sqrt2 * x[2];
  x[2] = x[1] - tsqx;
  x[1] += tsqx;
 L102:
  return 0;
 L103:
  fftpack_cosqf1 (n, &x[1], &wsave[1], &wsave[*n + 1]);
  return 0;
}				/* dcosqf_ */


/* Subroutine */ int
fftpack_dcosqi (int *n, double *wsave)
{
  /* Initialized data */

  static double pih = 1.57079632679489661923;

  /* System generated locals */
  int i__1;

  /* Builtin functions */
  double cos (double);

  /* Local variables */
  int k;
  double fk, dt;

  /* Parameter adjustments */
  --wsave;

  /* Function Body */
  dt = pih / (double) (*n);
  fk = 0.;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      fk += 1.;
      wsave[k] = cos (fk * dt);
      /* L101: */
    }
  fftpack_dffti (n, &wsave[*n + 1]);
  return 0;
}				/* dcosqi_ */



/* Subroutine */ int
fftpack_dcost (int *n, double *x, double *wsave)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int modn, i__, k;
  double c1, t1, t2;
  int kc;
  double xi;
  int nm1, np1;
  double x1h;
  int ns2;
  double tx2, x1p3, xim2;

  /* Parameter adjustments */
  --wsave;
  --x;

  /* Function Body */
  nm1 = *n - 1;
  np1 = *n + 1;
  ns2 = *n / 2;
  if ((i__1 = *n - 2) < 0)
    {
      goto L106;
    }
  else if (i__1 == 0)
    {
      goto L101;
    }
  else
    {
      goto L102;
    }
 L101:
  x1h = x[1] + x[2];
  x[2] = x[1] - x[2];
  x[1] = x1h;
  return 0;
 L102:
  if (*n > 3)
    {
      goto L103;
    }
  x1p3 = x[1] + x[3];
  tx2 = x[2] + x[2];
  x[2] = x[1] - x[3];
  x[1] = x1p3 + tx2;
  x[3] = x1p3 - tx2;
  return 0;
 L103:
  c1 = x[1] - x[*n];
  x[1] += x[*n];
  i__1 = ns2;
  for (k = 2; k <= i__1; ++k)
    {
      kc = np1 - k;
      t1 = x[k] + x[kc];
      t2 = x[k] - x[kc];
      c1 += wsave[kc] * t2;
      t2 = wsave[k] * t2;
      x[k] = t1 - t2;
      x[kc] = t1 + t2;
      /* L104: */
    }
  modn = *n % 2;
  if (modn != 0)
    {
      x[ns2 + 1] += x[ns2 + 1];
    }
  fftpack_dfftf (&nm1, &x[1], &wsave[*n + 1]);
  xim2 = x[2];
  x[2] = c1;
  i__1 = *n;
  for (i__ = 4; i__ <= i__1; i__ += 2)
    {
      xi = x[i__];
      x[i__] = x[i__ - 2] - x[i__ - 1];
      x[i__ - 1] = xim2;
      xim2 = xi;
      /* L105: */
    }
  if (modn != 0)
    {
      x[*n] = xim2;
    }
 L106:
  return 0;
}				/* dcost_ */

/* Subroutine */ int
fftpack_dcosti (int *n, double *wsave)
{
  /* Initialized data */

  static double pi = 3.14159265358979323846;

  /* System generated locals */
  int i__1;

  /* Builtin functions */
  double sin (double), cos (double);

  /* Local variables */
  int k;
  int kc;
  double fk, dt;
  int nm1, np1, ns2;

  /* Parameter adjustments */
  --wsave;

  /* Function Body */
  if (*n <= 3)
    {
      return 0;
    }
  nm1 = *n - 1;
  np1 = *n + 1;
  ns2 = *n / 2;
  dt = pi / (double) nm1;
  fk = 0.;
  i__1 = ns2;
  for (k = 2; k <= i__1; ++k)
    {
      kc = np1 - k;
      fk += 1.;
      wsave[k] = sin (fk * dt) * 2.;
      wsave[kc] = cos (fk * dt) * 2.;
      /* L101: */
    }
  fftpack_dffti (&nm1, &wsave[*n + 1]);
  return 0;
}				/* dcosti_ */


/* Subroutine */ int
fftpack_dsinti (int *n, double *wsave)
{
  /* Initialized data */

  static double pi = 3.14159265358979323846;

  /* System generated locals */
  int i__1;

  /* Builtin functions */
  double sin (double);

  /* Local variables */
  int k;
  double dt;
  int np1, ns2;

  /* Parameter adjustments */
  --wsave;

  /* Function Body */
  if (*n <= 1)
    {
      return 0;
    }
  ns2 = *n / 2;
  np1 = *n + 1;
  dt = pi / (double) np1;
  i__1 = ns2;
  for (k = 1; k <= i__1; ++k)
    {
      wsave[k] = sin (k * dt) * 2.;
      /* L101: */
    }
  fftpack_dffti (&np1, &wsave[ns2 + 1]);
  return 0;
}				/* dsinti_ */

/* Subroutine */ int
fftpack_int (int *n, double *war, double *was, double *xh, double *x,
	     int *ifac)
{
  /* Initialized data */

  static double sqrt3 = 1.73205080756887729352;

  /* System generated locals */
  int i__1;

  /* Local variables */
  int modn, i__, k;
  double xhold, t1, t2;
  int kc, np1, ns2;

  /* Parameter adjustments */
  --ifac;
  --x;
  --xh;
  --was;
  --war;

  /* Function Body */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      xh[i__] = war[i__];
      war[i__] = x[i__];
      /* L100: */
    }
  if ((i__1 = *n - 2) < 0)
    {
      goto L101;
    }
  else if (i__1 == 0)
    {
      goto L102;
    }
  else
    {
      goto L103;
    }
 L101:
  xh[1] += xh[1];
  goto L106;
 L102:
  xhold = sqrt3 * (xh[1] + xh[2]);
  xh[2] = sqrt3 * (xh[1] - xh[2]);
  xh[1] = xhold;
  goto L106;
 L103:
  np1 = *n + 1;
  ns2 = *n / 2;
  x[1] = 0.;
  i__1 = ns2;
  for (k = 1; k <= i__1; ++k)
    {
      kc = np1 - k;
      t1 = xh[k] - xh[kc];
      t2 = was[k] * (xh[k] + xh[kc]);
      x[k + 1] = t1 + t2;
      x[kc + 1] = t2 - t1;
      /* L104: */
    }
  modn = *n % 2;
  if (modn != 0)
    {
      x[ns2 + 2] = xh[ns2 + 1] * 4.;
    }
  fftpack_rfftf1 (&np1, &x[1], &xh[1], &war[1], &ifac[1]);
  xh[1] = x[1] * .5;
  i__1 = *n;
  for (i__ = 3; i__ <= i__1; i__ += 2)
    {
      xh[i__ - 1] = -x[i__];
      xh[i__] = xh[i__ - 2] + x[i__ - 1];
      /* L105: */
    }
  if (modn != 0)
    {
      goto L106;
    }
  xh[*n] = -x[*n + 1];
 L106:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      x[i__] = war[i__];
      war[i__] = xh[i__];
      /* L107: */
    }
  return 0;
}				/* sint1_ */


/* Subroutine */ int
fftpack_dsint (int *n, double *x, double *wsave)
{
  int np1, iw1, iw2, iw3;

  /* Parameter adjustments */
  --wsave;
  --x;

  /* Function Body */
  np1 = *n + 1;
  iw1 = *n / 2 + 1;
  iw2 = iw1 + np1;
  iw3 = iw2 + np1;
  fftpack_int (n, &x[1], &wsave[1], &wsave[iw1], &wsave[iw2],
	       (int *) &wsave[iw3]);
  return 0;
}				/* dsint_ */

/* Subroutine */ int
fftpack_cosqb1 (int *n, double *x, double *w, double *xh)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int modn, i__, k;
  int kc, np2, ns2;
  double xim1;

  /* Parameter adjustments */
  --xh;
  --w;
  --x;

  /* Function Body */
  ns2 = (*n + 1) / 2;
  np2 = *n + 2;
  i__1 = *n;
  for (i__ = 3; i__ <= i__1; i__ += 2)
    {
      xim1 = x[i__ - 1] + x[i__];
      x[i__] -= x[i__ - 1];
      x[i__ - 1] = xim1;
      /* L101: */
    }
  x[1] += x[1];
  modn = *n % 2;
  if (modn == 0)
    {
      x[*n] += x[*n];
    }
  fftpack_dfftb (n, &x[1], &xh[1]);
  i__1 = ns2;
  for (k = 2; k <= i__1; ++k)
    {
      kc = np2 - k;
      xh[k] = w[k - 1] * x[kc] + w[kc - 1] * x[k];
      xh[kc] = w[k - 1] * x[k] - w[kc - 1] * x[kc];
      /* L102: */
    }
  if (modn == 0)
    {
      x[ns2 + 1] = w[ns2] * (x[ns2 + 1] + x[ns2 + 1]);
    }
  i__1 = ns2;
  for (k = 2; k <= i__1; ++k)
    {
      kc = np2 - k;
      x[k] = xh[k] + xh[kc];
      x[kc] = xh[k] - xh[kc];
      /* L103: */
    }
  x[1] += x[1];
  return 0;
}

