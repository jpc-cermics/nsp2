#include "signal.h"

static double signal_dgee01 (const int *k,const int *n,const int *m, double *x);
static double signal_gee (const int *k,const int *n, const double *ad, const double *x, const  double *y, const double *grid);

/*
 *  this subroutine implements the remez exchange algorithm 
 *  for the weighted chebyshev approximation of a continuous 
 *  function with a sum of cosines.  inputs to the subroutine 
 *  are a dense grid which replaces the frequency axis, the 
 *  desired function on this grid, the weight function on the 
 *  grid, the number of cosines, and an initial guess of the 
 *  extremal frequencies.  the program minimizes the chebyshev 
 *  error by determining the best location of the extremal 
 *  frequencies (points of maximum error) and then calculates 
 *  the coefficients of the best approximation. 

 *    subroutine remez(ngr,nfc,iext,ad,x,y,des,grid,wt,a,p,q,alpha) 
 *    dimensioning of arrays in calling program is as follows: 
 *        working vectors   ad,x,y,a,p,q:   nfc+2 
 *        input vectors     des,grid,wt:    ngr 
 *        input vector      iext:           nfc+2 
 *        output vector     alpha:          nfc+2 
 *  Authors:
 *        JAMES H. MCCLELLAN 
 *        DEPARTMENT OF ELECTRICAL ENGINEERING AND COMPUTER SCIENCE 
 *        MASSACHUSETTS INSTITUTE OF TECHNOLOGY 
 *        CAMBRIDGE, MASS. 02139 
 * 
 *        THOMAS W. PARKS 
 *        DEPARTMENT OF ELECTRICAL ENGINEERING 
 *        RICE UNIVERSITY 
 *        HOUSTON, TEXAS 77001 
 * 
 *        LAWRENCE R. RABINER 
 *        BELL LABORATORIES 
 *        MURRAY HILL, NEW JERSEY 07974 
 * 
 * the program allows a maximum number of iterations of 25 
 */

struct
{
  double pi2, dev;
  int nfcns, ngrid;
} remez_data;

static int c__1 = 1;


int
signal_remez (int *ngr, int *nfc, int *iext, double *ad, double *x, double *y,
	      double *des, double *grid, double *wt, double *a, double *p,
	      double *q, double *alpha)
{
  int remez_niter = 0;
  int i__1, i__2;
  double dden, delf, devl;
  double comp = 0;
  int luck = 0;
  double dnum;
  int klow, nzmj;
  int j, k, l;
  double dtemp, gtemp;
  int k1, nzzmj;
  double y1 = 0 , aa, bb, dk, cn, pi;
  int kn;
  double ft, xe;
  int jchnge, nu, nz;
  double xt;
  int itrmax, jm1, nm1, jp1;
  double xt1, dak;
  double fsh;
  int kkk, jet;
  double err;
  int kup, knz, jxt, nut;
  double ynz = 0;
  int nzz, nf1j, nut1 = 0;

  /* Parameter adjustments */
  --alpha;
  --q;
  --p;
  --a;
  --wt;
  --grid;
  --des;
  --y;
  --x;
  --ad;
  --iext;

  /* Function Body */
  itrmax = 25;
  remez_data.ngrid = *ngr;
  remez_data.nfcns = *nfc;
  pi = atan (1.) * 4.;
  remez_data.pi2 = pi * 2.;
  devl = -1.;
  nz = remez_data.nfcns + 1;
  nzz = remez_data.nfcns + 2;
  remez_niter = 0;
 L100:
  iext[nzz] = remez_data.ngrid + 1;
  ++remez_niter;
  if (remez_niter > itrmax)
    {
      goto L400;
    }
  i__1 = nz;
  for (j = 1; j <= i__1; ++j)
    {
      jxt = iext[j];
      dtemp = grid[jxt];
      dtemp = cos (dtemp * remez_data.pi2);
      /* L110: */
      x[j] = dtemp;
    }
  jet = (remez_data.nfcns - 1) / 15 + 1;
  i__1 = nz;
  for (j = 1; j <= i__1; ++j)
    {
      /* L120: */
      ad[j] = signal_dgee01 (&j, &nz, &jet, &x[1]);
    }
  dnum = 0.;
  dden = 0.;
  k = 1;
  i__1 = nz;
  for (j = 1; j <= i__1; ++j)
    {
      l = iext[j];
      dtemp = ad[j] * des[l];
      dnum += dtemp;
      dtemp = (double) k *ad[j] / wt[l];
      dden += dtemp;
      /* L130: */
      k = -k;
    }
  remez_data.dev = dnum / dden;
  /*     write(6,131) dev 
   * 131 format(1x,12hdeviation = ,f12.9) 
   */
  nu = 1;
  if (remez_data.dev > 0.)
    {
      nu = -1;
    }
  remez_data.dev = -((double) nu) * remez_data.dev;
  k = nu;
  i__1 = nz;
  for (j = 1; j <= i__1; ++j)
    {
      l = iext[j];
      dtemp = (double) k *remez_data.dev / wt[l];
      y[j] = des[l] + dtemp;
      /* L140: */
      k = -k;
    }
  if (remez_data.dev > devl)
    {
      goto L150;
    }
  /* signal_ouch (); */
  goto L400;
 L150:
  devl = remez_data.dev;
  jchnge = 0;
  k1 = iext[1];
  knz = iext[nz];
  klow = 0;
  nut = -nu;
  j = 1;
  /* 
   * search for the extremal frequencies of the best 
   * approximation 
   * 
   */
 L200:
  if (j == nzz)
    {
      ynz = comp;
    }
  if (j >= nzz)
    {
      goto L300;
    }
  kup = iext[j + 1];
  l = iext[j] + 1;
  nut = -nut;
  if (j == 2)
    {
      y1 = comp;
    }
  comp = remez_data.dev;
  if (l >= kup)
    {
      goto L220;
    }
  err = signal_gee (&l, &nz, &ad[1], &x[1], &y[1], &grid[1]);
  err = (err - des[l]) * wt[l];
  dtemp = (double) nut *err - comp;
  if (dtemp <= 0.)
    {
      goto L220;
    }
  comp = (double) nut *err;
 L210:
  ++l;
  if (l >= kup)
    {
      goto L215;
    }
  err = signal_gee (&l, &nz, &ad[1], &x[1], &y[1], &grid[1]);
  err = (err - des[l]) * wt[l];
  dtemp = (double) nut *err - comp;
  if (dtemp <= 0.)
    {
      goto L215;
    }
  comp = (double) nut *err;
  goto L210;
 L215:
  iext[j] = l - 1;
  ++j;
  klow = l - 1;
  ++jchnge;
  goto L200;
 L220:
  --l;
 L225:
  --l;
  if (l <= klow)
    {
      goto L250;
    }
  err = signal_gee (&l, &nz, &ad[1], &x[1], &y[1], &grid[1]);
  err = (err - des[l]) * wt[l];
  dtemp = (double) nut *err - comp;
  if (dtemp > 0.)
    {
      goto L230;
    }
  if (jchnge <= 0)
    {
      goto L225;
    }
  goto L260;
 L230:
  comp = (double) nut *err;
 L235:
  --l;
  if (l <= klow)
    {
      goto L240;
    }
  err = signal_gee (&l, &nz, &ad[1], &x[1], &y[1], &grid[1]);
  err = (err - des[l]) * wt[l];
  dtemp = (double) nut *err - comp;
  if (dtemp <= 0.)
    {
      goto L240;
    }
  comp = (double) nut *err;
  goto L235;
 L240:
  klow = iext[j];
  iext[j] = l + 1;
  ++j;
  ++jchnge;
  goto L200;
 L250:
  l = iext[j] + 1;
  if (jchnge > 0)
    {
      goto L215;
    }
 L255:
  ++l;
  if (l >= kup)
    {
      goto L260;
    }
  err = signal_gee (&l, &nz, &ad[1], &x[1], &y[1], &grid[1]);
  err = (err - des[l]) * wt[l];
  dtemp = (double) nut *err - comp;
  if (dtemp <= 0.)
    {
      goto L255;
    }
  comp = (double) nut *err;
  goto L210;
 L260:
  klow = iext[j];
  ++j;
  goto L200;
 L300:
  if (j > nzz)
    {
      goto L320;
    }
  if (k1 > iext[1])
    {
      k1 = iext[1];
    }
  if (knz < iext[nz])
    {
      knz = iext[nz];
    }
  nut1 = nut;
  nut = -nu;
  l = 0;
  kup = k1;
  comp = ynz * 1.00001;
  luck = 1;
 L310:
  ++l;
  if (l >= kup)
    {
      goto L315;
    }
  err = signal_gee (&l, &nz, &ad[1], &x[1], &y[1], &grid[1]);
  err = (err - des[l]) * wt[l];
  dtemp = (double) nut *err - comp;
  if (dtemp <= 0.)
    {
      goto L310;
    }
  comp = (double) nut *err;
  j = nzz;
  goto L210;
 L315:
  luck = 6;
  goto L325;
 L320:
  if (luck > 9)
    {
      goto L350;
    }
  if (comp > y1)
    {
      y1 = comp;
    }
  k1 = iext[nzz];
 L325:
  l = remez_data.ngrid + 1;
  klow = knz;
  nut = -nut1;
  comp = y1 * 1.00001;
 L330:
  --l;
  if (l <= klow)
    {
      goto L340;
    }
  err = signal_gee (&l, &nz, &ad[1], &x[1], &y[1], &grid[1]);
  err = (err - des[l]) * wt[l];
  dtemp = (double) nut *err - comp;
  if (dtemp <= 0.)
    {
      goto L330;
    }
  j = nzz;
  comp = (double) nut *err;
  luck += 10;
  goto L235;
 L340:
  if (luck == 6)
    {
      goto L370;
    }
  i__1 = remez_data.nfcns;
  for (j = 1; j <= i__1; ++j)
    {
      nzzmj = nzz - j;
      nzmj = nz - j;
      /* L345: */
      iext[nzzmj] = iext[nzmj];
    }
  iext[1] = k1;
  goto L100;
 L350:
  kn = iext[nzz];
  i__1 = remez_data.nfcns;
  for (j = 1; j <= i__1; ++j)
    {
      /* L360: */
      iext[j] = iext[j + 1];
    }
  iext[nz] = kn;
  goto L100;
 L370:
  if (jchnge > 0)
    {
      goto L100;
    }
  /* 
   * calculation of the coefficients of the best approximation 
   * using the inverse discrete fourier transform 
   * 
   */
 L400:
  nm1 = remez_data.nfcns - 1;
  fsh = 1e-6;
  gtemp = grid[1];
  x[nzz] = -2.;
  cn = (double) ((remez_data.nfcns << 1) - 1);
  delf = 1. / cn;
  l = 1;
  kkk = 0;
  if (grid[1] < .01 && grid[remez_data.ngrid] > .49)
    {
      kkk = 1;
    }
  if (remez_data.nfcns <= 3)
    {
      kkk = 1;
    }
  if (kkk == 1)
    {
      goto L405;
    }
  dtemp = cos (remez_data.pi2 * grid[1]);
  dnum = cos (remez_data.pi2 * grid[remez_data.ngrid]);
  aa = 2. / (dtemp - dnum);
  bb = -(dtemp + dnum) / (dtemp - dnum);
 L405:
  i__1 = remez_data.nfcns;
  for (j = 1; j <= i__1; ++j)
    {
      ft = (double) (j - 1);
      ft *= delf;
      xt = cos (remez_data.pi2 * ft);
      if (kkk == 1)
	{
	  goto L410;
	}
      xt = (xt - bb) / aa;
      xt1 = sqrt (1. - xt * xt);
      ft = atan2 (xt1, xt) / remez_data.pi2;
    L410:
      xe = x[l];
      if (xt > xe)
	{
	  goto L420;
	}
      if (xe - xt < fsh)
	{
	  goto L415;
	}
      ++l;
      goto L410;
    L415:
      a[j] = y[l];
      goto L425;
    L420:
      if (xt - xe < fsh)
	{
	  goto L415;
	}
      grid[1] = ft;
      a[j] = signal_gee (&c__1, &nz, &ad[1], &x[1], &y[1], &grid[1]);
    L425:
      if (l > 1)
	{
	  --l;
	}
      /* L430: */
    }
  grid[1] = gtemp;
  dden = remez_data.pi2 / cn;
  i__1 = remez_data.nfcns;
  for (j = 1; j <= i__1; ++j)
    {
      dtemp = 0.;
      dnum = (double) (j - 1);
      dnum *= dden;
      if (nm1 < 1)
	{
	  goto L505;
	}
      i__2 = nm1;
      for (k = 1; k <= i__2; ++k)
	{
	  dak = a[k + 1];
	  dk = (double) k;
	  /* L500: */
	  dtemp += dak * cos (dnum * dk);
	}
    L505:
      dtemp = dtemp * 2. + a[1];
      /* L510: */
      alpha[j] = dtemp;
    }
  i__1 = remez_data.nfcns;
  for (j = 2; j <= i__1; ++j)
    {
      /* L550: */
      alpha[j] = alpha[j] * 2. / cn;
    }
  alpha[1] /= cn;
  if (kkk == 1)
    {
      goto L545;
    }
  p[1] = alpha[remez_data.nfcns] * 2. * bb + alpha[nm1];
  p[2] = aa * 2. * alpha[remez_data.nfcns];
  q[1] = alpha[remez_data.nfcns - 2] - alpha[remez_data.nfcns];
  i__1 = nm1;
  for (j = 2; j <= i__1; ++j)
    {
      if (j < nm1)
	{
	  goto L515;
	}
      aa *= .5;
      bb *= .5;
    L515:
      p[j + 1] = 0.;
      i__2 = j;
      for (k = 1; k <= i__2; ++k)
	{
	  a[k] = p[k];
	  /* L520: */
	  p[k] = bb * 2. * a[k];
	}
      p[2] += a[1] * 2. * aa;
      jm1 = j - 1;
      i__2 = jm1;
      for (k = 1; k <= i__2; ++k)
	{
	  /* L525: */
	  p[k] = p[k] + q[k] + aa * a[k + 1];
	}
      jp1 = j + 1;
      i__2 = jp1;
      for (k = 3; k <= i__2; ++k)
	{
	  /* L530: */
	  p[k] += aa * a[k - 1];
	}
      if (j == nm1)
	{
	  goto L540;
	}
      i__2 = j;
      for (k = 1; k <= i__2; ++k)
	{
	  /* L535: */
	  q[k] = -a[k];
	}
      nf1j = remez_data.nfcns - 1 - j;
      q[1] += alpha[nf1j];
    L540:
      ;
    }
  i__1 = remez_data.nfcns;
  for (j = 1; j <= i__1; ++j)
    {
      /* L543: */
      alpha[j] = p[j];
    }
 L545:
  if (remez_data.nfcns > 3)
    {
      return 0;
    }
  alpha[remez_data.nfcns + 1] = 0.;
  alpha[remez_data.nfcns + 2] = 0.;
  return 0;
}				/* remez_ */



/*!purpose 
 *  function to evaluate the frequency response using the 
 *  lagrange interpolation formula in the barycentric form 
 *! 
 */

static double signal_gee (const int *k,const int *n, const double *ad, const double *x, const  double *y, const double *grid)
{
  int i__1;
  double ret_val;

  /* Local variables */
  double c__, d__;
  int j;
  double p, xf;

  /* Parameter adjustments */
  --grid;
  --y;
  --x;
  --ad;

  /* Function Body */
  p = 0.;
  xf = grid[*k];
  xf = cos (remez_data.pi2 * xf);
  d__ = 0.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      c__ = xf - x[j];
      c__ = ad[j] / c__;
      d__ += c__;
      /* L1: */
      p += c__ * y[j];
    }
  ret_val = p / d__;
  return ret_val;
}				/* gee_ */



/*!purpose 
 *  function to calculate the lagrange interpolation 
 *  coefficients for use in the function gee. 
 *! 
 * 
 */

double
signal_dgee01 (const int *k,const int *n,const int *m, double *x)
{
  int i__1, i__2, i__3;
  double ret_val;

  /* Local variables */
  double d__;
  int j, l;
  double q;

  /* Parameter adjustments */
  --x;

  /* Function Body */
  d__ = 1.;
  q = x[*k];
  i__1 = *m;
  for (l = 1; l <= i__1; ++l)
    {
      i__2 = *n;
      i__3 = *m;
      for (j = l; i__3 < 0 ? j >= i__2 : j <= i__2; j += i__3)
	{
	  if (j - *k == 0)
	    {
	      goto L2;
	    }
	  else
	    {
	      goto L1;
	    }
	L1:
	  d__ = d__ * 2. * (q - x[j]);
	L2:
	  ;
	}
      /* L3: */
    }
  ret_val = 1. / d__;
  return ret_val;
}				/* dgee01_ */
