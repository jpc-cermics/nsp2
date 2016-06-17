#include "signal.h"

/* Common Block Declarations */

struct
{
  int iero;
} iercorr_;

#define iercorr_1 iercorr_

/* Table of constant values */

static int c__0 = 0;
static int c__1 = 1;

/*
 *    calcul de  correlations. 
 *authors:      l r rabiner 
 *              bell laboratories, murray hill, new jersey 07974 
 *              r w schafer and d dlugos 
 * 
 *ieee trans on audio and elect, vol 18, no 4, pp 439-442, 1970. 
 * 
 *input:        m is the section size(must be a power of 2) 
 *              n is the number of samples to be used is the analysis 
 *              mode is the data format type 
 *                  mode = 0   auto correlation 
 *                  mode = 1   cross correlation 
 *                  mode = 2   auto covariance 
 *                  mode = 3   cross covariance 
 *              fs is the sampling frequency in hz 
 *              iwin is the window type 
 *                  iwin = 1   rectangular window 
 *                  iwin = 2   hamming window 
 * 
 *    modifications scilab : 
 * 
 *       en sortie xa contient m/2+1 coeff de correlation 
 *       xr,xi,zr,zi sont des tableaux de travail 
 *       xa,xr,xi dimensionnes a m=puissance de 2 
 *       zr,zi dimensionnes a m/2+1 
 *       zr(1),zr(2) contient en sortie xmean et ymean 
 */

int
signal_cmpse2 (int *m, int *n, int *mode, F_dget * dgetx, F_dget * dgety,
	       double *xa, double *xr, double *xi, double *zr, double *zi,
	       int *ierr)
{
  int i__1, i__2;
  double xmni;
  int nrdy, nrdx;
  double xmnr, xsum, ysum;
  int mhlf1;
  int nrdx1, nrdy1, i__, j, k;
  double xmean, ymean;
  int nsect, lshft, nsect1;
  double fn, yirbis;
  int nrd;
  double xii, yii;
  int iss;
  double xir, yir;

  /* Parameter adjustments */
  --xi;
  --xr;
  --xa;
  --zr;
  --zi;

  /* Function Body */
  lshft = *m / 2;
  mhlf1 = lshft + 1;
  nsect = (int) (((double) (*n) + (double) lshft - 1.) / (double) lshft);
  iss = 1;
  nrd = lshft;
  xsum = 0.;
  ysum = 0.;
  i__1 = nsect;
  for (k = 1; k <= i__1; ++k)
    {
      if (k == nsect)
	{
	  nrd = *n - (k - 1) * nrd;
	}
      (*dgetx) (&xa[1], &nrd, &iss);
      if (iercorr_1.iero > 0)
	{
	  return 0;
	}
      i__2 = nrd;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  xsum += xa[i__];
	  /* L40: */
	}
      if (*mode == 2)
	{
	  goto L60;
	}
      (*dgety) (&xa[1], &nrd, &iss);
      if (iercorr_1.iero > 0)
	{
	  return 0;
	}
      i__2 = nrd;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ysum += xa[i__];
	  /* L50: */
	}
    L60:
      iss += nrd;
      /* L70: */
    }
  xmean = xsum / (double) (*n);
  ymean = ysum / (double) (*n);
  if (*mode == 2)
    {
      ymean = xmean;
    }
  xmnr = xmean;
  xmni = ymean;
  iss = 1;
  nrdy = *m;
  nrdx = lshft;
  i__1 = mhlf1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      zr[i__] = 0.;
      zi[i__] = 0.;
      /* L90: */
    }
  i__1 = nsect;
  for (k = 1; k <= i__1; ++k)
    {
      nsect1 = nsect - 1;
      if (k < nsect1)
	{
	  goto L110;
	}
      nrdy = *n - (k - 1) * lshft;
      if (k == nsect)
	{
	  nrdx = nrdy;
	}
      if (nrdy == *m)
	{
	  goto L110;
	}
      nrdy1 = nrdy + 1;
      i__2 = *m;
      for (i__ = nrdy1; i__ <= i__2; ++i__)
	{
	  xr[i__] = 0.;
	  xi[i__] = 0.;
	  /* L100: */
	}
    L110:
      (*dgetx) (&xa[1], &nrdy, &iss);
      if (iercorr_1.iero > 0)
	{
	  return 0;
	}
      i__2 = nrdy;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  xr[i__] = xa[i__];
	  xi[i__] = xa[i__];
	  /* L120: */
	}
      if ((double) (*mode) == 0. || *mode == 2)
	{
	  goto L140;
	}
      (*dgety) (&xa[1], &nrdy, &iss);
      if (iercorr_1.iero > 0)
	{
	  return 0;
	}
      i__2 = nrdy;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  xi[i__] = xa[i__];
	  /* L130: */
	}
    L140:
      if (*mode < 2)
	{
	  goto L160;
	}
      i__2 = nrdy;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  xr[i__] -= xmnr;
	  xi[i__] -= xmni;
	  /* L150: */
	}
    L160:
      nrdx1 = nrdx + 1;
      i__2 = *m;
      for (i__ = nrdx1; i__ <= i__2; ++i__)
	{
	  xr[i__] = 0.;
	  /* L170: */
	}
      signal_fft842 (&c__0, m, &xr[1], &xi[1], ierr);
      if (*ierr > 0)
	{
	  return 0;
	}
      i__2 = lshft;
      for (i__ = 2; i__ <= i__2; ++i__)
	{
	  j = *m + 2 - i__;
	  xir = (xr[i__] + xr[j]) * .5;
	  xii = (xi[i__] - xi[j]) * .5;
	  yir = (xr[j] - xr[i__]) * .5;
	  yii = (xi[i__] + xi[j]) * .5;
	  yirbis = yir;
	  yir = yii;
	  yii = yirbis;
	  zr[i__] = zr[i__] + xir * yir + xii * yii;
	  zi[i__] = zi[i__] + xir * yii - xii * yir;
	  /* L180: */
	}
      zr[1] += xr[1] * xi[1];
      zr[mhlf1] += xr[mhlf1] * xi[mhlf1];
      iss += lshft;
      /* L190: */
    }
  i__1 = lshft;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      j = *m + 2 - i__;
      xr[i__] = zr[i__];
      xi[i__] = zi[i__];
      xr[j] = zr[i__];
      xi[j] = -zi[i__];
      /* L200: */
    }
  xr[1] = zr[1];
  xi[1] = zi[1];
  xr[mhlf1] = zr[mhlf1];
  xi[mhlf1] = zi[mhlf1];
  signal_fft842 (&c__1, m, &xr[1], &xi[1], ierr);
  if (*ierr > 0)
    {
      return 0;
    }
  fn = (double) (*n);
  i__1 = mhlf1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      xa[i__] = xr[i__] / fn;
      /* L210: */
    }
  xr[1] = xmean;
  xr[2] = ymean;
  return 0;
}				/* cmpse2_ */
