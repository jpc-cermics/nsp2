#include "signal.h"

static double c_b2 = 0.;
static int c1 = 1;
static int c0 = 0;

/*
 *    calcul de  correlations d'apres rabiner 
 *    ieee trans on audio and elect, vol 18, no 4, pp 439-442, 1970. 
 * 
 *    m: is the section size (must be a power of 2) 
 *    n: is the number of samples to be used is the analysis 
 *        mode is the data format type 
 *        mode = 0   auto correlation 
 *        mode = 1   cross correlation 
 *    n: size of x and y 
 *    x,y: data to be correlated (mode=0, auto) (mode=1, cross)
 *    
 *    En sortie x contient m/2+1 coeff de correlation 
 *       xr,xi: tableaux de travail 
 *              dimensionnes a m=puissance de 2 
 *       zr,zi: dimensionnes a m/2+1 
 *              en sortie zr et zi sont updates 
 *       
 */

int signal_cmpse3 (int *m, int *n, int *mode, double *x, double *y,
		   double *xr, double *xi, double *zr, double *zi,
		   int *ierr, int *ichaud, int *nbx)
{
  int i1, i2;
  int nrdx, nrdy, mhlf1;
  int nrdx1, nrdy1, i, j, k, nsect;
  int lshft, nsect1;
  double yirbis, xii, yii;
  int iss;
  double xir, yir;
  /* Parameter adjustments */
  --xi;
  --xr;
  --y;
  --x;
  --zr;
  --zi;

  lshft = *m / 2;
  mhlf1 = lshft + 1;
  nsect = (int) (((double) (*n) + (double) lshft - 1.) / (double) lshft);
  iss = 1;
  nrdy = *m;
  nrdx = lshft;
  if (*ichaud == 1)
    {
      /*  hot start */
      nsp_dset (nbx, &c_b2, &xi[1], &c1);
      if (*mode == 1)
	{
	  i1 = *m - *nbx;
	  C2F(dcopy) (&i1, &y[1], &c1, &xi[*nbx + 1], &c1);
	}
      if (*mode == 0)
	{
	  i1 = *m - *nbx;
	  C2F(dcopy) (&i1, &x[1], &c1, &xi[*nbx + 1], &c1);
	}
      signal_fft842 (&c0, m, &xr[1], &xi[1], ierr);
      i1 = lshft;
      for (i = 2; i <= i1; ++i)
	{
	  j = *m + 2 - i;
	  xir = (xr[i] + xr[j]) * .5;
	  xii = (xi[i] - xi[j]) * .5;
	  yir = (xr[j] - xr[i]) * .5;
	  yii = (xi[i] + xi[j]) * .5;
	  yirbis = yir;
	  yir = yii;
	  yii = yirbis;
	  zr[i] = zr[i] + xir * yir + xii * yii;
	  zi[i] = zi[i] + xir * yii - xii * yir;
	}
      zr[1] += xr[1] * xi[1];
      zr[mhlf1] += xr[mhlf1] * xi[mhlf1];
    }
  i1 = nsect;
  for (k = 1; k <= i1; ++k)
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
      i2 = *m;
      for (i = nrdy1; i <= i2; ++i)
	{
	  xr[i] = 0.;
	  xi[i] = 0.;
	}
    L110:
      C2F(dcopy) (&nrdy, &x[iss], &c1, &xr[1], &c1);
      if (*mode == 0)
	{
	  C2F(dcopy) (&nrdy, &x[iss], &c1, &xi[1], &c1);
	}
      if (*mode == 1)
	{
	  C2F(dcopy) (&nrdy, &y[iss], &c1, &xi[1], &c1);
	}
      nrdx1 = nrdx + 1;
      i2 = *m;
      for (i = nrdx1; i <= i2; ++i)
	{
	  xr[i] = 0.;
	}
      signal_fft842 (&c0, m, &xr[1], &xi[1], ierr);
      if (*ierr > 0)
	{
	  return 0;
	}
      i2 = lshft;
      for (i = 2; i <= i2; ++i)
	{
	  j = *m + 2 - i;
	  xir = (xr[i] + xr[j]) * .5;
	  xii = (xi[i] - xi[j]) * .5;
	  yir = (xr[j] - xr[i]) * .5;
	  yii = (xi[i] + xi[j]) * .5;
	  yirbis = yir;
	  yir = yii;
	  yii = yirbis;
	  zr[i] = zr[i] + xir * yir + xii * yii;
	  zi[i] = zi[i] + xir * yii - xii * yir;
	  /* L180: */
	}
      zr[1] += xr[1] * xi[1];
      zr[mhlf1] += xr[mhlf1] * xi[mhlf1];
      iss += lshft;
      /* L190: */
    }
  i1 = lshft;
  for (i = 2; i <= i1; ++i)
    {
      j = *m + 2 - i;
      zr[j] = zr[i];
      zi[j] = -zi[i];
      /* L200: */
    }
  *nbx = nrdy;
  return 0;
}

