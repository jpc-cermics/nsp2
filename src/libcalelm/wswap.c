#include "calpack.h"

  /*
   *    cette subroutine wswap echange le contenu de deux vecteurs 
   *    complexes x et y (dont les parties reelles de ses 
   *    composantes sont rangees, respectivement, dans xr et yr 
   *    et les parties imaginaires dans xi et yi). 
   * 
   *     subroutine wswap(n,xr,xi,incx,yr,yi,incy) 
   *    n: entier, taille des vecteur x et y. 
   *    xr, xi: vecteurs double precision, parties reelles et 
   *    imaginaires, respectivement, des composantes du vecteur x. 
   *    incx: entier, increment entre deux elements consecutifs 
   *    de x. 
   *    yr, yi: vecteurs double precision, parties reelles et 
   *    imaginaire, respectivement, des composantes du vecteur y. 
   *    incy: entier, increment entre deux elements consecutifs 
   *    de x. 
   *
   *    cleve moler
   */

int
nsp_calpack_wswap (int *n, double *xr, double *xi, int *incx, double *yr,
		   double *yi, int *incy)
{
  int i__1;
  int i__;
  double t;
  int ix, iy;

  --yi;
  --yr;
  --xi;
  --xr;

  if (*n <= 0)
    {
      return 0;
    }
  ix = 1;
  iy = 1;
  if (*incx < 0)
    {
      ix = (-(*n) + 1) * *incx + 1;
    }
  if (*incy < 0)
    {
      iy = (-(*n) + 1) * *incy + 1;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      t = xr[ix];
      xr[ix] = yr[iy];
      yr[iy] = t;
      t = xi[ix];
      xi[ix] = yi[iy];
      yi[iy] = t;
      ix += *incx;
      iy += *incy;
    }
  return 0;
}

