#include "calpack.h"

double
nsp_calpack_wdotcr (int *n, double *xr, double *xi, int *incx, double *yr,
		    double *yi, int *incy)
{
  int i__1;
  double ret_val;

  int i__;
  double s;
  int ix, iy;

  --yi;
  --yr;
  --xi;
  --xr;

  s = 0.;
  if (*n <= 0)
    {
      goto L20;
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
      s = s + xr[ix] * yr[iy] + xi[ix] * yi[iy];
      ix += *incx;
      iy += *incy;
      /* L10: */
    }
L20:
  ret_val = s;
  return ret_val;
}

