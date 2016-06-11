/* wcopy.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

  /*
   *    copies a vector, x, to a vector, y. 
   *    uses unrolled loops for increments equal to one. 
   *
   *    jack dongarra, linpack, 3/11/78. 
   */

int
nsp_calpack_wcopy (int *n, double *dxr, double *dxi, int *incx, double *dyr,
		   double *dyi, int *incy)
{
  int i__1;
  int i__, ix, iy;

  /* Parameter adjustments */
  --dyi;
  --dyr;
  --dxi;
  --dxr;

  /* Function Body */
  if (*n <= 0)
    {
      return 0;
    }
  if (*incx == 1 && *incy == 1)
    {
      goto L20;
    }
  /* 
   *code for unequal increments or equal increments not equal to 1 
   * 
   */
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
      dyr[iy] = dxr[ix];
      dyi[iy] = dxi[ix];
      ix += *incx;
      iy += *incy;
      /* L10: */
    }
  return 0;
  /* 
   *code for both increments equal to 1 
   * 
   */
L20:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      dyr[i__] = dxr[i__];
      dyi[i__] = dxi[i__];
      /* L30: */
    }
  /* 
   */
  return 0;
}				/* wcopy_ */
