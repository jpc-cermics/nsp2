
#include "calpack.h"

/*
 * 
 *    cette subroutine ajoute le vecteur x, de taille n, au 
 *    vecteur y. (y=y+x) 
 *    dans le cas de deux increments egaux a 1, cette subroutine 
 *    emploie des boucles "epanouies". 
 *    dans le cas ou les increments sont negatifs cette 
 *    subroutine prend les composantes en ordre inverse. 
 *
 *     subroutine  dadd(n,dx,incx,dy,incy) 
 *    n: taille du vecteur x 
 *    dx: vecteur double precision contenant x 
 *    dy: vecteur double precision contenant y 
 *    incx, incy: increments entre les composantes des vecteurs. 
 *    Copyright INRIA 
 */

int nsp_calpack_dadd (int *n, double *dx, int *incx, double *dy, int *incy)
{
  int i__1;
  int i__, ix, iy;

  --dy;
  --dx;

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
      dy[iy] = dx[ix] + dy[iy];
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
      dy[i__] = dx[i__] + dy[i__];
      /* L30: */
    }
  /* 
   */
  return 0;
}				/* dadd_ */
