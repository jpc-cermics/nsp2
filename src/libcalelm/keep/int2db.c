/* int2db.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int nsp_calpack_int2db (int *n, int *dx, int *incx, double *dy, int *incy)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, ix, iy;

  /*!but 
   * 
   *    cette subroutine traduit un vecteur dx, de taille n, sur un 
   *    vecteur double precision dy. 
   *    dans le cas de deux increments egaux a 1, cette subroutine 
   *    emploie des boucles "epanouies". 
   *    dans le cas ou les increments sont negatifs cette 
   *    subroutine prend les composantes en ordre inverse. 
   * 
   *!liste d'appel 
   * 
   *     subroutine  dcopy(n,dx,incx,dy,incy) 
   * 
   *    n: taille du vecteur dx 
   * 
   *    dx: entier, vecteur "emetteur". 
   * 
   *    dy: double precision, vecteur "recepteur". 
   * 
   *    incx, incy: increments entre les composantes des vecteurs. 
   * 
   *!auteur 
   * 
   *    jack dongarra, linpack, 3/11/78. 
   * 
   *! 
   * 
   *    Copyright INRIA 
   * 
   */
  /* Parameter adjustments */
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
      dy[iy] = (double) dx[ix];
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
      dy[i__] = (double) dx[i__];
      /* L30: */
    }
  /* 
   */
  return 0;
}				/* int2db_ */
