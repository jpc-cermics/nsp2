/* icopy.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*/MEMBR ADD NAME=ICOPY,SSI=0 
 *    Copyright INRIA 
 */
int nsp_calpack_icopy (int *n, int *dx, int *incx, int *dy, int *incy)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, ix, iy;

  /*!but 
   * 
   *    cette subroutine copie un vecteur dx, de taille n, sur un 
   *    vecteur dy. 
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
   *    dx: int, vecteur "emetteur". 
   * 
   *    dy: int, vecteur "recepteur". 
   * 
   *    incx, incy: increments entre les composantes des vecteurs. 
   * 
   *!auteur 
   * 
   *    jack dongarra, linpack, 3/11/78. 
   * 
   *! 
   * 
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
      dy[iy] = dx[ix];
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
      dy[i__] = dx[i__];
      /* L30: */
    }
  /* 
   */
  return 0;
}				/* icopy_ */
