/* dvmul.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int nsp_calpack_dvmul (int *n, double *dx, int *incx, double *dy, int *incy)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, ix, iy;

  /*!but 
   * 
   *    etant donne un vecteur dx et un vecteur dy, 
   *     cette subroutine fait: 
   *                   dy = dy * dx 
   *    quand les deux increments sont egaux a un, cette 
   *    subroutine emploie des boucles "epanouis". dans le cas ou 
   *    les increments sont negatifs, cette subroutine prend 
   *    les composantes en ordre inverse. 
   * 
   *!liste d'appel 
   * 
   *    subroutine dvmul(n,dx,incx,dy,incy) 
   * 
   *    dy, dx: vecteurs double precision. 
   * 
   *    incx, incy: increments entre deux composantes succesives 
   *    des vecteurs. 
   * 
   *!auteur 
   * 
   *    serge steer inria 
   *! 
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
      dy[iy] *= dx[ix];
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
      dy[i__] *= dx[i__];
      /* L30: */
    }
  /* 
   */
  return 0;
}				/* dvmul_ */
