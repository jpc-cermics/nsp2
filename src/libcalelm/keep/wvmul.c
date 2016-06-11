/* wvmul.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*/MEMBR ADD NAME=WVMUL,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_calpack_wvmul (int *n, double *dxr, double *dxi, int *incx, double *dyr,
		   double *dyi, int *incy)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, ix, iy;
  double sr;

  /*!but 
   * 
   *    etant donne un vecteur dx et un vecteur dy complexes, 
   *     cette subroutine fait: 
   *                   dy = dy * dx 
   *    quand les deux increments sont egaux a un, cette 
   *    subroutine emploie des boucles "epanouis". dans le cas ou 
   *    les increments sont negatifs, cette subroutine prend 
   *    les composantes en ordre inverse. 
   * 
   *!liste d'appel 
   * 
   *    subroutine wvmul(n,dxr,dxi,incx,dyr,dyi,incy) 
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
   * 
   */
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
      sr = dyr[iy];
      dyr[iy] = sr * dxr[ix] - dyi[iy] * dxi[ix];
      dyi[iy] = sr * dxi[ix] + dyi[iy] * dxr[ix];
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
      sr = dyr[i__];
      dyr[i__] = sr * dxr[i__] - dyi[i__] * dxi[i__];
      dyi[i__] = sr * dxi[i__] + dyi[i__] * dxr[i__];
      /* L30: */
    }
  /* 
   */
  return 0;
}				/* wvmul_ */
