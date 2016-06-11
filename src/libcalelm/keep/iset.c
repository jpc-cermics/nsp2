/* iset.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*/MEMBR ADD NAME=ISET,SSI=0 
 *    Copyright INRIA 
 */
int nsp_calpack_iset (int *n, int *dx, int *dy, int *incy)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, iy;

  /*!but 
   *    iset affecte un entier a tous les elements d'un vecteur 
   *!liste d'appel 
   *    subroutine iset(n,dx,dy,incy) 
   *    int dx,dy(n*incy) 
   *    int n,incy 
   * 
   *    n : nombre d'elements du vecteur dy 
   *    dx : scalaire a affecter 
   *    dy : tableau contenant le vecteur 
   *    incy : increment entre deux elements consecutifs du vecteur y 
   *             dans le tableau dy 
   *! 
   * 
   */
  /* Parameter adjustments */
  --dy;

  /* Function Body */
  if (*n <= 0)
    {
      return 0;
    }
  iy = 1;
  if (*incy < 0)
    {
      iy = (-(*n) + 1) * *incy + 1;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      dy[iy] = *dx;
      iy += *incy;
      /* L10: */
    }
  return 0;
}				/* iset_ */
