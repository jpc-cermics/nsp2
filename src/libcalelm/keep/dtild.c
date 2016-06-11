/* dtild.f -- translated by f2c (version 19961017).
 * Serge Steer Inria 1986 Copyright INRIA 
 */

#include "calpack.h"

/*!but 
 *  reverses the order of the elements of x 
 *     subroutine  dtild(n,x,incx) 
 *    n: taille du vecteur dx 
 *    x: double precision, vecteur 
 *    incx: increment entre les composantes du vecteur. 
 */

int nsp_calpack_dtild (int *n, double *x, int *incx)
{
  int i__1;
  int i__, i1, i2;
  double xx;

  --x;

  /* Function Body */
  if (*n <= 1)
    {
      return 0;
    }
  i1 = 1;
  i2 = *n * *incx;
  i__1 = *n / 2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      xx = x[i1];
      x[i1] = x[i2];
      x[i2] = xx;
      i1 += *incx;
      i2 -= *incx;
      /* L10: */
    }
  return 0;
}				/* dtild_ */
