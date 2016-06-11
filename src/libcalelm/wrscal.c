#include "calpack.h"

  /*
   *    cette subroutine calcule le produit d'une constante reelle 
   *    double precision s par un vecteur complexe x, dont les 
   *    reelles de ses composantes sont rangees dans xr et les 
   *    parties imaginaires dans xi. 
   * 
   *     subroutine wrscal(n,s,xr,xi,incx) 
   *    n: entier, longueur du vecteur x. 
   *    s: double precision. the real factor 
   *    xr, xi: doubles precision, parties reelles et imaginaires, 
   *       respectivement, des composantes du vecteur x. 
   *    incx: increment entre deux composantes consecutives de x. 
   *
   *    cleve moler
   */

int nsp_calpack_wrscal (int *n, double *s, double *xr, double *xi, int *incx)
{
  int i__1;
  int i__, ix;

  --xi;
  --xr;

  if (*n <= 0)
    {
      return 0;
    }
  ix = 1;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      xr[ix] = *s * xr[ix];
      xi[ix] = *s * xi[ix];
      ix += *incx;
    }
  return 0;
}

