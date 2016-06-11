#include "calpack.h"

  /*
   *    cette fonction determine la addition des normes l1 des 
   *    composantes d'un vecteur complexe dont les parties reelles 
   *    sont rangees dans le vecteur double precision xr et les 
   *    parties imaginaires dans le vecteur double precision xi. 
   * 
   * 
   *     double precision function wasum(n,xr,xi,incx) 
   *     n: entier, taille du vecteur traite 
   *     xr, xi: vecteurs double precision contenant, 
   *       respectivement,  les parties reelles et imaginaires du 
   *       vecteur traite. 
   *     incx: increment entre deux composantes consecutives des 
   *       vecteurs xr ou xi. 
   * 
   *    cleve moler.- mathlab. 
   */

double nsp_calpack_wasum (int *n, double *xr, double *xi, int *incx)
{
  int i__1;
  double ret_val, d__1, d__2;
  int i__;
  double s;
  int ix;
  --xi;
  --xr;
  s = 0.;
  if (*n <= 0)
    {
      goto L20;
    }
  ix = 1;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      s = s + (d__1 = xr[ix], Abs (d__1)) + (d__2 = xi[ix], Abs (d__2));
      ix += *incx;
      /* L10: */
    }
L20:
  ret_val = s;
  return ret_val;
}

