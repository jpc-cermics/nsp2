#include "calpack.h"

  /*
   *    cette fonction wnrm2 determine la norme l2 d'un vecteur 
   *    complexe double precision x, dont les parties reelles 
   *    sont rangees dans le vecteur double precision xr et 
   *    les parties imaginaires sont rangees dans le vecteur 
   *    double precision xi. 
   * 
   *     double precision function wnrm2(n,xr,xi,incx) 
   *     n: entier, taille du vecteur traite. 
   *     xr, xi: vecteurs double precision, parties reelles et 
   *             imaginaires, respectivement du vecteur x. 
   *     incx: increment entre deux composantes consecutives du 
   *           vecteur x. 
   * 
   *    cleve moler.
   */

double nsp_calpack_wnrm2 (int *n, double *xr, double *xi, int *incx)
{
  int i__1;
  double ret_val;
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
      s = nsp_calpack_pythag (&s, &xr[ix]);
      s = nsp_calpack_pythag (&s, &xi[ix]);
      ix += *incx;
      /* L10: */
    }
L20:
  ret_val = s;
  return ret_val;
}

