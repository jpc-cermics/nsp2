/* iwamax.f -- translated by f2c (version 19961017).
 *
 *
 */

  /*!but 
   * 
   *    la fonction iwamax determine l'indice de la composante 
   *    de plus grande norme l1 d'un vecteur complexe dont les 
   *    parties reelles des composantes sont rangees dans le 
   *    vecteur double precision xr et les parties imaginaires 
   *    dans le vecteur xi. 
   *    int function iwamax(n,xr,xi,incx) 
   * 
   *    n: taille du vecteur 
   *    xr, xi: vecteurs double precision qui contiennent, 
   *            respectivement, les parties reelles et imaginaires 
   *            des composantes du vecteur a traiter. 
   *    incx: increment entre deux elements consecitifs des 
   *          vecteurs xr ou xi. 
   * 
   *    cleve moler.- mathlab. 
   */

#include "calpack.h"

int nsp_calpack_iwamax (int *n, double *xr, double *xi, int *incx)
{
  /* System generated locals */
  int ret_val, i__1;
  double d__1, d__2;

  /* Local variables */
  int i__, k;
  double p, s;
  int ix;

  /* Parameter adjustments */
  --xi;
  --xr;

  /* Function Body */
  k = 0;
  if (*n <= 0)
    {
      goto L20;
    }
  k = 1;
  s = 0.;
  ix = 1;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      p = (d__1 = xr[ix], Abs (d__1)) + (d__2 = xi[ix], Abs (d__2));
      if (p > s)
	{
	  k = i__;
	}
      if (p > s)
	{
	  s = p;
	}
      ix += *incx;
      /* L10: */
    }
L20:
  ret_val = k;
  return ret_val;
}				/* iwamax_ */
