#include "calpack.h"

  /*
   *    cette subroutine wscal multiplie une contante complexe s 
   *    (dont la partie reelle est rangee dans sr et la partie 
   *    imaginaire dans si) par un vecteur x (dont les parties 
   *    reelles de ses composantes sont rangees dans xr et les 
   *    parties imaginaires dans xi). le resultat reste dans x. 
   *
   *     subroutine wscal(n,sr,si,xr,xi,incx) 
   *    n: entier, taille du vecteur x. 
   *    sr, si: double precision, parties reelle et imaginaire 
   *            de s. 
   *    xr, xi: vecteurs double precision, contiennent, 
   *            respectivement, les parties reelles et imaginaires des 
   *            composants du vecteur x. 
   *    incx: entier, increment entre deux composantes consecutives 
   *          de x. 
   * 
   *    cleve moler. 
   */

int
nsp_calpack_wscal (int *n, double *sr, double *si, double *xr, double *xi,
		   int *incx)
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
      nsp_calpack_wmul (sr, si, &xr[ix], &xi[ix], &xr[ix], &xi[ix]);
      ix += *incx;
      /* L10: */
    }
  return 0;
}

