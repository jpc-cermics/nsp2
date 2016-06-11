/* waxpy.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

  /*
   *    cette subroutine multiplie la constante complexe double 
   *    precision s (dont la partie reelle est sr et la partie 
   *    imaginaire si) par le vecteur complexe double precision 
   *    (dont les parties reelles de ses composantes sont dans 
   *    le vecteur double precision xr). le produit ainsi 
   *    obtenu est additionne au vecteur complexe y (dont les 
   *    parties reelles de ses composantes sont rangees dans le 
   *    vecteur double precision yr et les parties imaginaires 
   *    dans le vecteur double precision yr). le resultat de 
   *    l'addition reste dans y. 
   * 
   *     subroutine waxpy(n,sr,si,xr,xi,incx,yr,yi,incy) 
   *    n: entier, taille des vecteurs traites 
   *    sr, si: double precision, parties reel et imaginaire de s 
   *    xr, xi: vecteurs double precision, parties rellees et 
   *    imaginaires, respectivement du vecteur complexe x. 
   *    yr, yi: vecteurs double precision, parties rellees et 
   *    imaginaires, respectivement du vecteur complexe y. 
   *    incx, incy: entiers, increments entre deux composantes 
   *    successives des vecteurs x et y. 
   *
   *    cleve moler.- mathlab. 
   */

int
nsp_calpack_waxpy (int *n, double *sr, double *si, double *xr, double *xi,
		   int *incx, double *yr, double *yi, int *incy)
{
  int i__1;
  int i__, ix, iy;

  --yi;
  --yr;
  --xi;
  --xr;

  if (*n <= 0)
    {
      return 0;
    }
  if (*sr == 0. && *si == 0.)
    {
      return 0;
    }
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
      yr[iy] = yr[iy] + *sr * xr[ix] - *si * xi[ix];
      yi[iy] = yi[iy] + *sr * xi[ix] + *si * xr[ix];
      ix += *incx;
      iy += *incy;
      /* L10: */
    }
  return 0;
}

