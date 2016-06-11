/* dsum.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*/MEMBR ADD NAME=DSUM,SSI=0 
 *    Copyright INRIA 
 */
double nsp_calpack_dsum (int *n, double *dx, int *incx)
{
  /* System generated locals */
  int i__1, i__2;
  double ret_val;

  /* Local variables */
  int i__;
  double dtemp;
  int nincx;

  /* 
   *!but 
   * 
   *    cette fonction donne la somme des n composantes d'un vecteur dx. 
   * 
   *!liste d'appel 
   * 
   *    double precision function dsum(n,dx,incx) 
   * 
   *    n: entier, taille du vecteur dx. 
   * 
   *    dx: double precision, vecteur dont on veut la somme 
   * 
   *    incx: increment entre deux composantes consecutives de dx. 
   * 
   *!auteur 
   * 
   *    serge Steer ,inria 86 
   *! 
   * 
   * 
   */
  /* Parameter adjustments */
  --dx;

  /* Function Body */
  ret_val = 0.;
  dtemp = 0.;
  if (*n <= 0)
    {
      return ret_val;
    }
  if (*incx == 1)
    {
      goto L20;
    }
  /* 
   *code for increment not equal to 1 
   * 
   */
  nincx = *n * *incx;
  i__1 = nincx;
  i__2 = *incx;
  for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2)
    {
      dtemp += dx[i__];
      /* L10: */
    }
  ret_val = dtemp;
  return ret_val;
  /* 
   *code for increment equal to 1 
   * 
   */
L20:
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      dtemp += dx[i__];
      /* L30: */
    }
  ret_val = dtemp;
  /* 
   */
  return ret_val;
}				/* dsum_ */
