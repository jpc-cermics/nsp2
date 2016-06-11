/* bezous.f -- translated by f2c (version 19961017). */

#include "ctrlpack.h"

/*
 * ce sous programme calcule le second coefficient de bezout du 
 * couple (a,at) si at=(z**n)*a(1/z) et a est un polynome de degre 
 * n ayant tous ces poles dans le disque unite.;a et at sont donc 
 * premiers 
 *
 * 
 *      subroutine bezous(a,n,c,w,ierr) 
 * 
 *a:tableau de taille n+1 contenant les coefficients du polynome a 
 *   ranges par puissances croissantes. 
 *n:degre de a 
 *c:tableau de taille n contenant apres execution le facteur de bezout 
 *   les coefficients etant ranges par puissances croissantes 
 *w:tableau de travail de taille n*n+(n+1)/2 
 *ierr:indicateur d'erreur: 
 *     si ierr=0 ok 
 *     si ierr=1 a est de degre inferieur a n 
 *     si ierr=2 a et at non premiers ou calcul numeriquement faux. 
 * 
 *
 *la methode utilisee ici est de resoudre le systeme lineaire associe 
 *a la relation de bezout: a*b+at*c=1 c'est a dire: 
 *     [x' y'] [e1]  [b] 
 *     [     ].[  ] =[ ] 
 *     [y  x ] [ 0]  [c] 
 *ou x et y sont des matrice n*n toeplitz triangulaires superieures 
 *la premiere ligne de x est formee des n premiers coefficient de a 
 *la premiere ligne de y  des n derniers ranges en ordre inverse 
 *c est alors solution du systeme : (y'-x'*(y**-1)*x)*c=e1 
 *
 *    serge Steer Inria 1983 
 *    Copyright INRIA 
 */

static int c_n1 = -1;
static int c__1 = 1;

int nsp_ctrlpack_bezous (double *a, int *n, double *c__, double *w, int *ierr)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  int i__, j;
  double rcond;
  int i1, i2, jj;

  /* Parameter adjustments */
  --w;
  --c__;
  --a;

  /* Function Body */
  nsp_ctrlpack_invtpl (&a[2], n, n, &c__[1], ierr);
  if (*ierr != 0)
    {
      goto L70;
    }
  *ierr = 0;
  /*c contient les coeff de la premiere ligne de y**-1 dans l'ordre invers 
   * 
   *calcul de la premiere ligne de la matrice de toeplitz:-(y**-1)*x 
   */
  j = *n + 1;
  i__1 = *n;
  for (jj = 1; jj <= i__1; ++jj)
    {
      --j;
      c__[jj] = -C2F (ddot) (&j, &a[1], &c_n1, &c__[jj], &c_n1);
      /* L10: */
    }
  /*c contient la premiere ligne du produit ranges dans l'ordre inverse 
   * 
   *calcul de x'*(-(y**-1)*x) 
   */
  i2 = 0;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      w[i2 + i__] = a[i__] * c__[*n];
      /* L20: */
    }
  if (*n == 1)
    {
      goto L45;
    }
  i__1 = *n;
  for (j = 2; j <= i__1; ++j)
    {
      i1 = i2;
      i2 += *n;
      w[i2 + 1] = a[1] * c__[*n + 1 - j];
      i__2 = *n;
      for (i__ = 2; i__ <= i__2; ++i__)
	{
	  w[i2 + i__] = w[i1 + i__ - 1] + a[i__] * c__[*n + 1 - j];
	  /* L30: */
	}
      /* L40: */
    }
  /*calcul de y'+w 
   */
 L45:
  i1 = -(*n);
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      c__[j] = 0.;
      i1 += *n;
      i__2 = *n;
      for (i__ = j; i__ <= i__2; ++i__)
	{
	  w[i1 + i__] += a[*n + 1 + j - i__];
	  /* L50: */
	}
      /* L60: */
    }
  c__[1] = 1.;
  /*w contient la matrice du systeme lineaire et c le second menbre 
   * 
   *resolution 
   */
  nsp_ctrlpack_dlslv (&w[1], n, n, &c__[1], n, &c__1, &w[*n * *n + 1], &rcond,
		      ierr, &c__1);
  if (*ierr != 0)
    {
      goto L80;
    }
  return 0;
 L70:
  *ierr = 1;
  return 0;
 L80:
  *ierr = 2;
  return 0;
}				/* bezous_ */
