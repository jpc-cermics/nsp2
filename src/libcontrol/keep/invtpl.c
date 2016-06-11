/* invtpl.f -- translated by f2c (version 19961017).
 *
 *
 */

/*ce sous programme calcule les coefficients de l'inverse d'une matrice 
 *de toeplitz triangulaire bande 
 * 
 *t:vecteur de taille m contenant les elements des la matrice de toeplit 
 *  ranges par sous diagonales et tel que t(m) soit l'element de la diag 
 *n:dimension de la matrice de toeplitz 
 *m:largeur de bande m.le.n 
 *tm1 vecteur de dimension n  contenant les coeff de l'inverse ranges 
 *    comme precedemment. 
 *ierr:code d'erreur si ierr.ne.0 matrice non inversible avec l'ordinate 
 * 
 *    Copyright INRIA 
 */

#include "ctrlpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c_n1 = -1;

int nsp_ctrlpack_invtpl (double *t, int *n, int *m, double *tm1, int *ierr)
{
  /* Initialized data */

  static double amax = 1e30;

  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int i__, it;
  double tm;
  int nmm;

  /* Parameter adjustments */
  --tm1;
  --t;

  /* Function Body */
  *ierr = 1;
  if (*m <= 1)
    {
      goto L50;
    }
  tm = amax;
  if ((d__1 = t[*m], Abs (d__1)) < 1.)
    {
      tm = (d__2 = t[*m], Abs (d__2)) * amax;
    }
  tm1[*n] = 1.;
  it = *n;
  i__1 = *m;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      --it;
      i__2 = i__ - 1;
      tm1[it] =
	-C2F (ddot) (&i__2, &t[*m + 1 - i__], &c__1, &tm1[it + 1], &c_n1);
      /*test d'overflow 
       */
      if ((d__1 = tm1[it], Abs (d__1)) >= tm)
	{
	  return 0;
	}
      /*division 
       */
      /* L10: */
      tm1[it] /= t[*m];
    }
  if (*n <= *m)
    {
      goto L30;
    }
  nmm = *n - *m;
  i__1 = nmm;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      --it;
      i__2 = *m - 1;
      tm1[it] = -C2F (ddot) (&i__2, &t[1], &c__1, &tm1[nmm - i__ + 2], &c_n1);
      if ((d__1 = tm1[it], Abs (d__1)) >= tm)
	{
	  return 0;
	}
      /* L20: */
      tm1[it] /= t[*m];
    }
  /*normalisation 
   */
 L30:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if ((d__1 = tm1[i__], Abs (d__1)) > tm)
	{
	  return 0;
	}
      /* L40: */
      tm1[i__] /= t[*m];
    }
  *ierr = 0;
  return 0;
 L50:
  if (t[*m] == 0.)
    {
      return 0;
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L60: */
      tm1[i__] = 0.;
    }
  tm1[1] = 1. / t[*m];
  *ierr = 0;
  return 0;
}				/* invtpl_ */
