/* drref.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 *    drref calcule la forme echelon d'une matrice 
 *
 *     subroutine drref(a,lda,m,n,eps) 
 *    double precision a(lda,n),eps 
 *    int lda,m,n 
 * 
 *    a: tableau contenant a l'appel la matrice dont on veut determiner 
 *       la forme echelon, apres execution a contient la forme echelon 
 *    lda : nombre de ligne du tableau a dans le programme appelant 
 *    m : nombre de ligne de la matrice a 
 *    n : nombre de colonnes de a matrice a 
 *    eps : tolerance. les reels  inferieurs a 2*max(m,n)*eps*n1(a), 
 *          ou n1(a) est la norme 1 de a ,sont consideres comme nuls 
 * 
 *    si l'on veut la transformation appliquee,appeler drref avec 
 *    la matrice obtenue en concatenant l'identite a la matrice a 
 *    en rajoutant des colonnes. 
 *
 *    Copyright INRIA 
 */


static int c__1 = 1;
static double c_b7 = 0.;
static int c__0 = 0;

int nsp_ctrlpack_drref (double *a, int *lda, int *m, int *n, double *eps)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int i__, j, k, l;
  double t;
  double tol;

  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  tol = 0.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      /*Computing MAX 
       */
      d__1 = tol, d__2 = C2F (dasum) (m, &a[j * a_dim1 + 1], &c__1);
      tol = Max (d__1, d__2);
      /* L10: */
    }
  tol = *eps * (double) (Max (*m, *n) << 1) * tol;
  k = 1;
  l = 1;
 L20:
  if (k > *m || l > *n)
    {
      return 0;
    }
  i__1 = *m - k + 1;
  i__ = C2F (idamax) (&i__1, &a[k + l * a_dim1], &c__1) + k - 1;
  if ((d__1 = a[i__ + l * a_dim1], Abs (d__1)) > tol)
    {
      goto L30;
    }
  i__1 = *m - k + 1;
  C2F (dcopy) (&i__1, &c_b7, &c__0, &a[k + l * a_dim1], &c__1);
  ++l;
  goto L20;
 L30:
  i__1 = *n - l + 1;
  C2F (dswap) (&i__1, &a[i__ + l * a_dim1], lda, &a[k + l * a_dim1], lda);
  t = 1. / a[k + l * a_dim1];
  i__1 = *n - l + 1;
  C2F (dscal) (&i__1, &t, &a[k + l * a_dim1], lda);
  a[k + l * a_dim1] = 1.;
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      t = -a[i__ + l * a_dim1];
      if (i__ != k)
	{
	  i__2 = *n - l + 1;
	  C2F (daxpy) (&i__2, &t, &a[k + l * a_dim1], lda,
		       &a[i__ + l * a_dim1], lda);
	}
      /* L40: */
    }
  ++k;
  ++l;
  goto L20;
}				/* drref_ */
