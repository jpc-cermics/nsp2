/* wrref.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

static int c__1 = 1;
static double c_b7 = 0.;
static double c_b11 = 1.;

/*/MEMBR ADD NAME=WRREF,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_ctrlpack_wrref (double *ar, double *ai, int *lda, int *m, int *n,
		    double *eps)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int i__, j, k, l;
  double ti, tr;
  double tol;

  /*!but 
   *    wrref calcule la forme echelon d'une matrice a coeff complexes 
   *!liste d'appel 
   * 
   *    subroutine wrref(ar,ai,lda,m,n,eps) 
   *    double precision ar(lda,n),ai(lda,n),eps 
   *    int lda,m,n 
   * 
   *    ar,ai : tableaux contenant a l'appel les parties reelles et 
   *       complexes  de la matrice dont on veut determiner la forme 
   *       echelon, apres execution a contient la forme echelon 
   *    lda : nombre de ligne du tableau a dans le programme appelant 
   *    m : nombre de ligne de la matrice a 
   *    n : nombre de colonnes de a matrice a 
   *    eps : tolerance. les reels  inferieurs a 2*max(m,n)*eps*n1(a), 
   *          ou n1(a) est la norme 1 de a ,sont consideres comme nuls 
   * 
   *    si l'on veut la transformation appliquee,appeler wrref avec 
   *    la matrice obtenue en concatenant l'identite a la matrice a 
   *    en rajoutant des colonnes. 
   *!sous programmes appeles 
   *    iwamax wcopy wswap wscal wasum waxpy (blas.extensions) 
   *    dset (blas) 
   *    dble abs Max(fortran) 
   *! 
   *! 
   */
  /* Parameter adjustments */
  ai_dim1 = *lda;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *lda;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;

  /* Function Body */
  tol = 0.;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      /*Computing MAX 
       */
      d__1 = tol, d__2 =
	nsp_calpack_wasum (m, &ar[j * ar_dim1 + 1], &ai[j * ai_dim1 + 1],
			   &c__1);
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
  i__ =
    nsp_calpack_iwamax (&i__1, &ar[k + l * ar_dim1], &ai[k + l * ai_dim1],
			&c__1) + k - 1;
  if ((d__1 = ar[i__ + l * ar_dim1], Abs (d__1)) + (d__2 =
						    ai[i__ + l * ai_dim1],
						    Abs (d__2)) > tol)
    {
      goto L30;
    }
  i__1 = *m - k + 1;
  nsp_dset (&i__1, &c_b7, &ar[k + l * ar_dim1], &c__1);
  i__1 = *m - k + 1;
  nsp_dset (&i__1, &c_b7, &ai[k + l * ai_dim1], &c__1);
  ++l;
  goto L20;
 L30:
  i__1 = *n - l + 1;
  nsp_calpack_wswap (&i__1, &ar[i__ + l * ar_dim1], &ai[i__ + l * ai_dim1],
		     lda, &ar[k + l * ar_dim1], &ai[k + l * ai_dim1], lda);
  nsp_calpack_wdiv (&c_b11, &c_b7, &ar[k + l * ar_dim1],
		    &ai[k + l * ai_dim1], &tr, &ti);
  i__1 = *n - l + 1;
  nsp_calpack_wscal (&i__1, &tr, &ti, &ar[k + l * ar_dim1],
		     &ai[k + l * ai_dim1], lda);
  ar[k + l * ar_dim1] = 1.;
  ai[k + l * ai_dim1] = 0.;
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      tr = -ar[i__ + l * ar_dim1];
      ti = -ai[i__ + l * ai_dim1];
      if (i__ != k)
	{
	  i__2 = *n - l + 1;
	  nsp_calpack_waxpy (&i__2, &tr, &ti, &ar[k + l * ar_dim1],
			     &ai[k + l * ai_dim1], lda,
			     &ar[i__ + l * ar_dim1], &ai[i__ + l * ai_dim1],
			     lda);
	}
      /* L40: */
    }
  ++k;
  ++l;
  goto L20;
}				/* wrref_ */
