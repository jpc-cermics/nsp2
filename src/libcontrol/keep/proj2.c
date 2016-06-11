/* proj2.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Table of constant values */

static double c_b3 = 0.;
static int c__1 = 1;

/*/MEMBR ADD NAME=PROJ2,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_ctrlpack_proj2 (double *f, int *nn, double *am, int *n, int *np1,
		    int *np2, double *pf, double *w)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  int i__, j;
  double an;
  int nb, nf, np, iw;
  double wn;

  /*ce sous programme calcule les produits scalaires: 
   *<f,(z**(n-v)/am>=pf(np+1-v)   v=np1...np2  ;np=np2-np1+1 
   *les pf(v) sont les dernieres valeurs de la filtree de f(nn+1-i) par 
   *(z**n-1)/am,am est un polynome de degre n range par puissance 
   *croissante 
   * 
   *w:tableau de travail de taille n 
   * 
   * 
   *w contient l'etat du filtre 
   * 
   */
  /* Parameter adjustments */
  --f;
  --am;
  --w;
  --pf;

  /* Function Body */
  np = *np2;
  if (*np1 > 1)
    {
      np = *np2 - *np1 + 1;
    }
  /* 
   */
  nf = 0;
  nb = *nn - np;
  an = am[*n + 1];
  if (*n == 1)
    {
      goto L50;
    }
  nsp_dset (n, &c_b3, &w[1], &c__1);
  i__1 = nb;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      wn = w[*n] / an;
      ++nf;
      iw = *n + 1;
      i__2 = *n;
      for (j = 2; j <= i__2; ++j)
	{
	  --iw;
	  w[iw] = w[iw - 1] - am[iw] * wn;
	  /* L10: */
	}
      w[1] = -am[1] * wn;
      w[*n] += f[nf];
      /* L20: */
    }
  /*les n valeurs suivantes de la sortie du filtre donnent les produits 
   *scalaires 
   */
  i__1 = np;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      wn = w[*n] / an;
      iw = *n + 1;
      ++nf;
      i__2 = *n;
      for (j = 2; j <= i__2; ++j)
	{
	  --iw;
	  w[iw] = w[iw - 1] - am[iw] * wn;
	  /* L30: */
	}
      w[1] = -am[1] * wn;
      w[*n] += f[nf];
      pf[i__] = w[*n] / an;
      /* L31: */
    }
  if (*np1 >= 1)
    {
      return 0;
    }
  i__1 = *np2 - *np1 + 1;
  for (i__ = np + 1; i__ <= i__1; ++i__)
    {
      wn = w[*n] / an;
      iw = *n + 1;
      i__2 = *n;
      for (j = 2; j <= i__2; ++j)
	{
	  --iw;
	  w[iw] = w[iw - 1] - am[iw] * wn;
	  /* L40: */
	}
      w[1] = -am[1] * wn;
      pf[i__] = w[*n] / an;
      /* L41: */
    }
  return 0;
  /*cas  particulier n=1 
   */
 L50:
  wn = 0.;
  i__1 = nb;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++nf;
      wn = -am[1] / an * wn + f[nf];
      /* L60: */
    }
  i__1 = np;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ++nf;
      wn = -am[1] / an * wn + f[nf];
      pf[i__] = wn / an;
      /* L70: */
    }
  if (*np1 >= 1)
    {
      return 0;
    }
  i__1 = *np2 - *np1 + 1;
  for (i__ = np + 1; i__ <= i__1; ++i__)
    {
      ++nf;
      wn = -am[1] / an * wn;
      pf[i__] = wn / an;
      /* L71: */
    }
  return 0;
}				/* proj2_ */
