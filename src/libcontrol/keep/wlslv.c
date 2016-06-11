/* wlslv.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Table of constant values */

static int c__0 = 0;
static int c__1 = 1;
static double c_b11 = -1.;

/*/MEMBR ADD NAME=WLSLV,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_ctrlpack_wlslv (double *ar, double *ai, int *na, int *n, double *br,
		    double *bi, int *nb, int *m, double *w, double *rcond,
		    int *ierr, int *job)
{
  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1;

  /* Local variables */
  int j;
  int k1, jb;
  double dti[2], dtr[2];

  /*! purpose 
   *  la factorisation lu de la matrice a si job=0 
   *  la resolution du systeme a*x=b si job=1 
   *  la resolution du systeme x*a=b si job=2 
   *le calcul de l'inverse de a si job=3 
   *!calling sequence 
   * 
   *      subroutine wlslv(ar,ai,na,n,br,bi,nb,m,w,rcond,ierr,job) 
   *ar,ai:tableaux de taille na*n contenant la matrice a 
   *   apres execution a contient la factorisation lu 
   *na:dimensionnement de a dans le programme appelant 
   *n:dimensions de la matrice a 
   *br,bi:tableaux de taille nb*m contenant la matrice b et le resultat x 
   *nb:dimensionnement de b dans le programme appelant 
   *m:nombre de colonnes de b si job=1;ou nombre de ligne si job=2 
   *w:tableau de travail de taille 2*n+adr(n,1) 
   *rcond:reel contenant le conditionnement inverse de a 
   *ierr:indicateur de deroulement 
   *    ierr=0 ok 
   *    ierr=1 rcond=0 
   *    ierr=-1 rcond negligeable 
   *job: 
   *    si a et w n'ont pas ete modifies on peut reentrer dans le 
   *    sous programme avec une nouvelle matrice b (job=-1 ou job=-2) 
   *!sous programmes appeles 
   *    wgeco wgesl wgedi (linpack.extensions) 
   *    dcopy dscal (blas) 
   *    Abs(fortran) 
   *! 
   * 
   * 
   *     iadr(l)=l+l-1 
   * 
   */
  /* Parameter adjustments */
  ai_dim1 = *na;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *na;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
  --br;
  --bi;
  --w;

  /* Function Body */
  k1 = *n / 2 + 1 + 1;
  *ierr = 0;
  if (*job < 0)
    {
      goto L20;
    }
  /*factorisation lu 
   */
  nsp_ctrlpack_wgeco (&ar[ar_offset], &ai[ai_offset], na, n, (int *) &w[1],
		      rcond, &w[k1], &w[k1 + *n]);
  if (*rcond == 0.)
    {
      goto L70;
    }
  if (*rcond + 1. == 1.)
    {
      *ierr = -1;
    }
  if (*job == 0)
    {
      return 0;
    }
  if (*job == 3)
    {
      goto L60;
    }
  /*resolution 
   */
 L20:
  if (Abs (*job) == 2)
    {
      goto L40;
    }
  jb = 1;
  i__1 = *m;
  for (j = 1; j <= i__1; ++j)
    {
      nsp_ctrlpack_wgesl (&ar[ar_offset], &ai[ai_offset], na, n,
			  (int *) &w[1], &br[jb], &bi[jb], &c__0);
      jb += *nb;
      /* L30: */
    }
  return 0;
 L40:
  i__1 = *m;
  for (j = 1; j <= i__1; ++j)
    {
      C2F (dcopy) (n, &br[j], nb, &w[k1], &c__1);
      C2F (dcopy) (n, &bi[j], nb, &w[k1 + *n], &c__1);
      C2F (dscal) (n, &c_b11, &w[k1 + *n], &c__1);
      nsp_ctrlpack_wgesl (&ar[ar_offset], &ai[ai_offset], na, n,
			  (int *) &w[1], &w[k1], &w[k1 + *n], &c__1);
      C2F (dcopy) (n, &w[k1], &c__1, &br[j], nb);
      C2F (dcopy) (n, &w[k1 + *n], &c__1, &bi[j], nb);
      C2F (dscal) (n, &c_b11, &bi[j], nb);
      /* L50: */
    }
  return 0;
 L60:
  nsp_ctrlpack_wgedi (&ar[ar_offset], &ai[ai_offset], na, n, (int *) &w[1],
		      dtr, dti, &w[k1], &w[k1 + *n], &c__1);
  return 0;
 L70:
  *ierr = 1;
  return 0;
}				/* wlslv_ */
