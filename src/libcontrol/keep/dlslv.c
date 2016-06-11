/* dlslv.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 *     ce sous programme effectue: 
 *       la factorisation lu de la matrice a si job=0 
 *       la resolution du systeme a*x=b si job=1 
 *       la resolution du systeme x*a=b si job=2 
 *       l'inversion de a si job=3 
 * 
 *          subroutine dlslv(a,na,n,b,nb,m,w,rcond,ierr,job) 
 *     a:tableau de taille na*n contenant la matrice a 
 *        apres execution a contient la factorisation lu 
 *     na:dimensionnement de a dans le programme appelant 
 *     n:dimensions de la matrice a 
 *     b:tableau de taille nb*m contenant la matrice b et le resultat x 
 *     nb:dimensionnement de b dans le programme appelant 
 *     m:nombre de colonnes de b si job=1 ;ou nombre de ligne si job=2 
 *     w:tableau de travail de taille n+adr(n,1) 
 *     rcond:reel contenant le conditionnement inverse de a 
 *     ierr:indicateur de deroulement 
 *         ierr=0 ok 
 *         ierr=1 rcond=0 
 *         ierr=-1 rcond negligeable 
 *     job: 
 *    si a et w n'ont pas ete modifies on peut reentrer dans le 
 *    sous programme avec une nouvelle matrice b (job=-1 ou job=-2) 
 * 
 * S Steer Copyright INRIA 
 */

static int c__0 = 0;
static int c__1 = 1;

int
nsp_ctrlpack_dlslv (double *a, int *na, int *n, double *b, int *nb, int *m,
		    double *w, double *rcond, int *ierr, int *job)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1;

  /* Local variables */
  int j;
  int k1, jb;
  double dt[2];

  /* Parameter adjustments */
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --b;
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
  nsp_ctrlpack_dgeco (&a[a_offset], na, n, (int *) &w[1], rcond, &w[k1]);
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
      nsp_ctrlpack_dgesl (&a[a_offset], na, n, (int *) &w[1], &b[jb], &c__0);
      jb += *nb;
      /* L30: */
    }
  return 0;
 L40:
  i__1 = *m;
  for (j = 1; j <= i__1; ++j)
    {
      C2F (dcopy) (n, &b[j], nb, &w[k1], &c__1);
      nsp_ctrlpack_dgesl (&a[a_offset], na, n, (int *) &w[1], &w[k1], &c__1);
      C2F (dcopy) (n, &w[k1], &c__1, &b[j], nb);
      /* L50: */
    }
  return 0;
 L60:
  nsp_ctrlpack_dgedi (&a[a_offset], na, n, (int *) &w[1], dt, &w[k1], &c__1);
  return 0;
 L70:
  *ierr = 1;
  return 0;
}				/* dlslv_ */
