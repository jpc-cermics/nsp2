#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

static int idegre(double *a, int *majo, int *nvrai);

/*    bcalcul de residus 
 *    calcul de la somme des residus de p/(a.b) 
 *    aux zeros de a 
 *    p=polynome de degre np 
 *    a=                  na 
 *    b=                  nb 
 * 
 *    les zeros de b sont supposes tous differents des 
 *    zeros de a.... 
 * 
 *    a,b et p dimensionnes au moins a leur degre+1 dans le pgm 
 *    appelant. 
 *    rangement par degres croissants. 
 *    v=resultat 
 *    ierr=0 O.K. 
 *    ierr=1 mauvais appel 
 *    si a et b ont une racine commune 
 *    on teste la division par zero par rapport a tol 
 *    principe du calcul:si a (resp b) est une constante on a 
 *    v=p(nb)/b(nb+1)/a(1)    (resp v=p(na)/a(na+1)/b(1) ) 
 *    sinon on remplace p et a par le reste de la division 
 *    euclidienne de p et a par b,puis on inverse les roles 
 *    de a et b en changeant le signe de v. 
 *    on itere jusqu a trouver degre de a ou degre de b=0. 
 *    routines appelees:dpodiv,idegre (bibli blaise Inria) 
 *    Auteur!    F.D. (blaise) 
 */

int nsp_ctrlpack_residu(double *p, int *np, double *a, int *na, double *b, int *nb, double *v, double *tol, int *ierr)
{
  int i__1, i__2;
  int k;
  double r__, b1;
  int nbb, nit, npp;

  /* Parameter adjustments */
  --b;
  --a;
  --p;

  /* Function Body */
  *v = 0.;
  *ierr = 0;
  npp = *np;
  idegre(&a[1], na, na);
  idegre(&b[1], nb, nb);
  if (*na == 0) {
    return 0;
  }
  /* 
   *    b=constante v=... et return 
   */
  if (*nb == 0) {
    b1 = b[1];
    if (b1 == 0.) {
      *ierr = 1;
      return 0;
    }
    if (npp >= *na - 1) {
      *v = p[*na] / a[*na + 1] / b1;
      return 0;
    } else {
      *v = 0.;
      return 0;
    }
  }
  /* 
   *    degre de b >= 1 
   * 
   */
  if (*na <= *np) {
    /*    p=p/a  (reste de la division euclidienne...) 
     */
    nsp_ctrlpack_dpodiv(&p[1], &a[1], np, na);
    i__1 = *na - 1;
    idegre(&p[1], &i__1, np);
  }
  if (*na <= *nb) {
    /*    b=b/a  (reste de la div euclidienne...) 
     */
    nsp_ctrlpack_dpodiv(&b[1], &a[1], nb, na);
    i__1 = *na - 1;
    idegre(&b[1], &i__1, nb);
  }
  /*    ici nb=degre de b < na=degre de a 
   *    et np=degre de p < na=degre de a 
   *    si degre de a=1 (b et p =cstes) v=... et return 
   */
  if (*na == 1) {
    b1 = b[1];
    if ( Abs(b1) <= *tol) {
      *ierr = 1;
      return 0;
    }
    *v = p[*na] / a[*na + 1] / b1;
    return 0;
  }
  /* 
   *    si degre de b=0 v=... et return 
   *Computing MIN 
   */
  i__2 = *na - 1;
  i__1 = Min(i__2,*nb);
  idegre(&b[1], &i__1, nb);
  if (*nb == 0) {
    b1 = b[1];
    if (Abs(b1) <= *tol) {
      *ierr = 1;
      return 0;
    }
    if (npp >= *na - 1) {
      *v = p[*na] / a[*na + 1] / b1;
      return 0;
    } else {
      *v = 0.;
      return 0;
    }
  }
  /*    si degre de b>0 on itere 
   */
  nit = 0;
 L20:
  if (nit >= 1) {
    *na = nbb;
  }
  ++nit;
  nbb = *nb;
  /*    a=a/b   (reste de la division euclidienne...) 
   *    p=p/b   (reste de la division euclidienne...) 
   * 
   */
  nsp_ctrlpack_dpodiv(&a[1], &b[1], na, nb);
  i__1 = *nb - 1;
  idegre(&a[1], &i__1, na);
  nsp_ctrlpack_dpodiv(&p[1], &b[1], np, nb);
  i__1 = *nb - 1;
  idegre(&p[1], &i__1, np);
  /*    b=-a 
   */
  i__1 = *nb + 1;
  for (k = 1; k <= i__1; ++k) {
    r__ = b[k];
    b[k] = -a[k];
    a[k] = r__;
    /* L30: */
  }
  /* 
   *    si degre de b=0 v=... 
   * 
   */
  idegre(&b[1], na, nb);
  if (*nb == 0) {
    b1 = b[1];
    if ( Abs(b1) <= *tol) {
      *ierr = 1;
      *v = 0.;
      return 0;
    }
    *v = p[nbb] / a[nbb + 1] / b1;
    return 0;
  }
  /*    sinon goto 20 
   */
  goto L20;
} /* residu_ */


/*!but 
 *   calcul du degre d un polynome 
 *!liste d'appel 
 *   a=coeff par ordre croissant 
 *   majo=majorant 
 *   nvrai=degre calcule 
 *! 
 *    Copyright INRIA 
 * 
 */

static int c__1 = 1;

int idegre(double *a, int *majo, int *nvrai)
{
  double d__1, test, an;
  int k, kk, i__1= *majo + 1;
  --a;
  
  an = C2F(dasum)(&i__1, &a[1], &c__1);
  if (an == 0.) {
    goto L20;
  }
  if (*majo == 0) {
    goto L20;
  }
  for (k = 1; k <= i__1; ++k) {
    kk = *majo + 2 - k;
    test = (d__1 = a[kk], abs(d__1)) / an;
    if (test + 1. != 1.) {
      *nvrai = kk - 1;
      return 0;
    }
    /* L10: */
  }
 L20:
  *nvrai = 0;
  return 0;
}

