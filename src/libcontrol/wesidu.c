/*    calcul de la somme des residus de p/(a.b) 
 *    aux zeros de a 
 *    p=pr+i*pi=polynome de degre np a coefficients complexes 
 *    a=ar+i*ai                   na 
 *    b=br+i*bi                   nb 
 *    les zeros de b sont supposes tous differents des 
 *    zeros de a.... 
 *    a,b et p dimensionnes au moins a leur degre+1 dans le pgm 
 *    appelant. 
 *    rangement par degres croissants. 
 *    v=vr+i*vi=resultat 
 *    principe du calcul:si a (ou b) est une constante on a 
 *    v=p(nb)/b(nb+1)/a(1) 
 *    sinon on remplace p et a par le reste de la division 
 *    euclidienne de p et a par b,puis on inverse les roles 
 *    de a et b en changeant le signe de v. 
 *    on itere jusqu a trouver degre de a ou degre de b=0. 
 *    F.D. 
 * 
 *    Copyright INRIA 
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

static int nsp_ctrlpack_wpodiv(double *ar, double *ai, double *br, double *bi, int *na, int *nb, int *ierr);

int nsp_ctrlpack_wesidu (double *pr, double *pi, int *np, double *ar, double *ai,
			 int *na, double *br, double *bi, int *nb, double *vr,
			 double *vi, double *tol, int *ierr)
{
  int i__1, i__2, k, nbb, nit, npp; 
  double d__1, b1, ri, rr;
 
  --bi;
  --br;
  --ai;
  --ar;
  --pi;
  --pr;
  /* Function Body */
  *vr = 0.;
  *vi = 0.;
  npp = *np;
  nsp_ctrlpack_wdegre (&ar[1], &ai[1], na, na);
  nsp_ctrlpack_wdegre (&br[1], &bi[1], nb, nb);
  if (*na == 0)
    {
      return 0;
    }
  if (*nb == 0)
    {
      b1 = (d__1 = br[1] + bi[1], Abs (d__1));
      if (b1 == 0.)
	{
	  *ierr = 0;
	  return 0;
	}
      if (npp >= *na - 1)
	{
	  nsp_calpack_wdiv (&pr[*na], &pi[*na], &ar[*na + 1], &ai[*na + 1],
			    vr, vi);
	  nsp_calpack_wdiv (vr, vi, &br[1], &bi[1], vr, vi);
	  return 0;
	}
      else
	{
	  *vr = 0.;
	  *vi = 0.;
	  return 0;
	}
    }
  if (*na > *np)
    {
      goto L11;
    }
  /*    p=p/a  (reste de la division euclidienne...) 
   */
  nsp_ctrlpack_wpodiv (&pr[1], &pi[1], &ar[1], &ai[1], np, na, ierr);
  if (*ierr != 0)
    {
      return 0;
    }
  i__1 = *na - 1;
  nsp_ctrlpack_wdegre (&pr[1], &pi[1], &i__1, np);
 L11:
  if (*na > *nb)
    {
      goto L31;
    }
  /*    b=b/a  (reste de la div euclidienne...) 
   */
  nsp_ctrlpack_wpodiv (&br[1], &bi[1], &ar[1], &ai[1], nb, na, ierr);
  if (*ierr != 0)
    {
      return 0;
    }
  i__1 = *na - 1;
  nsp_ctrlpack_wdegre (&br[1], &bi[1], &i__1, nb);
 L31:
  if (*na == 1)
    {
      /*    v=p(na)/a(na+1)/b(1) 
       */
      b1 = Abs (br[1]) + Abs (bi[1]);
      if (b1 <= *tol)
	{
	  *ierr = 1;
	  return 0;
	}
      nsp_calpack_wdiv (&pr[*na], &pi[*na], &ar[*na + 1], &ai[*na + 1], vr,
			vi);
      nsp_calpack_wdiv (vr, vi, &br[1], &bi[1], vr, vi);
      return 0;
    }
  /*Computing MIN 
   */
  i__2 = *na - 1;
  i__1 = Min (i__2, *nb);
  nsp_ctrlpack_wdegre (&br[1], &bi[1], &i__1, nb);
  if (*nb > 0)
    {
      goto L32;
    }
  b1 = Abs (br[1]) + Abs (bi[1]);
  if (b1 <= *tol)
    {
      *ierr = 1;
      return 0;
    }
  if (npp >= *na - 1)
    {
      /*    v=p(na)/a(na+1)/b(1) 
       */
      nsp_calpack_wdiv (&pr[*na], &pi[*na], &ar[*na + 1], &ai[*na + 1], vr,
			vi);
      nsp_calpack_wdiv (vr, vi, &br[1], &bi[1], vr, vi);
      return 0;
    }
  else
    {
      *vr = 0.;
      *vi = 0.;
    }
 L32:
  nit = 0;
 L20:
  if (nit >= 1)
    {
      *na = nbb;
    }
  ++nit;
  nbb = *nb;
  nsp_ctrlpack_wpodiv (&ar[1], &ai[1], &br[1], &bi[1], na, nb, ierr);
  if (*ierr != 0)
    {
      return 0;
    }
  i__1 = *nb - 1;
  nsp_ctrlpack_wdegre (&ar[1], &ai[1], &i__1, na);
  nsp_ctrlpack_wpodiv (&pr[1], &pi[1], &br[1], &bi[1], np, nb, ierr);
  if (*ierr != 0)
    {
      return 0;
    }
  i__1 = *nb - 1;
  nsp_ctrlpack_wdegre (&pr[1], &pi[1], &i__1, np);
  i__1 = *nb + 1;
  for (k = 1; k <= i__1; ++k)
    {
      rr = br[k];
      ri = bi[k];
      br[k] = -ar[k];
      bi[k] = -ai[k];
      ar[k] = rr;
      ai[k] = ri;
      /* L30: */
    }
  nsp_ctrlpack_wdegre (&br[1], &bi[1], na, nb);
  if (*nb == 0)
    {
      goto L99;
    }
  goto L20;
 L99:
  /*    v=p(nbb)/a(nbb+1)/b(1) 
   */
  b1 = Abs (br[1]) + Abs (bi[1]);
  if (b1 <= *tol)
    {
      *ierr = 1;
      return 0;
    }
  nsp_calpack_wdiv (&pr[nbb], &pi[nbb], &ar[nbb + 1], &ai[nbb + 1], vr, vi);
  nsp_calpack_wdiv (vr, vi, &br[1], &bi[1], vr, vi);
  return 0;
}				/* wesidu_ */

/*
 *    division euclidienne de deux polynomes a coefficients complexes 
 *    le suffixe r designe la partie reelle et i la partie imaginaire 
 * 
 *    1. a: avant execution c'est le tableau des coefficients du 
 *   polynome dividende range suivant les puissances croissantes 
 *   de la variable (ordre na+1). 
 *   apres execution,contient dans les nb premiers elements 
 *   le tableau des coefficients du reste ordonne suivant les 
 *   puissances croissantes, et dans les (na-nb+1) derniers elements, 
 *   le tableau des coefficients du polynome quotient range suivant 
 *   les puissances croissantes de la variable. 
 *    2. b: tableau des coefficients du polynome diviseur range suivant 
 *   les puissances croissantes de la variable (ordre nb+1). 
 *    3. na: degre du polynome a. 
 *    4. nb: degre du polynome b. 
 * 
 */

int nsp_ctrlpack_wpodiv(double *ar, double *ai, double *br, double *bi, int *na, int *nb, int *ierr)
{
  int i__1, nb1, i__, l, n, n1, n2;
  double qi, wi, qr, wr;
  /* Parameter adjustments */
  --bi;
  --br;
  --ai;
  --ar;

  /* Function Body */
  *ierr = 0;
  l = *na - *nb + 1;
 L2:
  if (l <= 0) {
    goto L5;
  } else {
    goto L3;
  }
 L3:
  n = l + *nb;
  /*     q=a(n)/b(nb+1) 
   */
  nsp_calpack_wwdiv(&ar[n], &ai[n], &br[*nb + 1], &bi[*nb + 1], &qr, &qi, ierr);
  if (*ierr != 0) {
    return 0;
  }
  nb1 = *nb + 1;
  i__1 = nb1;
  for (i__ = 1; i__ <= i__1; ++i__) {
    n1 = *nb - i__ + 2;
    n2 = n - i__ + 1;
    /*  4  a(n2)=a(n2)-b(n1)*q 
     */
    nsp_calpack_wmul(&br[n1], &bi[n1], &qr, &qi, &wr, &wi);
    ar[n2] -= wr;
    ai[n2] -= wi;
    /* L4: */
  }
  ar[n] = qr;
  ai[n] = qi;
  --l;
  goto L2;
 L5:
  return 0;
} /* wpodiv_ */


