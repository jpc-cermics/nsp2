/* kronc.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*/MEMBR ADD NAME=KRONC,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_calpack_kronc (double *ar, double *ai, int *ia, int *ma, int *na,
		   double *br, double *bi, int *ib, int *mb, int *nb,
		   double *pkr, double *pki, int *ik)
{
  /* System generated locals */
  int i__1, i__2, i__3, i__4;

  /* Local variables */
  int i__, l, l1, ka, kb, kk, ja, jb, ka1, kk1;

  /*!but 
   *ce sous programme genere le produit de kronecker de deux matrices 
   * a et b complexes  pk(i,j)=a(i,j)*b 
   *!liste d'appel 
   *     subroutine kronc(ar,ai,ia,ma,na,br,bi,ib,mb,nb,pkr,pki,ik) 
   *    double precision ar(*),ai(*),br(*),bi(*),pkr(*),pki(*) 
   *    int ia,ma,na,ib,mb,nb,ik 
   * 
   *    ar,ai : tableaux contenant les parties reelles et imaginaires 
   *         de la matrice a 
   *    ia : increment entre 2 elements consecutif d'une meme 
   *            ligne de a 
   *    ma : nombre de lignes de a 
   *    na : nombre de colonnes dea 
   *    br,bi,ib,mb,nb : definitions similaires pour la matrice b 
   *    pkr,pki : tableaux contenant les parties reelles et imaginaires 
   *              du resultat 
   *    ik : increment entre deux elements consecutifs d'une meme 
   *         ligne de pk 
   *! 
   * 
   */
  /* Parameter adjustments */
  --pki;
  --pkr;
  --bi;
  --br;
  --ai;
  --ar;

  /* Function Body */
  ka1 = 1 - *ia;
  kk1 = -(*nb);
  i__1 = *na;
  for (ja = 1; ja <= i__1; ++ja)
    {
      kb = 1;
      ka1 += *ia;
      kk1 += *nb;
      i__2 = *nb;
      for (jb = 1; jb <= i__2; ++jb)
	{
	  ka = ka1;
	  kk = (jb - 1 + kk1) * *ik + 1;
	  i__3 = *ma;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      i__4 = *mb;
	      for (l = 1; l <= i__4; ++l)
		{
		  l1 = l - 1;
		  pkr[kk + l1] = ar[ka] * br[kb + l1] - ai[ka] * bi[kb + l1];
		  pki[kk + l1] = ar[ka] * bi[kb + l1] + ai[ka] * br[kb + l1];
		  /* L5: */
		}
	      kk += *mb;
	      ++ka;
	      /* L10: */
	    }
	  kb += *ib;
	  /* L20: */
	}
      /* L30: */
    }
  return 0;
}				/* kronc_ */
