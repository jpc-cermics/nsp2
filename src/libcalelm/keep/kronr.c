/* kronr.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;

/*    Copyright INRIA 
 */
int
nsp_calpack_kronr (double *a, int *ia, int *ma, int *na, double *b, int *ib,
		   int *mb, int *nb, double *pk, int *ik)
{
  /* System generated locals */
  int i__1, i__2, i__3;

  /* Local variables */
  int i__;
  int ka, kb, ja, jb, kk, ka1, kk1;

  /*!but 
   *ce sous programme genere le produit de kronecker de deux matrices 
   * a et b  pk(i,j)=a(i,j)*b 
   *!liste d'appel 
   *     subroutine kronr(a,ia,ma,na,b,ib,mb,nb,pk,ik) 
   *    double precision a(ia,na),b(ib,nb),pk(ik,*) 
   *    int ia,ma,na,ib,mb,nb,ik 
   * 
   *    a : tableau contenant la matrice a 
   *    ia : increment entre 2 elements consecutif d'une meme 
   *            ligne de a 
   *    ma : nombre de lignes de a 
   *    na : nombre de colonnes dea 
   *    b,ib,mb,nb : definitions similaires pour la matrice b 
   *    pk : tableau contenant la matrice resultat pk 
   *    ik : increment entre deux elements consecutifs d'une meme 
   *         ligne de pk 
   *! 
   * 
   */
  /* Parameter adjustments */
  --pk;
  --b;
  --a;

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
	      C2F (dcopy) (mb, &b[kb], &c__1, &pk[kk], &c__1);
	      C2F (dscal) (mb, &a[ka], &pk[kk], &c__1);
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
}				/* kronr_ */
