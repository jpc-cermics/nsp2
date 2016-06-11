/* mtran.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*/MEMBR ADD NAME=MTRAN,SSI=0 
 *    Copyright INRIA 
 */
int nsp_calpack_mtran (double *a, int *na, double *b, int *nb, int *m, int *n)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  int i__, j, ia, ib;

  /*!but 
   *    mtran transpose la matrice a dans le tableau b 
   *    a et b n'ayant pas la meme implantation memoire 
   * 
   *!liste d'appel 
   *    subroutine mtran(a,na,b,nb,m,n) 
   *    double precision a(na,n),b(nb,m) 
   *    int na,nb,m,n 
   * 
   *    a        tableau contenant la matrice a 
   *    na       nombre de ligne du tableau a dans le prog appelant 
   *    b,nb     definition similaire a celle de a,na 
   *    m        nombre de lignes de a et de colonnes de b 
   *    n        nombre de colonnes de a et de lignes de b 
   *!sous programmes utilises 
   *    neant 
   *! 
   * 
   */
  /* Parameter adjustments */
  --b;
  --a;

  /* Function Body */
  ia = 0;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      ib = j;
      i__2 = *m;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  b[ib] = a[ia + i__];
	  ib += *nb;
	  /* L10: */
	}
      ia += *na;
      /* L20: */
    }
  return 0;
}				/* mtran_ */
