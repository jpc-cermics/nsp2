/* imcopy.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*/MEMBR ADD NAME=IMCOPY,SSI=0 
 *    Copyright INRIA 
 */
int nsp_calpack_imcopy (int *a, int *na, int *b, int *nb, int *m, int *n)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  int i__, j, ia, ib, mn;

  /*!but 
   *    ce sous programme effectue:b=a 
   *    avec a matrice m lignes et n colonnes 
   *    imcopy utilise un code particulier si les matrices sont 
   *    compactes 
   *!liste d'appel 
   * 
   *    subroutine imcopy(a,na,b,nb,m,n) 
   *    int a(na,n),b(nb,m) 
   *    int na,nb,m,n 
   * 
   *    a         tableau contenant la matrice a 
   *    na        nombre de lignes du tableau a dans le prog appelant 
   *    b,nb      definition similaires a :a,na 
   *    m         nombre de lignes des matrices a et b 
   *    n         nombre de colonnes des matrices a et b 
   *!sous programmes utilises 
   *    neant 
   *! 
   * 
   */
  /* Parameter adjustments */
  --b;
  --a;

  /* Function Body */
  if (*na == *m && *nb == *m)
    {
      goto L20;
    }
  ia = -(*na);
  ib = -(*nb);
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      ia += *na;
      ib += *nb;
      i__2 = *m;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  b[ib + i__] = a[ia + i__];
	  /* L10: */
	}
    }
  return 0;
L20:
  /*code pour des matrices compactes 
   */
  mn = *m * *n;
  i__2 = mn;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      b[i__] = a[i__];
      /* L30: */
    }
  return 0;
}				/* imcopy_ */
