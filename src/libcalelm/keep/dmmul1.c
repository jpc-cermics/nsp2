#include "calpack.h"

static int c__1 = 1;

int
nsp_calpack_dmmul1_void (double *a, int *na, double *b, int *nb, double *c__,
			 int *nc, int *l, int *m, int *n)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  int i__, j, ib, ic;

  /*!but 
   *    ce sous programme effectue le produit matriciel: 
   *    c=c+a*b . 
   *!liste d'appel 
   * 
   *    subroutine dmmul1(a,na,b,nb,c,nc,l,m,n) 
   *    double precision a(na,m),b(nb,n),c(nc,n) 
   *    int na,nb,nc,l,m,n 
   * 
   *    a            tableau de taille na*m contenant la matrice a 
   *    na           nombre de lignes du tableau a dans le programme appel 
   *    b,nb,c,nc    definitions similaires a celles de a,na 
   *    l            nombre de ligne des matrices a et c 
   *    m            nombre de colonnes de a et de lignes de b 
   *    n            nombre de colonnes de b et c 
   *!sous programmes utilises 
   *    neant 
   *! 
   *    Copyright INRIA 
   * 
   */
  /* Parameter adjustments */
  --c__;
  --b;
  --a;

  /* Function Body */
  ib = 1;
  ic = 0;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *l;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /* L20: */
	  c__[ic + i__] += C2F (ddot) (m, &a[i__], na, &b[ib], &c__1);
	}
      ic += *nc;
      ib += *nb;
      /* L30: */
    }
  return 0;
}				/* dmmul1_ */
