#include "signal.h"

#define d_lg10 log10
#define i_dnnt(x) ((int) ((x)>=0 ? floor(x + .5) : -floor(.5 - x) ) )

static int c_n1 = -1;
static int c__1 = 1;

/*!but 
 *    on cherche une factorisation spectrale d'un polynome a donne 
 *    par : a*(a(1/z) = b(n)*z**-n+....+b(0)+ ... +b(n)*z**n 
 *!liste d'appel 
 *    subroutine sfact1(b,n,w,maxit,ierr) 
 * 
 *    double precision b(n+1),w(6*(n+1)) 
 *    int n,maxit,ierr 
 * 
 *    b : contient les coeffs b(n),b(n-1),....,b(0) 
 *        apres execution b contient les coeff du resultat 
 *    n : degre de a 
 *    w : tableau de travail de taille 6*(n+1) 
 *    maxit : nombre maxi d'iterations admis 
 *    ierr : indicateur d'erreur 
 *            si ierr=0 : ok 
 *            si ierr<0 : convergence a 10**ierr pres 
 *            si ierr=1 : non convergence 
 *            si ierr=2 : b(0) est negatif ou nul. 
 * 
 *!auteur 
 *    f Delebecque inria (86) d'apres Kucera V 
 *    modif s. Steer (90) 
 *!origine 
 *    V Kucera : Discrete Linear control (john Wiley& Sons) 1979 
 *! 
 *    Copyright INRIA 
 * 
 */

int signal_sfact1 (double *b,const int *n, double *w,const int *maxit, int *ierr)
{
  int i1, i2, i3;
  double d1;
  int leta;
  double best=0.0, temp;
  int i__, j, k;
  double s;
  int lbold, lomeg, lsave;
  double a0, b0, b00;
  int lb, lambda;
  int lalpha;
  double eps;
  int lro;

  --b;
  --w;

  eps = nsp_dlamch ("p") * 10.;
  /* 
   */
  lb = *n + 1;
  *ierr = 0;
  /* 
   */
  lomeg = 1;
  lalpha = lomeg + lb;
  lro = lalpha + lb;
  leta = lro + lb;
  lbold = leta + lb;
  lambda = lbold + lb;
  lsave = lambda + lb;
  /* 
   */
  C2F(dcopy) (&lb, &b[1], &c_n1, &w[lbold], &c__1);
  C2F(dcopy) (&lb, &w[lbold], &c__1, &b[1], &c__1);
  b00 = w[lbold];
  if (b00 <= 0.)
    {
      goto L91;
    }
  b0 = sqrt (b00);
  i1 = lb;
  for (j = 1; j <= i1; ++j)
    {
      w[lalpha - 1 + j] = b[j] / b0;
      /* L1: */
    }
  /* 
   */
  i1 = *maxit;
  for (i__ = 1; i__ <= i1; ++i__)
    {
      /* 
       */
      C2F(dcopy) (&lb, &w[lbold], &c__1, &b[1], &c__1);
      C2F(dcopy) (&lb, &w[lalpha], &c__1, &w[lomeg], &c__1);
      /*    premier tableau 
       */
      i2 = lb - 1;
      for (k = 1; k <= i2; ++k)
	{
	  i3 = lb - k + 1;
	  C2F(dcopy) (&i3, &w[lalpha], &c_n1, &w[lro], &c__1);
	  w[lambda + k - 1] = w[lalpha + lb - k] / w[lro + lb - k];
	  i3 = lb - k;
	  for (j = 1; j <= i3; ++j)
	    {
	      w[lalpha - 1 + j] -= w[lambda + k - 1] * w[lro + j - 1];
	      /* L11: */
	    }
	  a0 = w[lalpha];
	  /*    calcul de eta 
	   */
	  w[leta + lb - k] = b[lb - k + 1] * 2. / a0;
	  if (k < lb - 1)
	    {
	      i3 = lb - k;
	      for (j = 2; j <= i3; ++j)
		{
		  b[j] -= w[leta + lb - k] * .5 * w[lalpha + lb - k - j + 1];
		  /* L12: */
		}
	    }
	  /* L15: */
	}
      w[leta] = b[1] / w[lalpha];
      /*    deuxieme tableau 
       */
      for (k = lb - 1; k >= 1; --k)
	{
	  i2 = lb - k + 1;
	  C2F(dcopy) (&i2, &w[leta], &c_n1, &b[1], &c__1);
	  i2 = lb - k + 1;
	  for (j = 1; j <= i2; ++j)
	    {
	      w[leta + j - 1] -= w[lambda + k - 1] * b[j];
	      /* L19: */
	    }
	  /* L21: */
	}
      s = 0.;
      i2 = lb;
      for (j = 1; j <= i2; ++j)
	{
	  w[lalpha - 1 + j] = (w[leta + j - 1] + w[lomeg + j - 1]) * .5;
	  s += w[lalpha - 1 + j] * w[lalpha - 1 + j];
	  /* L22: */
	}
      /* 
       *    test de convergence 
       *    calcul de l'erreur element par elements 
       *$$$  call dcopy(lb,w(lbold),-1,b,1) 
       *$$$  do 900 iii=0,n 
       *$$$  x=b(iii+1)-ddot(iii+1,w(lalpha),1,w(lalpha+n-iii),1) 
       *$$$  write(6,'(10x,e10.3,'','',e10.3,'';'')') x,b(1+iii) 
       *$$$  900  continue 
       */
      temp = (d1 = s - b00, Abs (d1)) / b00;
      if (temp <= eps)
	{
	  goto L50;
	}
      if (i__ == 1)
	{
	  best = temp;
	}
      if (temp < best)
	{
	  C2F(dcopy) (&lb, &w[lalpha], &c__1, &w[lsave], &c__1);
	  best = temp;
	}
      /* L40: */
    }
  goto L90;
  /* 
   */
 L50:
  i1 = lb;
  for (i__ = 1; i__ <= i1; ++i__)
    {
      b[i__] = w[lalpha - 1 + i__];
      /* L51: */
    }
  return 0;
  /* 
   *    retours en erreur 
   * 
   */
 L90:
  if (best <= .001)
    {
      C2F(dcopy) (&lb, &w[lsave], &c__1, &b[1], &c__1);
      d1 = d_lg10 (best);
      *ierr = i_dnnt (d1);
    }
  else
    {
      /*    non convergence 
       */
      *ierr = 1;
    }
  return 0;
 L91:
  /*    b00 est negatif ou nul 
   */
  *ierr = 2;
  return 0;
}

/* 
 *!but 
 *     Etant donnee la matrice bloc :[B0, ...,  B(n-1), B(n)] 
 *    ou les Bi sont les coefficients (de degre i) du produit 
 *    de la matrice polynomiale A par A'(1/z) 
 * 
 *    alors cette subroutine produit les coefficients d'une 
 *    matrice polynomiale Hurwitz D, qui est le facteur 
 *    spectrale gauche associe a A, tel que A*A'(1/z)=D*D'(1/z) 
 *    (ou D=D0+D1*d+ ... +Dn*(d**n)). 
 * 
 *!methode 
 *    La methode de factorisation spectrale donnee ici est basee 
 *    dans la methode de factorisation de Cholesky. Elle est 
 *    iterative et assure les convergences monotone et geometrique. 
 *    En plus elle peut etre employee naturellement pour des 
 *    polynomes scalaires, bien que dans ce cas existent 
 *    des algorithmes plus surs et plus rapides. 
 * 
 *    Voir V. KUCERA.- Discrete linear Control (The polynomial 
 *    equation approach), J. Wiley & sons, 1979. Secs 2.10 et 7.13. 
 * 
 *!parametres d'appel 
 * 
 *    call sfact2(b,l,n,matg,maxit,ierr) 
 * 
 *    double precision b(l,(n+1)*l) ) , matg(n*n*l*l) 
 *    int n,maxit,ierr 
 * 
 *    b : contient les coefficients bi : bi=b(1:l,1+(i-1)*l) 
 *        apres execution b contient les di. 
 *    l : nombre de lignes et de colonnes des bi 
 *    n: degre du polynome matriciel A, qui donne origine a b 
 *    matg : tableau de travail de taille q*(q+1)/2 avec q=(n+1)*l 
 * 
 *    maxit:entier, indique le nombre maximum d'maxitations admis 
 * 
 *    ierr:entier, si 0 fin normale, 
 *                 si -1 fin pour quantite maximum d'iterations 
 *                 si 1 probleme singulier ou non symetrique 
 *                 atteinte 
 * 
 *!auteur 
 * 
 *    Cette subroutine est la version fortran de l'algorithme 
 *    donne dans la section 7.13 du livre de vladimir kucera: 
 *    "discrete linear control", faite par 
 *    carlos klimann, inria, 16-xii-85. 
 * 
 *! 
 * 
 * 
 * 
 * 
 *la matrice delta(i-1) est stockee dans matg sous forme compacte 
 *l'element (i,j) est stocke en iadr(i,j) 
 *l'element en haut a gauche est repere par id0,id0 
 * 
 */

int signal_sfact2 (double *b,const int *l, const int *n, double *matg,const int *maxit, int *ierr)
{
  int b_dim1, b_offset, i1, i2, i3, i__4;
  double d1;
  int iter, j, k, p, q, r__;
  double sigma;
  int  j1, j2, jj, q22, kk, id0;
  double tr1=0, tr2, acu;
  int nel;

  /* Parameter adjustments */
  b_dim1 = *l;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  --matg;

  /* Function Body */
  p = *n * *l;
  q = p + *l;
  q22 = (q << 1) + 2;
  /* 
   */
  nel = q * (q + 1) / 2;
  i1 = nel;
  for (j = 1; j <= i1; ++j)
    {
      /* L5: */
      matg[j] = 0.;
    }
  i1 = q;
  for (j = p + 1; j <= i1; ++j)
    {
      i2 = q;
      for (r__ = j; r__ <= i2; ++r__)
	{
	  /* L6: */
	  matg[r__ - j + 1 + (q22 - j) * (j - 1) / 2] =
	    b[r__ - p + (j - p) * b_dim1];
	}
    }
  /* 
   */
  id0 = p + 1;
  iter = 0;
  j = p;
  /* 
   *calcul de delta(0) - par choleski 
   */
  goto L20;
  /* 
   */
 L10:
  /* 
   *calcul de x=[bi,...,b1]*delta(i-1)'**(-1) 
   * 
   */
  i2 = p;
  for (j = id0; j <= i2; ++j)
    {
      j1 = (j - 1) / *l;
      j2 = j - j1 * *l;
      jj = (*n - j1) * *l + j2;
      if (matg[j - j + 1 + (q22 - j) * (j - 1) / 2] == 0.)
	{
	  goto L60;
	}
      i1 = q;
      for (r__ = p + 1; r__ <= i1; ++r__)
	{
	  sigma = 0.;
	  if (j == id0)
	    {
	      goto L12;
	    }
	  i3 = j - 1;
	  for (k = id0; k <= i3; ++k)
	    {
	      sigma +=
		matg[j - k + 1 + (q22 - k) * (k - 1) / 2] * matg[r__ - k + 1 +
								 (q22 -
								  k) * (k -
									1) /
								 2];
	      /* L11: */
	    }
	L12:
	  matg[r__ - j + 1 + (q22 - j) * (j - 1) / 2] =
	    (b[r__ - p + jj * b_dim1] - sigma) / matg[j - j + 1 +
						      (q22 - j) * (j -
								   1) / 2];
	  /* L13: */
	}
      /* L14: */
    }
  /* 
   *calcul de b0-x*x' 
   * 
   */
  i2 = q;
  for (j = p + 1; j <= i2; ++j)
    {
      i1 = q;
      for (r__ = j; r__ <= i1; ++r__)
	{
	  sigma = 0.;
	  i3 = p;
	  for (k = id0; k <= i3; ++k)
	    {
	      /* L16: */
	      sigma +=
		matg[r__ - k + 1 + (q22 - k) * (k - 1) / 2] * matg[j - k + 1 +
								   (q22 -
								    k) * (k -
									  1) /
								   2];
	    }
	  matg[r__ - j + 1 + (q22 - j) * (j - 1) / 2] =
	    b[r__ - p + (j - p) * b_dim1] - sigma;
	  /* L17: */
	}
      /* L18: */
    }
  /* 
   */
 L20:
  /* 
   *factorisation de cholesky du bloc en bas a droite 
   * 
   */
  i2 = q;
  for (j = p + 1; j <= i2; ++j)
    {
      sigma = matg[j - j + 1 + (q22 - j) * (j - 1) / 2];
      if (j == p + 1)
	{
	  goto L22;
	}
      i1 = j - 1;
      for (k = p + 1; k <= i1; ++k)
	{
	  sigma -=
	    matg[j - k + 1 + (q22 - k) * (k - 1) / 2] * matg[j - k + 1 +
							     (q22 - k) * (k -
									  1) /
							     2];
	  /* L21: */
	}
    L22:
      if (sigma <= 0.)
	{
	  goto L60;
	}
      matg[j - j + 1 + (q22 - j) * (j - 1) / 2] = sqrt (sigma);
      if (j == q)
	{
	  goto L26;
	}
      /* 
       */
      i1 = q;
      for (r__ = j + 1; r__ <= i1; ++r__)
	{
	  sigma = matg[r__ - j + 1 + (q22 - j) * (j - 1) / 2];
	  if (j == p + 1)
	    {
	      goto L24;
	    }
	  i3 = j - 1;
	  for (k = p + 1; k <= i3; ++k)
	    {
	      sigma -=
		matg[j - k + 1 + (q22 - k) * (k - 1) / 2] * matg[r__ - k + 1 +
								 (q22 -
								  k) * (k -
									1) /
								 2];
	      /* L23: */
	    }
	L24:
	  matg[r__ - j + 1 + (q22 - j) * (j - 1) / 2] =
	    sigma / matg[j - j + 1 + (q22 - j) * (j - 1) / 2];
	  /* L25: */
	}
    L26:
      ;
    }
  /* 
   */
  if (*n == 0)
    {
      goto L50;
    }
  /* 
   *calcul de la trace du bloc en bas a droite 
   * 
   */
  tr2 = 0.;
  i2 = q;
  for (jj = p + 1; jj <= i2; ++jj)
    {
      tr2 += matg[jj - jj + 1 + (q22 - jj) * (jj - 1) / 2];
      /* L30: */
    }
  /* 
   *test de convergence 
   * 
   */
  if (iter == 1)
    {
      goto L40;
    }
  acu = (d1 = tr1 - tr2, Abs (d1));
  if (acu + Abs (tr2) <= Abs (tr2))
    {
      goto L50;
    }
  if (iter >= *maxit)
    {
      goto L50;
    }
  /* 
   *shift 
   * 
   */
 L40:
  /*Computing MAX 
   */
  i2 = id0 - *l;
  id0 = Max (i2, 1);
  i2 = p;
  for (jj = id0; jj <= i2; ++jj)
    {
      i1 = jj;
      for (kk = id0; kk <= i1; ++kk)
	{
	  i3 = jj + *l;
	  i__4 = kk + *l;
	  matg[jj - kk + 1 + (q22 - kk) * (kk - 1) / 2] =
	    matg[i3 - i__4 + 1 + (q22 - i__4) * (i__4 - 1) / 2];
	  /* L41: */
	}
    }
  tr1 = tr2;
  /* 
   */
  ++iter;
  goto L10;
  /* 
   */
 L50:
  /* 
   *fin 
   * 
   */
  i1 = *l;
  for (r__ = 1; r__ <= i1; ++r__)
    {
      i2 = *l;
      for (j = r__; j <= i2; ++j)
	{
	  b[r__ + j * b_dim1] = 0.;
	  /* L51: */
	  i3 = p + j;
	  i__4 = p + r__;
	  b[j + r__ * b_dim1] =
	    matg[i3 - i__4 + 1 + (q22 - i__4) * (i__4 - 1) / 2];
	}
      if (*n == 0)
	{
	  goto L53;
	}
      i3 = q;
      for (j = *l + 1; j <= i3; ++j)
	{
	  j1 = (j - 1) / *l;
	  j2 = j - j1 * *l;
	  jj = (*n - j1) * *l + j2;
	  /* L52: */
	  i__4 = p + r__;
	  b[r__ + j * b_dim1] =
	    matg[i__4 - jj + 1 + (q22 - jj) * (jj - 1) / 2];
	}
    L53:
      ;
    }
  *ierr = 0;
  if (iter >= *maxit)
    {
      *ierr = -1;
    }
  return 0;
 L60:
  *ierr = 1;
  return 0;
}	
