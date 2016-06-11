/* isoval.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int
nsp_calpack_isoval (double *a, int *lda, int *m, int *n, double *c__,
		    double *path, int *npath, int *maxp, int *ierr, int *iw,
		    int *job)
{
  /* Initialized data */

  static int north = 0;
  static int south = 1;
  static int east = 2;
  static int west = 3;

  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  int pend;
  int i__, j, k, l, kpath, kpath0;
  int ia, ic, ih, kh, ij, kj, ip, ir, iv, kv, ia1, dir;

  /*!    but 
   *    Etant donnee une matrice A telle que A(l,k)=f(X(l),Y(k)) ou 
   *    f est une fonction de R X R dans R, ce sous programme recherche 
   *    les lignes d'isovaleurs (relatives a la valeur c) de la 
   *    tabulation reguliere ( X(l+1)-X(l)=DX,Y(k+1)-Y(k)=DY ) 
   *    de f donnee par A 
   *!    liste d'appel 
   *    subroutine isoval(a,lda,m,n,c,path,npath,maxp,ierr,iw,job) 
   * 
   *    double precision a(lda,n),c,path(2,maxp) 
   *    int iw(m*n-n) 
   *    int lda,m,n,maxp,ierr,job 
   * 
   *    a     : tableau contenant la tabulation de f 
   *    lda   : nombre de lignes du tableau a 
   *    m     : nombre de lignes effectif de a 
   *    n     : nombre de colonnes de a 
   *    c     : valeur pour la quelle on cherche les isovaleurs 
   *    path  : contient en sortie la description des isovaleurs: 
   *    path=[path ,...., path     ] 
   *    1           npath 
   *    ou pathi a la structure suivante : 
   *    [np x1,...xnp] 
   *    [0  y1,...ynp] 
   *    si : 
   *    np est la longueur de l'isovaleur 
   *    xj,yj les coordonnees interpolees des points de l'isovaleur 
   *    npath : le nombre de courbes disjointes d'isovaleurs 
   *    maxp  : la dimension maximale admise pour le tableau path 
   *    ierr  : indicateur d'erreur 
   *    0 : ok 
   *    1 : nombre de points decrivant les isovaleur > maxp 
   *    iw    : tableau de travail 
   *    job   : flag permettant d'indiquer au programme si la fonction f 
   *    est definie sur l'ensemble des points de la matrice A 
   *    job=0 : f definie partout 
   *    job=1 : f n'est pas definie au points A(i,j) si 
   *            iw(i+(j-1)m)=0 
   *!    origine 
   *    programme par S Steer a partir de la macro scilab  de Carey Bunks 
   *    1990 
   * 
   *    Corrige par C Bunks pour isovaleurs qui sont exactement egales 
   *    a des entrees de la matrice 6 mars 1991. 
   *! 
   *    Copyright INRIA 
   * 
   */
  /* Parameter adjustments */
  --iw;
  path -= 3;
  --a;

  /* Function Body */
  /* 
   */
  *ierr = 0;
  /* 
   */
  kv = 1;
  kh = kv + *m * *n;
  /* kw = kh + *m * (*n - 1); */
  /* 
   *    perturb values which are exactly equal to the level value 
   */
  i__1 = *n * *m;
  for (ip = 1; ip <= i__1; ++ip)
    {
      if (a[ip] == *c__)
	{
	  a[ip] += 1e-14;
	}
      /* L5: */
    }
  /* 
   *    make horizontal and vertical edge matrices for level value 
   */
  if (*job == 0)
    {
      ih = kh - 1;
      ia = -(*lda);
      i__1 = *n - 1;
      for (k = 1; k <= i__1; ++k)
	{
	  ia += *lda;
	  i__2 = *m;
	  for (l = 1; l <= i__2; ++l)
	    {
	      ++ih;
	      iw[ih] = 1;
	      if ((a[ia + *lda + l] - *c__) * (a[ia + l] - *c__) < 0.)
		{
		  iw[ih] = -1;
		}
	      /* L10: */
	    }
	  /* L11: */
	}
      /* 
       */
      iv = kv - 1;
      ia = -(*lda);
      i__1 = *n;
      for (k = 1; k <= i__1; ++k)
	{
	  ia += *lda;
	  i__2 = *m - 1;
	  for (l = 1; l <= i__2; ++l)
	    {
	      ++iv;
	      iw[iv] = 1;
	      if ((a[ia + l + 1] - *c__) * (a[ia + l] - *c__) < 0.)
		{
		  iw[iv] = -1;
		}
	      /* L12: */
	    }
	  /* L13: */
	}
    }
  else
    {
      /* 
       */
      kj = kv;
      /* 
       */
      ih = kh - 1;
      ia = -(*lda);
      ij = kj - 1;
      i__1 = *n - 1;
      for (k = 1; k <= i__1; ++k)
	{
	  ia += *lda;
	  i__2 = *m;
	  for (l = 1; l <= i__2; ++l)
	    {
	      ++ij;
	      ++ih;
	      iw[ih] = 1;
	      if (iw[ij] * iw[ij + *m] == 0)
		{
		  goto L20;
		}
	      if ((a[ia + *lda + l] - *c__) * (a[ia + l] - *c__) < 0.)
		{
		  iw[ih] = -1;
		}
	    L20:
	      ;
	    }
	  /* L21: */
	}
      /* 
       */
      iv = kv - 1;
      ia = -(*lda);
      ij = kj - 1;
      i__1 = *n;
      for (k = 1; k <= i__1; ++k)
	{
	  ia += *lda;
	  i__2 = *m - 1;
	  for (l = 1; l <= i__2; ++l)
	    {
	      ++iv;
	      ++ij;
	      iw[iv] = 1;
	      if (iw[ij] * iw[ij + 1] == 0)
		{
		  goto L22;
		}
	      if ((a[ia + l + 1] - *c__) * (a[ia + l] - *c__) < 0.)
		{
		  iw[iv] = -1;
		}
	    L22:
	      ;
	    }
	  ++ij;
	  /* L23: */
	}
    }
  /* 
   */
  *npath = 0;
  kpath0 = 1;
  kpath = 0;
  /* 
   *    search pathes (starting with boundaries) 
   * 
   *    horizontal boundaries 
   *    northern border 
   */
  ih = kh - *m;
  ia = 1 - *lda;
  i__1 = *n - 1;
  for (ic = 1; ic <= i__1; ++ic)
    {
      ih += *m;
      ia += *lda;
      if (iw[ih] < 0)
	{
	  kpath = 1;
	  path[((kpath0 + 1) << 1) + 2] = 1.;
	  path[((kpath0 + 1) << 1) + 1] =
	    ic + (*c__ - a[ia]) / (a[ia + *lda] - a[ia]);
	  i__ = 1;
	  j = ic;
	  dir = north;
	  pend = FALSE;
	L30:
	  nsp_calpack_isova0 (&a[1], lda, m, n, &path[((kpath0 + 1) << 1) + 1],
			      &kpath, &i__, &j, &dir, &pend, &iw[kh],
			      &iw[kv], c__);
	  if (kpath0 + kpath >= *maxp)
	    {
	      goto L999;
	    }
	  if (!pend)
	    {
	      goto L30;
	    }
	  if (kpath > 1)
	    {
	      path[(kpath0 << 1) + 1] = (double) kpath;
	      path[(kpath0 << 1) + 2] = 0.;
	      kpath0 = kpath0 + 1 + kpath;
	      ++(*npath);
	    }
	  kpath = 0;
	}
      /* L31: */
    }
  /* 
   *    southern border 
   * 
   */
  ih = kh + (*m - 1) + (*n - 1) * *m;
  ia = *m + (*n - 1) * *lda;
  for (ic = *n - 1; ic >= 1; --ic)
    {
      ih -= *m;
      ia -= *lda;
      if (iw[ih] < 0)
	{
	  kpath = 1;
	  path[((kpath0 + 1) << 1) + 2] = (double) (*m);
	  path[((kpath0 + 1) << 1) + 1] =
	    ic + (*c__ - a[ia]) / (a[ia + *lda] - a[ia]);
	  i__ = *m - 1;
	  j = ic;
	  dir = south;
	  pend = FALSE;
	L40:
	  nsp_calpack_isova0 (&a[1], lda, m, n, &path[((kpath0 + 1) << 1) + 1],
			      &kpath, &i__, &j, &dir, &pend, &iw[kh],
			      &iw[kv], c__);
	  if (kpath0 + kpath >= *maxp)
	    {
	      goto L999;
	    }
	  if (!pend)
	    {
	      goto L40;
	    }
	  if (kpath > 1)
	    {
	      path[(kpath0 << 1) + 1] = (double) kpath;
	      path[(kpath0 << 1) + 2] = 0.;
	      kpath0 = kpath0 + kpath + 1;
	      ++(*npath);
	    }
	  kpath = 0;
	}
      /* L41: */
    }
  /* 
   *    vertical boundaries 
   * 
   *    eastern border 
   */
  iv = kv - 1 + (*n - 1) * (*m - 1);
  ia = (*n - 1) * *lda;
  i__1 = *m - 1;
  for (ir = 1; ir <= i__1; ++ir)
    {
      ++iv;
      ++ia;
      if (iw[iv] < 0)
	{
	  kpath = 1;
	  path[((kpath0 + 1) << 1) + 2] =
	    ir + (*c__ - a[ia]) / (a[ia + 1] - a[ia]);
	  path[((kpath0 + 1) << 1) + 1] = (double) (*n);
	  i__ = ir;
	  j = *n - 1;
	  dir = east;
	  pend = FALSE;
	L50:
	  nsp_calpack_isova0 (&a[1], lda, m, n, &path[((kpath0 + 1) << 1) + 1],
			      &kpath, &i__, &j, &dir, &pend, &iw[kh],
			      &iw[kv], c__);
	  if (kpath0 + kpath >= *maxp)
	    {
	      goto L999;
	    }
	  if (!pend)
	    {
	      goto L50;
	    }
	  if (kpath > 1)
	    {
	      path[(kpath0 << 1) + 1] = (double) kpath;
	      path[(kpath0 << 1) + 2] = 0.;
	      kpath0 = kpath0 + 1 + kpath;
	      ++(*npath);
	    }
	  kpath = 0;
	}
      /* L51: */
    }
  /* 
   *    western border 
   * 
   */
  iv = kv + *m - 1;
  ia = *m;
  for (ir = *m - 1; ir >= 1; --ir)
    {
      --iv;
      --ia;
      if (iw[iv] < 0)
	{
	  kpath = 1;
	  path[((kpath0 + 1) << 1) + 2] =
	    ir + (*c__ - a[ia]) / (a[ia + 1] - a[ia]);
	  path[((kpath0 + 1) << 1) + 1] = 1.;
	  i__ = ir;
	  j = 1;
	  dir = west;
	  pend = FALSE;
	L60:
	  nsp_calpack_isova0 (&a[1], lda, m, n, &path[((kpath0 + 1) << 1) + 1],
			      &kpath, &i__, &j, &dir, &pend, &iw[kh],
			      &iw[kv], c__);
	  if (kpath0 + kpath >= *maxp)
	    {
	      goto L999;
	    }
	  if (!pend)
	    {
	      goto L60;
	    }
	  if (kpath > 1)
	    {
	      path[(kpath0 << 1) + 1] = (double) kpath;
	      path[(kpath0 << 1) + 2] = 0.;
	      kpath0 = kpath0 + kpath + 1;
	      ++(*npath);
	    }
	  kpath = 0;
	}
      /* L61: */
    }
  /* 
   *    all the rest 
   * 
   */
  ih = kh - 1;
  ia1 = 1 - *lda;
  i__1 = *n - 1;
  for (ic = 1; ic <= i__1; ++ic)
    {
      ia1 += *lda;
      ia = ia1;
      ++ih;
      i__2 = *m - 1;
      for (ir = 2; ir <= i__2; ++ir)
	{
	  ++ih;
	  ++ia;
	  if (iw[ih] < 0)
	    {
	      kpath = 1;
	      path[((kpath0 + 1) << 1) + 2] = (double) ir;
	      path[((kpath0 + 1) << 1) + 1] =
		ic + (*c__ - a[ia]) / (a[ia + *lda] - a[ia]);
	      i__ = ir;
	      j = ic;
	      dir = north;
	      pend = FALSE;
	    L70:
	      nsp_calpack_isova0 (&a[1], lda, m, n,
				  &path[((kpath0 + 1) << 1) + 1], &kpath, &i__,
				  &j, &dir, &pend, &iw[kh], &iw[kv], c__);
	      if (kpath0 + kpath >= *maxp)
		{
		  goto L999;
		}
	      if (!pend)
		{
		  goto L70;
		}
	      if (kpath > 1)
		{
		  path[(kpath0 << 1) + 1] = (double) kpath;
		  path[(kpath0 << 1) + 2] = 0.;
		  kpath0 = kpath0 + kpath + 1;
		  ++(*npath);
		}
	      kpath = 0;
	    }
	  /* L71: */
	}
      ++ih;
      /* L72: */
    }
  return 0;
L999:
  *ierr = 1;
  return 0;
}				/* isoval_ */
