/* rpem.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/*/MEMBR ADD NAME=RPEM,SSI=0 
 */
/* Subroutine */ int
signal_rpem (double *theta, double *p, int *n, double *u, double *y,
	     double *lambda, double *k, double *c__, int *istab2, double *v,
	     double *eps, double *eps1, int *idim, double *fi, double *psi,
	     double *tstab, double *work, double *f, double *g, double *l)
{
  /* System generated locals */
  int p_dim1, p_offset, i__1, i__2;
  double d__1;

  /* Local variables */
  double alfa, beta;
  int init, i__, j;
  double gamma, s, e1;
  int j1;
  double u1, y1;
  int istab1;
  double dd, ci, al;
  int kk, ni, nn;
  double po;
  double amy;
  int ist;

  /*!but 
   *       -1           -1           -1 
   *    a(q  )y(t) = b(q  )u(t) + c(q  )e(t) 
   * 
   *!parametres 
   * 
   *    ***description des parametres*** 
   * 
   *    theta - vecteur d'ordre (3*n) qui contienne les parametres 
   *            de estimation 
   *            theta=(a(1) ...a(n),b(1) ...b(n), c(1) ...c(n) 
   *            theta est changee dans la subroutine 
   *    p     - matrice symetrique d'ordre (3*n) 
   *            p est employee dans la forme u-d 
   *            p=u*d*u(transposee) 
   *            avec d diagonale et u triangulaire superieure 
   *            les elements de d sont conserves dans la diagonale de d 
   *            les elements de u sont gardes dans la partie diagonale 
   *            superieure de p. p est changee dans la subroutine. 
   *    n     - modele d'ordre (min 1, max 10) 
   *    u     - derniere valeur d'entree 
   *    y     - derniere valeur de sortie 
   *    lambda- facteur d'oubli (a fournir) 
   *    k     - facteur de contraction employe pour le filtrage 
   *            des donnees (a fournir) 
   *            commentaire: 
   *            pour des resultats raisonnables 
   *            0.lt.lambda.le.1 
   *            lambda proche de 1 apres plusieurs appels a rpem 
   *            0.lt.k.lt.1 
   *            k proche de 1 apres plusieurs appels a rpem 
   *    c     - parametre employe pour la regularisation 
   *            c doit etre choisi plutot grand 
   *    istab1- flag (a fournir) pour les tests d'estabilite de c(z). 
   *            si istab1=0 on n'execute pas des controles (test 
   *            d'estabilite et reductions de pas) 
   *            si istab1.ne.0 on execute des controles (test 
   *            d'estabilite et eventuelles reductions de pas) 
   *    istab2- entier donne en sortie qu'indique le nombre des 
   *            reductions de pas executees. si istab1=0, 
   *            alors la valeur de istab2 n'est pas significative 
   *    v     - fonction de perte- addition des carres des erreurs 
   *            predis. une modification due a des incertitudes dans 
   *            la phase transiente est inclue 
   *            v est changee par la subroutine 
   *    eps   - erreur de prediction (retourne) 
   *    eps1  - residu(retourne) 
   *    init  - flag employe pour commencer la prediction 
   *            si init=0 tous les parametres sont mis a jour 
   *            si init.ne.0 des valeurs initiaux adequats sont 
   *            mis en premier et apres les parametres sont mis 
   *            en employant les donnees disponibles u et y 
   *    po    - parametre scalaire employe pour donner a p une 
   *            valeur initiale (a etre fourni quand init.ne.0) 
   *            if init.ne.0 p=po*matrice unitaire 
   *    idim  - parametre de dimension 
   * 
   *!subroutines necessaires 
   *    nstabl 
   *! 
   * 
   */
  /* Parameter adjustments */
  --theta;
  p_dim1 = *idim;
  p_offset = p_dim1 + 1;
  p -= p_offset;
  --fi;
  --psi;
  --tstab;
  --work;
  --f;
  --g;
  --l;

  /* Function Body */
  nn = *n * 3;
  /*    ======================================== 
   *    test pour initialisation 
   *    ======================================== 
   */
  init = 0;
  istab1 = 1;
  po = 1.;
  if (init == 0)
    {
      goto L100;
    }
  /* 
   */
  *v = 0.;
  i__1 = nn;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = nn;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L10: */
	  p[i__ + j * p_dim1] = 0.;
	}
    }
  i__2 = nn;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      p[i__ + i__ * p_dim1] = po;
      theta[i__] = 0.;
      l[i__] = 0.;
      fi[i__] = 0.;
      /* L20: */
      psi[i__] = 0.;
    }
  /*    ======================================== 
   *    calcul de l'erreur de prediction 
   *    ======================================== 
   */
 L100:
  *eps = *y;
  i__2 = nn;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      /* L110: */
      *eps -= fi[i__] * theta[i__];
    }
  /*    ======================================== 
   *    calcul des nouvelles estimations de parametres 
   *    ======================================== 
   */
  amy = 1.;
  /*    ======================================== 
   *    test pour determiner si on fait des controles 
   *    ======================================== 
   */
  if (istab1 == 0)
    {
      goto L200;
    }
  *istab2 = 0;
 L120:
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      ni = (*n << 1) + i__;
      /* L130: */
      tstab[i__ + 1] = theta[ni] + l[ni] * *eps * amy;
    }
  tstab[1] = 1.;
  /*    ======================================== 
   *    test pour stabilite de c(z) 
   *    ======================================== 
   */
  signal_nstabl (&tstab[1], n, &work[1], &ist);
  if (ist == 0)
    {
      goto L200;
    }
  amy /= 2.;
  if (amy + 1. <= 1.)
    {
      goto L200;
    }
  ++(*istab2);
  goto L120;
  /*    ======================================== 
   *    mise a jour de parametres d'estimation 
   *    ======================================== 
   */
 L200:
  i__2 = nn;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      /* L210: */
      theta[i__] += l[i__] * *eps * amy;
    }
  /*    ======================================== 
   *    calcul residus 
   *    ======================================== 
   */
  *eps1 = *y;
  i__2 = nn;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      /* L220: */
      *eps1 -= fi[i__] * theta[i__];
    }
  /*    ======================================== 
   *    calcul des signaux filtrees y1, u1, e1 
   *    ======================================== 
   */
  y1 = *y;
  u1 = *u;
  e1 = *eps1;
  /* 
   */
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      ci = theta[(*n << 1) + i__] * nsp_pow_di (*k, i__);
      y1 += ci * psi[i__];
      u1 -= ci * psi[*n + i__];
      /* L620: */
      e1 -= ci * psi[(*n << 1) + i__];
    }
  /*    ======================================== 
   *    mise a jour des vecteurs fi et psi 
   *    ======================================== 
   */
  if (*n == 1)
    {
      goto L720;
    }
  i__2 = *n;
  for (j = 2; j <= i__2; ++j)
    {
      i__ = *n + 2 - j;
      fi[i__] = fi[i__ - 1];
      psi[i__] = psi[i__ - 1];
      i__ = (*n << 1) + 2 - j;
      fi[i__] = fi[i__ - 1];
      psi[i__] = psi[i__ - 1];
      i__ = *n * 3 + 2 - j;
      fi[i__] = fi[i__ - 1];
      /* L700: */
      psi[i__] = psi[i__ - 1];
    }
 L720:
  fi[1] = -(*y);
  psi[1] = -y1;
  fi[*n + 1] = *u;
  psi[*n + 1] = u1;
  fi[(*n << 1) + 1] = *eps1;
  psi[(*n << 1) + 1] = e1;
  /*    ======================================== 
   *    calcul du vecteur de gain l, mise a jour de p et v 
   *    ======================================== 
   */
  i__2 = nn;
  for (i__ = 2; i__ <= i__2; ++i__)
    {
      j = nn + 2 - i__;
      alfa = psi[j];
      j1 = j - 1;
      i__1 = j1;
      for (kk = 1; kk <= i__1; ++kk)
	{
	  /* L800: */
	  alfa += p[kk + j * p_dim1] * psi[kk];
	}
      f[j] = alfa;
      /* L810: */
      g[j] = p[j + j * p_dim1] * alfa;
    }
  g[1] = p[p_dim1 + 1] * psi[1];
  f[1] = psi[1];
  /* 
   */
  alfa = *lambda + f[1] * g[1];
  gamma = 0.;
  if (alfa > 0.)
    {
      gamma = 1. / alfa;
    }
  if (g[1] != 0.)
    {
      p[p_dim1 + 1] = gamma * p[p_dim1 + 1];
    }
  /* 
   */
  i__2 = nn;
  for (j = 2; j <= i__2; ++j)
    {
      beta = alfa;
      dd = g[j];
      alfa += dd * f[j];
      if (alfa == 0.)
	{
	  goto L830;
	}
      al = -f[j] * gamma;
      /* 
       */
      j1 = j - 1;
      i__1 = j1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  s = p[i__ + j * p_dim1];
	  p[i__ + j * p_dim1] = s + al * g[i__];
	  /* L820: */
	  g[i__] += dd * s;
	}
      gamma = 1. / alfa;
      p[j + j * p_dim1] = beta * gamma * p[j + j * p_dim1] / *lambda;
      /*Computing MIN 
       */
      d__1 = p[j + j * p_dim1];
      p[j + j * p_dim1] = Min (d__1, *c__);
    L830:
      ;
    }
  /* 
   *Computing 2nd power 
   */
  d__1 = *eps;
  *v += d__1 * d__1 / alfa;
  i__2 = nn;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      /* L840: */
      l[i__] = g[i__] / alfa;
    }
  /*    ======================================== 
   *    fin des calculs 
   *    ======================================== 
   */
  return 0;
}				/* rpem_ */
