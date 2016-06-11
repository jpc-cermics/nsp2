/* rtitr.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static double c_b4 = 0.;
static int c__0 = 0;
static int c__1 = 1;

/*/MEMBR ADD NAME=RTITR,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_ctrlpack_rtitr (int *nin, int *nout, int *nu, double *num, int *inum,
		    int *dgnum, double *den, int *iden, int *dgden,
		    double *up, double *u, int *iu, double *yp, double *y,
		    int *iy, int *job, int *iw, double *w, int *ierr)
{
  /* System generated locals */
  int num_dim1, num_offset, den_dim1, den_offset, up_dim1, up_offset, u_dim1,
    u_offset, yp_dim1, yp_offset, y_dim1, y_offset, i__1, i__2, i__3, i__4,
    i__5, i__6;

  /* Local variables */
  int k, l, n;
  double rcond;
  int kd, ln, nt, mx;
  double dmx;

  /*!but 
   *    le sous programme rtitr calcule la reponse temporelle d'un systeme 
   *    dynamique lineaire discret MIMO  represente par sa forme de 
   *    transfert: D**-1*N  soumis a une entree U 
   *!liste d'appel 
   *    subroutine rtitr(nin,nout,nu,num,inum,dgnum,den,iden,dgden, 
   *  &                 up,u,iu,yp,y,iy,job,iw,w,ierr) 
   * 
   *    int nin,nout,nu,inum,dgnum,iden,dgden,iu,iy,job,ierr,iw(nout) 
   *    double precision num(inum,nin*(dgnum+1)),den(iden,nout*(dgden+1)) 
   *    double precision up(iu,dgden+1),u(iu,nu),yp(iy,dgden+1) 
   *    double precision y(iy,nu+dgden-dgnum),w(nout) 
   * 
   *    nin    : nombre d'entrees du systeme dynamique, nombre de colonnes 
   *             de la matrice N. 
   *    nout   : nombre de sorties du systeme dynamique, nombre de lignes 
   *             de la matrice N et dimensions de D. 
   *    nu     : nombre d'echantillon de la reponse temporelle a calculer 
   *    num    : tableau contenant les coefficients (matriciels) du polynome 
   *             matriciel numerateur N. Si N=somme(Nk*z**k) alors num 
   *             est la matrice bloc : num=[N ,N ,....N       ] 
   *                                         0  1      dgnum+1 
   *             num est modifie par l'execution ( normalisation par l 
   *             coefficient de plus haut degre de D D(dgden+1) ) 
   *    inum   : nombre de ligne du tableau num dans le programme appelant 
   *    dgnum  : degre du polynome matriciel numerateur 
   *    den    : tableau contenant les coefficients (matriciels) du polynome 
   *             matriciel denominateur D. Si D=somme(Dk*z**k) alors den 
   *             est la matrice bloc : den=[D ,D ,....D       ] 
   *                                         0  1      dgden+1 
   *             den est modifie par l'execution (normalisation par la 
   *             matrice de plus haut degre D(dgden+1) ) 
   *    iden   : nombre de ligne du tableau den dans le programme appelant 
   *    dgden  : degre du polynome matriciel denominateur 
   *    up     : tableau contenant eventuellement (voir job) les dgden+1 
   *             entrees  passees du systeme stockees par colonnes: 
   *             up=[U      , ....,U  ] . Si omis up est pris nul. 
   *                  -dgden        -1 
   *    u      : tableau contenant les nu echantillons d'entrees soumis 
   *             au systeme . u=[U , .... , U    ] 
   *                              0          nu-1 
   *    iu     : nombre de lignes des tableaux up et u dans la programme 
   *             appelant 
   *    yp     : tableau contenant eventuellement (voir job) les dgden+1 
   *             sorties  passees du systeme stockees par colonnes: 
   *             yp=[Y      , .... , Y    ] . Si omis yp est pris nul. 
   *                  -dgden          -1 
   *    y      : tableau contenant apres execution les nt echantillons 
   *             de sorties du systeme . y=[Y ,....,Y                ] 
   *                                         0       nu+dgden-dgnum-1 
   *    iy     : nombre de lignes des tableaux yp et y dans la programme 
   *             appelant 
   *    job    : Si job = +-1 le programme suppose que les valeurs passees 
   *                          de U et Y sont nulles up et yp ne sont alors 
   *                          pas references 
   *             Si job = +-2 les valeurs passees de U et Y sont donnees 
   *                          par up et yp 
   *             job > 0 le sous programme effectue la normalisation 
   *             job < 0 on suppose que la normalisation a deja ete effectuee 
   *                     (rappel de rtitr pour le meme systeme) 
   *    iw ,w  : tableaux de travail. En retour w(1) contient le 
   *             conditionnement evalue par dgeco. 
   *    ierr   : indicateur d'erreur: 
   *             0 --> ok 
   *             1 --> la matrice coefficient de plus haut degre de D est 
   *                   mal conditionnee le conditionnement est estime par 
   *                   dgeco et le sous programme teste s'il est 
   *                   negligeable par rapport a 1. Dans ce cas le calcul 
   *                   est effectue 
   *             2 --> la matrice coefficient de plus haut degre de D n'est 
   *                   pas inversible. Calcul abandonne. 
   *            -1 --> argument d'appel incorrect (dimensionnement des 
   *                   tableaux negatif ou nul ou degre de N et D negatif) 
   *!sous programmes appeles 
   *    dgeco,dgesl (linpack) 
   *    ddif,ddad (blas) 
   *    dmmul (blas etendu) 
   *!methode 
   * 
   *    +inf                +inf           dn            dd 
   *    ---                 ---            ---           --- 
   *    \     -k            \     -k       \     i       \     j 
   *si U=> U z     ,    Y=   > Y z   ,  N=  > N z  ,  D=  > D z 
   *    /   k               /   k          /   i         /   j 
   *    ---                 ---            ---           --- 
   *    -inf                -inf            0             0 
   * 
   *la sortie Y verifie l'equation polynomiale D*Y=N*U qui peut s'ecrire: 
   * 
   *             dd-1          dn 
   *             ---           --- 
   *             \             \ 
   *  D  Y    = - > D Y     +   > N U              -inf < i < +inf 
   *   dd i+dd   /   k i+k     /   l i+l 
   *             ---           --- 
   *              0             0 
   * 
   *Si  D  est inversible l'equation precedente donne directement la 
   *     dd 
   *recursion permettant de calculer Y    connaissant les dd echantillons 
   *                                  i+dd 
   *precedents de Y et U 
   * 
   *!origine 
   *    Serge Steer INRIA 1988 
   *! 
   * 
   * 
   * 
   */
  /* Parameter adjustments */
  --w;
  --iw;
  num_dim1 = *inum;
  num_offset = num_dim1 + 1;
  num -= num_offset;
  den_dim1 = *iden;
  den_offset = den_dim1 + 1;
  den -= den_offset;
  u_dim1 = *iu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  up_dim1 = *iu;
  up_offset = up_dim1 + 1;
  up -= up_offset;
  y_dim1 = *iy;
  y_offset = y_dim1 + 1;
  y -= y_offset;
  yp_dim1 = *iy;
  yp_offset = yp_dim1 + 1;
  yp -= yp_offset;

  /* Function Body */
  *ierr = 0;
  nt = *nu + *dgden - *dgnum;
  if (*nin <= 0 || *nout <= 0 || nt <= 0 || *inum <= 0 || *iden <= 0
      || *iu <= 0 || *iy <= 0 || *dgden < 0 || *dgnum < 0)
    {
      *ierr = -1;
      return 0;
    }
  /* 
   */
  if (*nout == 1)
    {
      goto L40;
    }
  /*    initialisation de la reponse 
   */
  i__1 = *nout;
  for (k = 1; k <= i__1; ++k)
    {
      /* L1: */
      nsp_dset (&nt, &c_b4, &y[k + y_dim1], iy);
    }
  if (*job > 0)
    {
      /* 
       *    normalisation 
       * 
       *    factorisation du coeff de plus haut degre en z**-1 de d 
       */
      kd = *dgden * *nout + 1;
      nsp_ctrlpack_dgeco (&den[kd * den_dim1 + 1], iden, nout, &iw[1], &rcond,
			  &w[1]);
      if (rcond == 0.)
	{
	  *ierr = 2;
	  w[1] = 0.;
	  return 0;
	}
      if (rcond + 1. <= 1.)
	{
	  *ierr = 1;
	}
      /*    normalisation de N et D 
       */
      if (*dgden > 0)
	{
	  i__1 = *nout * *dgden;
	  for (k = 1; k <= i__1; ++k)
	    {
	      nsp_ctrlpack_dgesl (&den[kd * den_dim1 + 1], iden, nout, &iw[1],
				  &den[k * den_dim1 + 1], &c__0);
	      /* L10: */
	    }
	}
      i__1 = *nin * (*dgnum + 1);
      for (k = 1; k <= i__1; ++k)
	{
	  nsp_ctrlpack_dgesl (&den[kd * den_dim1 + 1], iden, nout, &iw[1],
			      &num[k * num_dim1 + 1], &c__0);
	  /* L11: */
	}
    }
  /* 
   *    recursion 
   * 
   */
  i__1 = nt - 1;
  for (n = 0; n <= i__1; ++n)
    {
      if (*dgden - n < 1 || Abs (*job) == 1)
	{
	  goto L25;
	}
      /*    termes faisant intervenir les valeurs passees 
       */
      kd = 1;
      i__2 = *dgden - n;
      for (k = 1; k <= i__2; ++k)
	{
	  nsp_calpack_dmmul (&den[kd * den_dim1 + 1], iden,
			     &yp[(n + k) * yp_dim1 + 1], iy, &w[1], nout,
			     nout, nout, &c__1);
	  nsp_calpack_ddif (nout, &w[1], &c__1, &y[(n + 1) * y_dim1 + 1],
			    &c__1);
	  kd += *nout;
	  /* L20: */
	}
      ln = 1;
      /*Computing MIN 
       */
      i__3 = *dgden - n, i__4 = *dgnum + 1;
      i__2 = Min (i__3, i__4);
      for (l = 1; l <= i__2; ++l)
	{
	  nsp_calpack_dmmul (&num[ln * num_dim1 + 1], inum,
			     &up[(n + l) * up_dim1 + 1], iu, &w[1], nout,
			     nout, nin, &c__1);
	  nsp_calpack_dadd (nout, &w[1], &c__1, &y[(n + 1) * y_dim1 + 1],
			    &c__1);
	  ln += *nin;
	  /* L21: */
	}
      /* L22: */
      /* 
       */
    L25:
      /*    autres termes 
       *Computing MAX 
       */
      i__2 = 1, i__3 = *dgden - n + 1;
      mx = Max (i__2, i__3);
      if (mx > *dgden)
	{
	  goto L27;
	}
      kd = (mx - 1) * *nout + 1;
      i__2 = *dgden;
      for (k = mx; k <= i__2; ++k)
	{
	  nsp_calpack_dmmul (&den[kd * den_dim1 + 1], iden,
			     &y[(n + k - *dgden) * y_dim1 + 1], iy, &w[1],
			     nout, nout, nout, &c__1);
	  nsp_calpack_ddif (nout, &w[1], &c__1, &y[(n + 1) * y_dim1 + 1],
			    &c__1);
	  kd += *nout;
	  /* L26: */
	}
    L27:
      if (mx > *dgnum + 1)
	{
	  goto L30;
	}
      ln = (mx - 1) * *nin + 1;
      i__2 = *dgnum + 1;
      for (l = mx; l <= i__2; ++l)
	{
	  nsp_calpack_dmmul (&num[ln * num_dim1 + 1], inum,
			     &u[(n + l - *dgden) * u_dim1 + 1], iu, &w[1],
			     nout, nout, nin, &c__1);
	  nsp_calpack_dadd (nout, &w[1], &c__1, &y[(n + 1) * y_dim1 + 1],
			    &c__1);
	  ln += *nin;
	  /* L28: */
	}
    L30:
      ;
    }
  w[1] = rcond;
  return 0;
  /* 
   */
 L40:
  /*    cas particulier d'un systeme mono-sortie. Evaluation plus directe 
   * 
   *    initialisation de la reponse 
   */
  nsp_dset (&nt, &c_b4, &y[y_offset], iy);
  if (*job > 0)
    {
      dmx = den[(*dgden + 1) * den_dim1 + 1];
      if (dmx == 0.)
	{
	  *ierr = 2;
	  w[1] = 0.;
	  return 0;
	}
      dmx = 1. / dmx;
      i__1 = *dgden + 1;
      C2F (dscal) (&i__1, &dmx, &den[den_offset], iden);
      i__1 = *nin * (*dgnum + 1);
      C2F (dscal) (&i__1, &dmx, &num[num_offset], inum);
    }
  /*    recursion 
   */
  i__1 = nt - 1;
  for (n = 0; n <= i__1; ++n)
    {
      if (*dgden - n < 1 || Abs (*job) == 1)
	{
	  goto L42;
	}
      /*    termes faisant intervenir les valeurs passees 
       */
      i__2 = *dgden - n;
      y[(n + 1) * y_dim1 + 1] =
	-C2F (ddot) (&i__2, &den[den_offset], iden,
		     &yp[(n + 1) * yp_dim1 + 1], iy);
      i__2 = *nin;
      for (l = 1; l <= i__2; ++l)
	{
	  /*Computing MIN 
	   */
	  i__4 = *dgden - n, i__5 = *dgnum + 1;
	  i__3 = Min (i__4, i__5);
	  i__6 = *inum * *nin;
	  y[(n + 1) * y_dim1 + 1] +=
	    C2F (ddot) (&i__3, &num[l * num_dim1 + 1], &i__6,
			&up[l + (n + 1) * up_dim1], iu);
	  /* L41: */
	}
    L42:
      /*    autres termes 
       *Computing MAX 
       */
      i__2 = 1, i__3 = *dgden - n + 1;
      mx = Max (i__2, i__3);
      if (mx > *dgden)
	{
	  goto L43;
	}
      i__2 = *dgden - mx + 1;
      y[(n + 1) * y_dim1 + 1] -=
	C2F (ddot) (&i__2, &den[mx * den_dim1 + 1], iden,
		    &y[(n + mx - *dgden) * y_dim1 + 1], iy);
    L43:
      if (mx > *dgnum + 1)
	{
	  goto L50;
	}
      ln = (mx - 1) * *nin;
      i__2 = *nin;
      for (l = 1; l <= i__2; ++l)
	{
	  i__3 = *dgnum + 2 - mx;
	  i__4 = *inum * *nin;
	  y[(n + 1) * y_dim1 + 1] +=
	    C2F (ddot) (&i__3, &num[(ln + l) * num_dim1 + 1], &i__4,
			&u[l + (n + mx - *dgden) * u_dim1], iu);
	  /* L44: */
	}
    L50:
      ;
    }
  w[1] = 1.;
  return 0;
  /* 
   */
}				/* rtitr_ */
