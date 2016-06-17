/* Copyright: INRIA, M Cardelli, L Baratchart INRIA sophia-Antipolis 1989, S Steer 
 */
#include "ctrlpack.h"
#include "../libcalelm/calpack.h"
#include "../include/nsp/sciio.h"
#include "../include/nsp/ode_solvers.h"

/* Table of constant values */

/* Common Block Declarations */

double no2f_gnrm = 0.0;
static int sortie_io=0, sortie_info=0, sortie_ll=0;
static double temps = 0.0;
static int arl2_comall=0;

/* constants */
static const int c_n1 = -1;
static const int c0 = 0;
static const int c1 = 1;
static const int c2 = 2;
static const int c14 = 14;
static const int c15 = 15;
static const int c17 = 17;
static const int c20 = 20;
static const int c21 = 21;
static const int c22 = 22;
static const int c23 = 23;

static const double c_b2 = 0.;
static const double c_b3 = 10.;
static const double c_b9 = 1e-5;
static const double c_b11 = 1e-7;
static const double c_b20 = .1;
static const double c_b26 = 1e-6;

typedef int (*ct_Feq)(const int *neq,const double *t, double *tq, double *tqdot);

static int nsp_ctrlpack_optml2 (ct_Feq feq, U_fp jacl2, int *neq, double *q, int *nch,double *w, int *iw);
static int nsp_ctrlpack_jacl2(int *neq, double *t, double *tq, int *ml, int *mu, double *pd, int *nrowpd);
static int nsp_ctrlpack_jacl2n(int *neq, double *t, double *tq, int *ml, int *mu, double *pd, int *nrowpd);

static int nsp_ctrlpack_domout(int *neq, double *q, double *qi, int *nbout, double *ti, double *touti, int *itol, double *rtol, double *atol, int *itask, int *istate, int *iopt, double *w, int *lrw, int *iw, int *liw, U_fp jacl2, int *mf, int *job);

static int nsp_ctrlpack_outl2(const int *ifich,const int *neq,const int *neqbac, double *tq, double *v, double *t, double *tout);

static int nsp_ctrlpack_front (const int *nq, double *tq, int *nbout, double *w);
static int nsp_ctrlpack_scapol (const int *na,const double *a,const int *nb,const double *b, double *y);
static int nsp_ctrlpack_modul (const int *neq,const double *zeror,const double *zeroi, double *zmod);
static int nsp_ctrlpack_feq1 (const int *nq,const double *t,double *tq, double *tg,const int *ng, double *tqdot, double *tr);
static int nsp_ctrlpack_feq (const int *neq, const double *t, double *tq, double *tqdot);
static int nsp_ctrlpack_feqn (const int *neq,const double *t, double *tq, double *tqdot);
static int nsp_ctrlpack_hessl2(int *neq, double *tq, double *pd, int *nrowpd);
static int nsp_ctrlpack_calsca (const int *ns,const double *ts, const double *tr, double *y0, const double *tg, const int *ng);
static int nsp_ctrlpack_mzdivq (const int *ichoix, int *nv, double *tv,const int *nq,const double *tq);
static int nsp_ctrlpack_dpmul1(const double *p1, const int *d1,const double *p2,const int *d2, double *p3);
static int nsp_ctrlpack_tild (const int *n,const double *tp, double *tpti);
static int nsp_ctrlpack_horner(const double *p,const int *dp,const double *xr, const double *xi, double *vr, double *vi);
  
/*
 *    Cette procedure a pour but de gerer l'execution dans 
 *    le cas ou un unique polynome approximant est desire 
 *
 *     subroutine arl2(f,nf,num,tq,dgmin,dgmax,errl2,w, 
 *    $     inf,ierr,ilog) 
 * 
 *    double precision tq(dgmax+1),f(nf),num(dgmax) 
 *    double precision w(*) 
 *    int dgmin,dgmax,dginit,info,ierr,iw(*) 
 * 
 *    Entry: 
 *    dgmin: degree of initial polynom when given or 0 
 *    dginit: est le premier degre pour lequel aura lieu la 
 *       recherche. 
 *    dgmax: requested degree of last approximant 
 *    tq. est le tableau contenant le polynome qui peut etre 
 *       fourni comme point de depart par l'utilisateur. 
 * 
 *    Sortie : 
 *    tq. contient la solution obtenu de degre dgmax. 
 *    num. contient les coefficients du numerateur optimal 
 *    errl2. contient l'erreur L2 pour l'optimum retourne 
 *    ierr. contient l'information sur le deroulement du programme 
 *         ierr=0 : ok 
 *         ierr=3 : boucle indesirable sur 2 ordres 
 *         ierr=4 : plantage lsode 
 *         ierr=5 : plantage dans recherche de l'intersection avec une face 
 * 
 * tableau de travail 
 *    w: dimension:  32+32*dgmax+7*ng+dgmax*ng+dgmax**2*(ng+2) 
 *    iw : dimension  29+dgmax**2+4*dgmax 
 * 
 */
 
int
nsp_ctrlpack_arl2 (double *f, int *nf, double *num, double *tq, int *dgmin,
		   int *dgmax, double *errl2, double *w, int *iw, int *inf,
		   int *ierr, int *ilog)
{
  int i1, i2;
  double d__1;

  double t, x, xx[1], tms[2], tps[2], phi0;
  int lwode, lqdot, dg, dgback, ntest1, ng, ifaceo, ncoeff, lw, dginit;
  int liwode, nch, dgr, ltg, nnn, /* liw,*/ ltq, ltr, lrw;
  
  --f;
  --tq;
  --num;
  --w;
  --iw;

  /* Function Body */
  /*Computing 2nd power 
   */
  i1 = *dgmax;
  lrw = i1 * i1 + *dgmax * 9 + 22;
  /* liw = *dgmax + 20; */
  /*    decoupage du tableau de travail w 
   */
  ncoeff = *nf;
  ng = *nf - 1;
  ltq = 1;
  ltg = ltq + *dgmax + 1;
  lwode = ltg + ng + 1;
  /*Computing 2nd power 
   */
  i1 = *dgmax;
  ltr =
    lwode + 5 + *dgmax * 5 + ng * 5 + *dgmax * ng + i1 * i1 * (ng + 1);
  /*Computing 2nd power 
   */
  i1 = *dgmax;
  /* lfree = ltr + 25 + *dgmax * 26 + ng + i1 * i1; */
  /*    les lrw elements de w suivant w(ltr) ne doivent pas etre modifies 
   *    d'un appel de optml2 a l'autre 
   */
  lw = ltr + lrw;
  /* 
   *    decoupage du tableau de travail iw 
   */
  liwode = 1;
  /* liww = liwode + 4 + (*dgmax + 1) * (*dgmax + 2); */
  /* lifree = liww + 20 + *dgmax; */
  iw[liwode + 1] = ng;
  iw[liwode + 2] = *dgmax;
  sortie_ll = 80;
  sortie_info = *inf;
  sortie_io = *ilog;
  /* 
   *test validite des arguments 
   * 
   */
  if (*dgmin > 0)
    {
      dginit = *dgmin;
      i1 = *dgmin + 1;
      C2F (dcopy) (&i1, &tq[1], &c1, &w[ltq], &c1);
    }
  else
    {
      w[ltq] = 1.;
      dginit = 1;
    }
  /* 
   */
  dgr = dginit;
  *ierr = 0;
  ntest1 = -1;
  /* 
   */
  ng = *nf - 1;
  C2F (dcopy) (nf, &f[1], &c1, &w[ltg], &c1);
  no2f_gnrm = C2F (dnrm2) (nf, &f[1], &c1);
  d__1 = 1. / no2f_gnrm;
  C2F (dscal) (nf, &d__1, &w[ltg], &c1);
  /*Computing 2nd power 
   */
  d__1 = no2f_gnrm;
  no2f_gnrm = d__1 * d__1;
  /* 
   */
  tps[0] = 1.;
  tps[1] = 1.;
  tms[0] = -1.;
  tms[1] = 1.;
  /* 
   *    ---- Boucle de calcul --------------------------------------------- 
   * 
   */
  i1 = *dgmax;
  for (nnn = dginit; nnn <= i1; ++nnn)
    {
      /* 
       */
      ifaceo = 0;
      /* 
       */
      if (nnn == dginit)
	{
	  if (*dgmin > 0)
	    {
	      dg = dginit;
	      goto L230;
	    }
	  else
	    {
	      dg = dginit - 1;
	    }
	}
      /* 
       */
    L200:
      ++dg;
      /* 
       *    -- Initialisation du nouveau point de depart -- 
       *    (dans l'espace de dimension dg , Hyperespace superieur 
       *    d'une dimension par rapport au precedent ). 
       * 
       */
      if (ntest1 == 1)
	{
	  i2 = dg - 1;
	  nsp_ctrlpack_dpmul1 (&w[ltq], &i2, tps, &c1, &w[ltr]);
	  i2 = dg + 1;
	  C2F (dcopy) (&i2, &w[ltr], &c1, &w[ltq], &c1);
	}
      else if (ntest1 == -1)
	{
	  i2 = dg - 1;
	  nsp_ctrlpack_dpmul1 (&w[ltq], &i2, tms, &c1, &w[ltr]);
	  i2 = dg + 1;
	  C2F (dcopy) (&i2, &w[ltr], &c1, &w[ltq], &c1);
	}
      /* 
       *    ------------------------ 
       * 
       */
    L230:
      dgback = dg;
      /* 
       */
      if (sortie_info > 1)
	{
	  nsp_ctrlpack_outl2 (&c20, &dg, &dgback, xx, xx, &x, &x);
	}
      /* 
       */
      nch = 1;
      iw[liwode] = dg;
      nsp_ctrlpack_optml2 (nsp_ctrlpack_feq, nsp_ctrlpack_jacl2, &iw[liwode],
			   &w[ltq], &nch, &w[ltr], &iw[1]);
      dg = iw[liwode];
      if (sortie_info > 1)
	{
	  nsp_ctrlpack_lq (&dg, &w[ltq], &w[lw], &w[ltg], &ng);
	  x = sqrt (no2f_gnrm);
	  C2F (dscal) (&dg, &x, &w[lw], &c1);
	  nsp_ctrlpack_outl2 (&nch, &dg, &dg, &w[ltq], &w[lw], &x, &x);
	  phi0 = (d__1 =
		  nsp_ctrlpack_phi (&w[ltq], &dg, &w[ltg], &ng, &w[lw]),
		  Abs (d__1));
	  lqdot = lw;
	  nsp_ctrlpack_feq (&iw[liwode], &t, &w[ltq], &w[lqdot]);
	  nsp_ctrlpack_outl2 (&c17, &dg, &dg, &w[ltq], &w[lqdot], &phi0,
			      &x);
	}
      if (nch >= 15)
	{
	  if (nch == 17)
	    {
	      i2 = dg + 1;
	      C2F (dcopy) (&i2, &w[ltq], &c1, &tq[1], &c1);
	      dgr = dg;
	      goto L231;
	    }
	  *ierr = nch - 11;
	  goto L510;
	}
      /* 
       */
      if (nch < 0)
	{
	  ++ifaceo;
	  ntest1 = -ntest1;
	  if (dg == 0)
	    {
	      goto L200;
	    }
	  goto L230;
	}
      /* 
       */
      if (sortie_info > 1)
	{
	  nsp_ctrlpack_outl2 (&c21, &dg, &dg, xx, xx, &x, &x);
	}
      nch = 2;
      iw[liwode] = dg;
      nsp_ctrlpack_optml2 (nsp_ctrlpack_feq, nsp_ctrlpack_jacl2, &iw[liwode],
			   &w[ltq], &nch, &w[ltr], &iw[1]);
      if (sortie_info > 0)
	{
	  nsp_ctrlpack_lq (&dg, &w[ltq], &w[lw], &w[ltg], &ng);
	  x = sqrt (no2f_gnrm);
	  C2F (dscal) (&dg, &x, &w[lw], &c1);
	  nsp_ctrlpack_outl2 (&nch, &dg, &dg, &w[ltq], &w[lw], &x, &x);
	  phi0 = (d__1 =
		  nsp_ctrlpack_phi (&w[ltq], &dg, &w[ltg], &ng, &w[lw]),
		  Abs (d__1));
	  lqdot = lw;
	  nsp_ctrlpack_feq (&iw[liwode], &t, &w[ltq], &w[lqdot]);
	  nsp_ctrlpack_outl2 (&c17, &dg, &dg, &w[ltq], &w[lqdot], &phi0,
			      &x);
	}
      if (nch >= 15)
	{
	  if (nch == 17)
	    {
	      i2 = dg + 1;
	      C2F (dcopy) (&i2, &w[ltq], &c1, &tq[1], &c1);
	      dgr = dg;
	      goto L231;
	    }
	  *ierr = nch - 11;
	  goto L510;
	}
      /* 
       */
      if (nch < 0)
	{
	  ++ifaceo;
	  ntest1 = -ntest1;
	  if (dg == 0)
	    {
	      goto L200;
	    }
	  goto L230;
	}
      /* 
       * 
       */
    L231:
      if (ifaceo == 8)
	{
	  if (sortie_info >= 0)
	    {
	      nsp_ctrlpack_outl2 (&c22, &dg, &dg, xx, xx, &x, &x);
	    }
	  *ierr = 3;
	  goto L510;
	}
      /* 
       */
      if (dg < nnn)
	{
	  goto L200;
	}
      i2 = dg + 1;
      C2F (dcopy) (&i2, &w[ltq], &c1, &tq[1], &c1);
      dgr = dg;
      /* 
       */
      /* L500: */
    }
  /* 
   *Fin de la recherche Optimale 
   *numerateur optimal 
   */
 L510:
  no2f_gnrm = sqrt (no2f_gnrm);
  nsp_ctrlpack_lq (&dgr, &tq[1], &w[ltr], &w[ltg], &ng);
  C2F (dcopy) (&dgr, &w[ltr], &c1, &num[1], &c1);
  C2F (dscal) (&dgr, &no2f_gnrm, &num[1], &c1);
  /*    Le gradient de la fonction critere y vaut :-tqdot 
   *    call feq(dg,t,w(ltq),tqdot) 
   *    valeur du critere 
   */
  lw = ltg + ncoeff + 1;
  *errl2 =
    sqrt (nsp_ctrlpack_phi (&tq[1], &dgr, &w[ltg], &ng, &w[lw])) *
    no2f_gnrm;
  *dgmax = dgr;
  /* 
   */
  return 0;
}


/*
 *    Cette procedure a pour but de rechercher le plus 
 *    grand nombre d'approximants pour chaque degre en partant 
 *    du degre 1 jusqu'a l'ordre nall. 
 *
 *    subroutine arl2a(f,nf,ta,nta,nall,info,ierr,io) 
 *    double precision ta(mxsol,0:nall),f(nf),w(*) 
 *    int iw(*) 
 * 
 *    entrees 
 *     f : vecteur des coefficients de Fourier 
 *     nf : nombre de coefficients de Fourrier maxi 200 
 *     nall: degre des polynomes minimums que l'on veut  atteindre. 
 *     inf : impression de la progression de l'algorithme: 
 *           0 = rien 
 *           1 = resultats intermediaires et messages d'erreur 
 *           2 = suivi detaille 
 *     ilog : etiquette logique du fichier ou sont ecrite ces informations 
 * 
 *     sorties 
 *      ta :tableau contenant les minimums  locaux a l'ordre nall 
 *      imina : nombre de minimums trouves 
 *      ierr. contient l'information sur le deroulement du programme 
 *         ierr=0 : ok 
 *         ierr=1 : trop de coefficients de fourrier (maxi 200) 
 *         ierr=2 : ordre d'approximation trop eleve 
 *         ierr=3 : boucle indesirable sur 2 ordres 
 *         ierr=4 : plantage lsode 
 *         ierr=5 : plantage dans recherche de l'intersection avec une face 
 *         ierr=7 : trop de solutions 
 * 
 *     tableaux de travail 
 *     w: 34+34*nall+7*ng+nall*ng+nall**2*(ng+2)+4*(nall+1)*mxsol 
 *     iw :29+nall**2+4*nall+2*mxsol 
 */

int nsp_ctrlpack_arl2a (double *f, int *nf, double *ta, int *mxsol, int *imina,
			int *nall, int *inf, int *ierr, int *ilog, double *w,
			int *iw)
{
  int ta_dim1, ta_offset, i__1, i__2;
  double d__1;
  int ideg, ldeg, ntbj, lter;
  int iback, j;
  int ildeg;
  double x[1];
  int iminb, dgmax, iminc, ilntb;
  int ng;
  int ltback;
  double tt;
  int ilnter, nch, ltb, ltc, neq, ltq;

  /* Parameter adjustments */
  --f;
  ta_dim1 = *mxsol;
  ta_offset = ta_dim1 + 1;
  ta -= ta_offset;
  --w;
  --iw;

  /* Function Body */
  dgmax = *nall;
  /* ncoeff = *nf; */
  ng = *nf - 1;
  ldeg = 1;
  /*Computing 2nd power 
   */
  i__1 = dgmax;
  ltb = ldeg + 33 + dgmax * 33 + ng * 7 + dgmax * ng + i__1 * i__1 * (ng + 2);
  ltc = ltb + (*nall + 1) * *mxsol;
  ltback = ltc + (*nall + 1) * *mxsol;
  lter = ltback + (*nall + 1) * *mxsol;
  ltq = ltback + (*nall + 1) * *mxsol;
  /* lfree = ltq + *nall + 1; */
  /* 
   *    decoupage du tableau de travail iw 
   */
  ildeg = 1;
  /*Computing 2nd power 
   */
  i__1 = dgmax;
  ilntb = ildeg + 29 + i__1 * i__1 + (dgmax << 2);
  ilnter = ilntb + *mxsol;
  /* ilfree = ilnter + *mxsol; */
  /*    initialisations 
   */
  sortie_io = *ilog;
  sortie_ll = 80;
  sortie_info = *inf;
  arl2_comall = *nall;
  /* 
   *test validite des arguments 
   * 
   */
  ng = *nf - 1;
  no2f_gnrm = C2F (dnrm2) (nf, &f[1], &c1);
  d__1 = 1. / no2f_gnrm;
  C2F (dscal) (nf, &d__1, &f[1], &c1);
  /*Computing 2nd power 
   */
  d__1 = no2f_gnrm;
  no2f_gnrm = d__1 * d__1;
  /* 
   * 
   */
  iback = 0;
  /* 
   */
  nsp_ctrlpack_deg1l2 (&f[1], &ng, imina, &ta[ta_offset], mxsol, &w[ldeg],
		       &iw[ildeg], ierr);
  if (*ierr > 0)
    {
      return 0;
    }
  if (*nall == 1)
    {
      goto L400;
    }
  neq = 1;
  /* 
   */
  i__1 = *nall;
  for (ideg = 2; ideg <= i__1; ++ideg)
    {
      nsp_ctrlpack_degl2 (&f[1], &ng, &neq, imina, &iminb, &iminc,
			  &ta[ta_offset], &w[ltb], &w[ltc], &iback,
			  &iw[ilntb], &w[ltback], mxsol, &w[ldeg], &iw[ildeg],
			  ierr);
      if (*ierr > 0)
	{
	  return 0;
	}
      /* 
       */
      if (*imina == 0)
	{
	  goto L201;
	}
      /* 
       */
      /* L200: */
    }
  /* 
   */
 L201:
  if (sortie_info > 1)
    {
      nsp_ctrlpack_outl2 (&c23, &neq, &iback, x, x, &tt, &tt);
    }
  /* 
   */
  if (iback > 0)
    {
      *imina = 0;
      neq = iw[ilntb];
      *inf = 1;
      i__1 = *nall - 1;
      for (ideg = neq; ideg <= i__1; ++ideg)
	{
	  /* 
	   */
	  i__2 = iback;
	  for (j = *inf; j <= i__2; ++j)
	    {
	      ntbj = iw[ilntb + j - 1];
	      if (ntbj == neq)
		{
		  C2F (dcopy) (&ntbj, &w[ltback - 1 + j], mxsol,
			       &w[ltq], &c1);
		  w[ltq + ntbj] = 1.;
		  /* 
		   */
		  nch = 1;
		  /*    remplacement de tq par w(ltq) tq n'est pas defini 
		   */
		  nsp_ctrlpack_storl2 (&neq, &w[ltq], &f[1], &ng, imina,
				       &ta[ta_offset], &iback, &iw[ilnter],
				       &w[lter], &nch, mxsol, &w[ldeg], ierr);
		}
	      else
		{
		  *inf = j;
		  goto L260;
		}
	      /* L250: */
	    }
	  /* 
	   */
	L260:
	  nsp_ctrlpack_degl2 (&f[1], &ng, &neq, imina, &iminb, &iminc,
			      &ta[ta_offset], &w[ltb], &w[ltc], &iback,
			      &iw[ilnter], &w[lter], mxsol, &w[ldeg],
			      &iw[ildeg], ierr);
	  if (*ierr > 0)
	    {
	      return 0;
	    }
	  /* 
	   */
	  /* L300: */
	}
    }
  /* 
   */
 L400:
  /* 
   */
  return 0;
}

/* calcule la fonction phi 
 *    Entree : 
 *       tg . tableau des coefficients de la fonction g . 
 *       ng . degre du polynome g 
 *       tq . tableau des coefficients du polynome q 
 *       nq . degre du polynome q 
 *       w  . tableau de travail de taille nq+ng+1 
 *    Sortie : 
 *       phi 
 */

double nsp_ctrlpack_phi (double *tq,const int *nq, double *tg,const int *ng, double *w)
{
  int ltlq, ltr;
  double y0;
  --tq;
  --w;
  --tg;
  ltr = 1;
  nsp_ctrlpack_lq (nq, &tq[1], &w[ltr], &tg[1], ng);
  ltlq = ltr;
  nsp_ctrlpack_calsca (nq, &tq[1], &w[ltlq], &y0, &tg[1], ng);
  return 1. - y0; 
}

/*
 *    cette routine calcule a  partir de g(z) et q(z) le 
 *    polynome Lq(z) defini comme le reste , tilde , de la division 
 *    par q(z) du produit g(z) par le tilde de q(z) . 
 *    Entree : 
 *       tg . tableau des coefficients de la fonction g . 
 *       ng . degre du polynome g 
 *       tq . tableau des coefficients du polynome q 
 *       nq . degre du polynome q 
 *    Sortie : 
 *       tr . tableau [tlq,tvq] 
 *            tlq =tr(1:nq) coefficients du polynome Lq 
 *            tvq =tr(nq+1:nq+ng+1) coefficients du quotient vq de la 
 *                   division par q du polynome gqti . 
 */

int nsp_ctrlpack_lq (const int *nq, const double *tq, double *tr, double *tg,const int *ng)
{
  int i__1;
  double temp;
  int j;
  --tq;
  --tg;
  --tr;
  
  /* Function Body */
  nsp_ctrlpack_tild (nq, &tq[1], &tr[1]);
  nsp_ctrlpack_dpmul1 (&tg[1], ng, &tr[1], nq, &tr[1]);
  /* 
   *    division euclidienne de tg*tq~ par tq 
   */
  i__1 = *ng + *nq;
  nsp_ctrlpack_dpodiv (&tr[1], &tq[1], &i__1, nq);
  /* 
   *    calcul du tilde du reste  sur place 
   */
  i__1 = *nq / 2;
  for (j = 1; j <= i__1; ++j)
    {
      temp = tr[j];
      tr[j] = tr[*nq + 1 - j];
      tr[*nq + 1 - j] = temp;
      /* L10: */
    }
  return 0;
}

/*
 *    1. a: avant execution c'est le tableau des coefficients du 
 *          polynome dividende range suivant les puissances croissantes 
 *          de la variable (ordre na+1). 
 *          apres execution,contient dans les nb premiers elements 
 *          le tableau des coefficients du reste ordonne suivant les 
 *          puissances croissantes, et dans les (na-nb+1) derniers elements, 
 *          le tableau des coefficients du polynome quotient range suivant 
 *          les puissances croissantes de la variable. 
 *    2. b: tableau des coefficients du polynome diviseur range suivant 
 *          les puissances croissantes de la variable (ordre nb+1). 
 *    3. na: degre du polynome a. 
 *    4. nb: degre du polynome b. 
 * 
 */

int nsp_ctrlpack_dpodiv(double *a, const double *b,const int *na,const int *nb)
{
  int i__1;
  int i__, l, n;
  double q;
  int n1, n2, nb1;
  --b;
  --a;
  l = *na - *nb + 1;
 L2:
  if (l <= 0) {
    goto L5;
  } else {
    goto L3;
  }
 L3:
  n = l + *nb;
  q = a[n] / b[*nb + 1];
  nb1 = *nb + 1;
  i__1 = nb1;
  for (i__ = 1; i__ <= i__1; ++i__) {
    n1 = *nb - i__ + 2;
    n2 = n - i__ + 1;
    /* L4: */
    a[n2] -= b[n1] * q;
  }
  a[n] = q;
  --l;
  goto L2;
 L5:
  return 0;
}

/*
 *    pour un polynome p(z)  l'operation tild aboutit a un polynome 
 *    ptild(z) defini par la relation suivante : 
 *      ptild(z)= z**n * p(1/z) . 
 *
 *    Entree : - tp . vecteur des coefficients du polynome a "tilder" . 
 *             -  n . degre du polynome "tp" 
 * 
 *    Sortie : - tpti . vecteur des coefficients du polynome resultant . 
 * 
 */

int nsp_ctrlpack_tild (const int *n, const double *tp, double *tpti)
{
  int j;
  for (j = 0; j <= *n; ++j)
    {
      tpti[j] = tp[*n - j];
    }
  return 0;
}

/*
 *    Cette procedure est charge de determiner quelle est 
 *    la face franchie par la trajectoire du gradient. 
 *
 *    subroutine watfac(nq,tq,nface,newrap,w) 
 *    dimension tq(0:nq),w(3*nq+1) 
 * 
 *    Entrees : 
 *    - nq. est toujours le degre du polynome q(z) 
 *    - tq. est le tableau des coefficients de ce polynome. 
 * 
 *    Sortie  : 
 *    - nface contient l indice de la face que le chemin 
 *      de la recherche a traverse. 
 *      Les valeurs possibles de nface sont: 0 pour la face 
 *      complexe, 1 pour la face 'z+1' et -1 pour la face  'z-1'. 
 *    - newrap est un parametre indiquant s'il est necessaire 
 *      ou pas d'effectuer un nouveau un rapprochement. 
 * 
 *    Tableaux de travail 
 *    - w : 3*nq+1 
 */

int nsp_ctrlpack_watfac (const int *nq, const double *tq, int *nface, int *newrap, double *w)
{
  int i__1, fail, indi=0, lpol, nmod1, j, lzmod, lzi, lzr, degree;
    
  --tq;
  --w;

  lpol = 1;
  lzr = lpol + *nq + 1;
  lzi = lzr + *nq;
  lzmod = lpol;
  /* lfree = lzi + *nq; */
  /* 
   */
  i__1 = *nq + 1;
  C2F (dcopy) (&i__1, &tq[1], &c1, &w[lpol], &c_n1);
  degree = *nq;/* when failing degree is set to the number of roots found */
  nsp_ctrlpack_rpoly (&w[lpol], &degree, &w[lzr], &w[lzi], &fail);
  nsp_ctrlpack_modul (&degree, &w[lzr], &w[lzi], &w[lzmod]);
  /* 
   */
  nmod1 = 0;
  i__1 = degree;
  for (j = 1; j <= i__1; ++j)
    {
      if (w[lzmod - 1 + j] >= 1.)
	{
	  ++nmod1;
	  if (nmod1 == 1)
	    {
	      indi = j;
	    }
	}
      /* L110: */
    }
  /* 
   */
  if (nmod1 == 2)
    {
      if (w[lzi - 1 + indi] == 0.)
	{
	  *newrap = 1;
	  return 0;
	}
      else
	{
	  *nface = 0;
	}
    }
  /* 
   */
  if (nmod1 == 1)
    {
      if (w[lzr - 1 + indi] > 0.)
	{
	  *nface = -1;
	}
      else
	{
	  *nface = 1;
	}
    }
  /* 
   */
  *newrap = 0;
  /* 
   */
  return 0;
}

/*
 *    ce sous programme calcule le vecteur des modules d'un vecteur 
 *    de nombres complexes 
 *
 *    subroutine modul(neq,zeror,zeroi,zmod) 
 *    double precision zeror(neq),zeroi(neq),zmod(neq) 
 * 
 *    neq : longueur des vecteurs 
 *    zeror (zeroi) : vecteurs des parties reelles (imaginaires) du 
 *           vecteur de nombres complexes 
 *    zmod : vecteur des modules 
 */

int nsp_ctrlpack_modul (const int *neq,const double *zeror,const double *zeroi, double *zmod)
{
  double d1, d2;
  int i;
  for (i = 0; i < *neq; ++i)
    {
      d1 = zeror[i];
      d2 = zeroi[i];
      zmod[i] = sqrt (d1 * d1 + d2 * d2);
    }
  return 0;
}

/*
 * ce sous programme effectue le produit polynomial: 
 * 
 *               p3(x) = p1(x) * p2(x) 
 * 
 *    subroutine dpmul1(p1,d1,p2,d2,p3) 
 *    double precision p1(d1+1),p2(d2+1),p3(d1+d2+1) 
 *    integer d1,d2,d3 
 * 
 *    p1 : contient les coefficient du premier polynome ranges 
 *         suivant les puissances croissantes 
 *    p2 : contient les coefficients du second polynome ranges 
 *         suivant les puissances croissantes 
 *    p3 :contient les coefficient du resultats. 
 *        p3 peut designer la meme adresse que p1 ou p2 
 *    d1,d2 : degre respectifs des  polynomesp1 et p2 
 */


int nsp_ctrlpack_dpmul1(const double *p1, const int *d1,const double *p2,const int *d2, double *p3)
{
  int i__1, k, l, d3, l1, l2, l3, m3;

  --p3;
  --p2;
  --p1;

  
  l = 1;
  l1 = *d1 + 1;
  l2 = *d2 + 1;
  d3 = *d1 + *d2;
  l3 = d3 + 1;
  /* 
   */
  m3 = Min(l1,l2);
  i__1 = m3;
  for (k = 1; k <= i__1; ++k) {
    p3[l3] = ddot_(&l, &p1[l1], &c1, &p2[l2], &c_n1);
    ++l;
    --l3;
    --l1;
    --l2;
    /* L10: */
  }
  --l;
  /* 
   */
  if (l1 == 0) {
    goto L30;
  }
  m3 = l1;
  i__1 = m3;
  for (k = 1; k <= i__1; ++k) {
    p3[l3] = ddot_(&l, &p1[l1], &c1, &p2[1], &c_n1);
    --l1;
    --l3;
    /* L20: */
  }
  goto L40;
 L30:
  if (l2 == 0) {
    goto L40;
  }
  m3 = l2;
  i__1 = m3;
  for (k = 1; k <= i__1; ++k) {
    p3[l3] = ddot_(&l, &p1[1], &c1, &p2[l2], &c_n1);
    --l2;
    --l3;
    /* L31: */
  }
  /* 
   */
 L40:
  if (l3 == 0) {
    return 0;
  }
  m3 = l3;
  i__1 = m3;
  for (k = 1; k <= i__1; ++k) {
    --l;
    p3[l3] = ddot_(&l, &p1[1], &c1, &p2[1], &c_n1);
    --l3;
    /* L41: */
  }
  return 0;
}

/*
 *    cette subroutine calcule la valeur du polynome p au point x 
 *    suivant la formule de horner 
 *
 *    subroutine horner(p,dp,xr,xi,vr,vi) 
 * 
 *    double precision p(dp+1),xr,xi,vr,vi 
 *    int dp 
 * 
 *    p : tableau contenant les coefficients du polynome ranges 
 *        consecutivement et par puissance croissante 
 *    dp : degre du polynome 
 *    xr,xi : parties reelle et imaginaire de l'argument 
 *    vr,vi : parties reelle et imaginaire du resultat 
 *
 *    Serge Steer INRIA 1986 
 */

int nsp_ctrlpack_horner(const double *p,const int *dp,const double *xr, const double *xi, double *vr, double *vi)
{
  int i1;
  int i;
  double t;
  int ip;
  --p;
  /* Function Body */
  ip = *dp + 1;
  *vr = p[ip];
  *vi = 0.;
  if (*dp == 0) {
    return 0;
  }
  if (*xi != 0.) {
    goto L20;
  }
  /* real */
  i1 = *dp;
  for (i = 1; i <= i1; ++i) {
    *vr = *vr * *xr + p[ip - i];
    /* L10: */
  }
  return 0;
  /* complexe */
 L20:
  i1 = *dp;
  for (i = 1; i <= i1; ++i) {
    t = *vr * *xr - *vi * *xi + p[ip - i];
    *vi = *vi * *xr + *vr * *xi;
    *vr = t;
    /* L21: */
  }
  return 0;
}

/*
 *    cette subroutine a pour but de calculer le produit 
 *    scalaire de deux polynomes 
 *
 *    subroutine scapol(na,a,nb,b,y) 
 *    Entree : 
 *     a. est le premier polynome de degre na 
 *     b. est le second polynome du produit, et est de degre nb 
 * 
 *    Sortie : 
 *     y. est le resultat du produit scalaire <a,b> 
 */

int nsp_ctrlpack_scapol (const int *na,const double *a,const int *nb,const double *b, double *y)
{
  int k;
  int nmax =  (*na >= *nb) ?  *nb: *na;
  double aux = 0.;
  for (k = 0; k <= nmax ; ++k)
    {
      aux += a[k] * b[k];
    }
  *y = aux;
  return 0;
}

/*
 *    cette routine calcule le nombre de racines  du polynome q(z) qui 
 *    sont situees a l'exterieur du cercle unite . 
 *
 *    subroutine front(nq,tq,nbout,w) 
 *    dimension tq(0:*),w(*) 
 *    Entree : 
 *    - nq . est le degre du polynome q(z) 
 *    - tq   . le tableau du polynome en question 
 * 
 *    Sortie : 
 *    -nbout . est le nombre de racine a l'exterieur du  du cercle unite 
 *    tableau de travail 
 *    -w 3*nq+1 
 */

int nsp_ctrlpack_front (const int *nq, double *tq, int *nbout, double *w)
{
  int i1 = *nq+1, fail, nbon, lpol=1 , i, lzmod, lzi, lzr,degree=*nq;
  
  --tq;
  --w;

  lzr = lpol + *nq + 1;
  lzi = lzr + *nq;
  lzmod = lpol;
  
  C2F (dcopy) (&i1, &tq[1], &c1, &w[lpol], &c_n1);
  nsp_ctrlpack_rpoly (&w[lpol], &degree, &w[lzr], &w[lzi], &fail);
  nsp_ctrlpack_modul (&degree, &w[lzr], &w[lzi], &w[lzmod]);
  /* 
   */
  *nbout = 0;
  nbon = 0;
  for (i = 1; i <= degree ; ++i)
    {
      if (w[lzmod - 1 + i] > 1.)
	{
	  ++(*nbout);
	}
      if (w[lzmod - 1 + i] == 1.)
	{
	  ++nbon;
	}
    }
  return 0;
}

/*
 *    cette subroutine contient les differents messages 
 *    a afficher suivant le deroulement de l execution. 
 *
 *    Entrees : 
 *    - ifich. est l'indice du message (-1 pour une 
 *       intersection avec la face, 1 pour une localisation 
 *       d un minimum local, 2 pour le resultat a un certain 
 *    degre ...) 
 *    - neq. est le degre (ou dimension) ou se situe 
 *       la recherche actuelle. 
 *    - neqbac. contient la valeur du degre avant le 1er 
 *       appel de lsoda 
 *    - tq. est le tableau contenant les coefficients du 
 *       polynome. 
 *    - w. trableau de travail 
 */

static int nsp_ctrlpack_dmdspf (const double *x,const int *nx,const int *m,const int *n,
				const int *maxc,const int *ll,const int *lunit);

int
nsp_ctrlpack_outl2 (const int *ifich,const int *neq,const int *neqbac, double *tq, double *v,
		    double *t, double *tout)
{
  int i__3;
  double errel;
  int mxsol, nq;
  char buf[80];
  /* int ltqdot, lpd, ltg, ltr, ltq; */
  double phi0;

  /* Parameter adjustments */
  --v;
  --tq;
  --neq;
  /* Function Body */
  nq = neq[1];
  /* 
   * 
   */
  sprintf (buf, "(%3d)", neq[1]);
  if (*ifich >= 80)
    {
      goto L400;
    }
  if (*ifich >= 70)
    {
      goto L350;
    }
  if (*ifich >= 60)
    {
      goto L300;
    }
  if (*ifich >= 50)
    {
      goto L250;
    }
  if (*ifich >= 40)
    {
      goto L200;
    }
  if (*ifich >= 30)
    {
      goto L150;
    }
  if (*ifich >= 20)
    {
      goto L100;
    }
  /* ng = neq[2]; */
  /* ltq = 1;
     ltg = ltq + neq[3] + 1; */
  /* ltqdot = ltg + ng + 1 + (nq + ng + 1); */
  /* ltr = ltqdot + nq; */
  /* lpd = ltr + ng + nq + 1; */
  /* ltrti = lpd + nq * nq; */
  /* lfree = ltrti + nq + 1; */
  if (*ifich < 17)
    {
      sprintf (buf, "(%3d)", nq);
      Sciprintf
	("----------------- TRACE AT  ORDER: %s ----------------------", buf);
      if (*ifich < 0)
	{
	  /*Writing concatenation 
	   */
	  Sciprintf ("Intersection with a degree %s facet ", buf);
	}
      else if (*ifich == 1)
	{
	  Sciprintf (" Minimum found for order: %s", buf);
	}
      else if (*ifich == 2)
	{
	  Sciprintf (" Local minimum found for order: %s", buf);
	}
      else if (*ifich == 3)
	{
	  Sciprintf (" Maximum found for order: %s", buf);
	}
      else if (*ifich == 4)
	{
	  Sciprintf (" Local maximum found for order: %s", buf);
	}
      else if (*ifich == 14 || *ifich == 15 || *ifich == 16)
	{
	  Sciprintf (" Reached point:");
	}
      Sciprintf ("Denominator:");
      i__3 = nq + 1;
      nsp_ctrlpack_dmdspf (&tq[1], &c1, &c1, &i__3, &c15, &sortie_ll,
			   &sortie_io);
      Sciprintf ("Numerator", 9L);
      nsp_ctrlpack_dmdspf (&v[1], &c1, &c1, &nq, &c15, &sortie_ll,
			   &sortie_io);
    }
  else
    {
      Sciprintf ("Gradient :", 10L);
      nsp_ctrlpack_dmdspf (&v[1], &c1, &c1, &nq, &c15, &sortie_ll,
			   &sortie_io);
      phi0 = *t;
      Sciprintf (" Error L2 norm                    : %14.7f\n", phi0);
      Sciprintf (" Datas L2 norm                    : %14.7f\n", *tout);
      errel = sqrt (phi0);
      Sciprintf (" Relative error norm              : %14.7f\n", errel);
      Sciprintf
	("---------------------------------------------------------------\n");
    }
 L100:
  /*    messages du sous programme arl2 
   */
  if (*ifich == 20)
    {
      Sciprintf
	("LSODE 1  ------------------------------------------------------\n");
      Sciprintf (" dg=%d dgback = %d\n", nq, *neqbac);
    }
  else if (*ifich == 21)
    {
      Sciprintf
	("LSODE 2  ------------------------------------------------------\n");
    }
  else if (*ifich == 22)
    {
      Sciprintf (" Unwanted loop beetween two orders..., Stop\n");
    }
  else if (*ifich == 23)
    {
      Sciprintf ("found %d face returns\n", *neqbac);
    }
  return 0;
 L150:
  /*    messages du sous programme optml2    */
  if (*ifich == 30)
    {
      Sciprintf
	("Optml2 ========== parameters before lsode call =================");
      Sciprintf ("t=%f tout=%f\n", *t, *tout);
      Sciprintf (" Q initial :");
      i__3 = nq + 1;
      nsp_ctrlpack_dmdspf (&tq[1], &c1, &c1, &i__3, &c14, &sortie_ll,
			   &sortie_io);
    }
  else if (*ifich == 31)
    {
      Sciprintf
	("Optml2 ========== parameters after lsode call   ================");
      Sciprintf ("|grad|= %f nbout=%d t=%f tout=%f\n", v[1], *neqbac, *t,
		 *tout);
      Sciprintf (" Q final :");
      i__3 = nq + 1;
      nsp_ctrlpack_dmdspf (&tq[1], &c1, &c1, &i__3, &c14, &sortie_ll,
			   &sortie_io);
      Sciprintf
	("Optml2 =========== End of LSODE description======================");
    }
  else if (*ifich == 32)
    {
      Sciprintf (" Lsode: no convergence (istate=-5)\n");
      Sciprintf (" new call with reduced tolerances\n");
    }
  else if (*ifich == 33)
    {
      Sciprintf (" Lsode: no convergence (istate=-6)\n");
    }
  else if (*ifich == 34)
    {
      Sciprintf ("t=%14.7f tout=%14.7f\n", *t, *tout);
      Sciprintf ("itol=%d rtol=%14.7f\n", *neqbac, v[1]);
      Sciprintf ("atol=");
      nsp_ctrlpack_dmdspf (&tq[1], &c1, &c1, &nq, &c14, &sortie_ll,
			   &sortie_io);
    }
  else if (*ifich == 35)
    {
      Sciprintf ("itol=%d \n", *neqbac);
      Sciprintf ("rtol=");
      nsp_ctrlpack_dmdspf (&v[1], &c1, &c1, &nq, &c14, &sortie_ll,
			   &sortie_io);
      Sciprintf ("atol=");
      nsp_ctrlpack_dmdspf (&tq[1], &c1, &c1, &nq, &c14, &sortie_ll,
			   &sortie_io);
    }
  else if (*ifich == 36)
    {
      Sciprintf ("new call with increased tolerances");
    }
  else if (*ifich == 37)
    {
      Sciprintf (" LSODE stops with istate = %d\n", *neqbac);
    }
  else if (*ifich == 38)
    {
      Sciprintf (" Lsode stops: too many integration steps  (istate= -1)\n");
      Sciprintf ("   new call to go further");
    }
  else if (*ifich == 39)
    {
      Sciprintf ("Repeated LSODE failure --  OPTML2 stops");
    }
  return 0;
 L200:
  /*message relatifs au sous programme domout */
  if (*ifich == 40)
    {
      Sciprintf
	("********LOOKING FOR INTERSECTION  WITH STABILITY DOMAIN BOUNDS ********");
      Sciprintf ("kmax=%d\n", (*neqbac));
    }
  else if (*ifich == 41)
    {
      Sciprintf
	("Domout ========== parameters before lsode call =================");
      Sciprintf ("t=%f tout=%f\n", *t, *tout);
      Sciprintf (" initial Q :");
      i__3 = nq + 1;
      nsp_ctrlpack_dmdspf (&tq[1], &c1, &c1, &i__3, &c14, &sortie_ll,
			   &sortie_io);
    }
  else if (*ifich == 42)
    {
      Sciprintf
	("Domout ========== parameters after lsode call  =================");
      Sciprintf (" nbout=%d t=%f tout=%f\n", *neqbac, *t, *tout);
      Sciprintf (" Q final :");
      i__3 = nq + 1;
      nsp_ctrlpack_dmdspf (&tq[1], &c1, &c1, &i__3, &c14, &sortie_ll,
			   &sortie_io);
      Sciprintf
	("Domout ========== End of LSODE description======================");
    }
  else if (*ifich == 43)
    {
      Sciprintf (" Lsode stops: too many integration steps  (istate= -1)");
      Sciprintf ("   new call to go further");
    }
  else if (*ifich == 44)
    {
      Sciprintf ("Number of unstable roots: %d\n", *neqbac);
    }
  else if (*ifich == 45)
    {
      Sciprintf
	(" lsode problem (istate=%d) when looking for intersection with ",
	 *neqbac);
      Sciprintf ("   stability domain bounds... Stop\n");
    }
  else if (*ifich == 46)
    {
      Sciprintf ("watface --> nface= %d", *neqbac);
      Sciprintf ("onface --> neq= %d", nq);
      Sciprintf (" yi=%f yf=%f", *t, *tout);
      i__3 = nq + 1;
      nsp_ctrlpack_dmdspf (&tq[1], &c1, &c1, &i__3, &c14, &sortie_ll,
			   &sortie_io);
    }
  else if (*ifich == 47)
    {
      Sciprintf (" goto 314 ===========================");
      Sciprintf (" qi = ", 6L);
      i__3 = nq + 1;
      nsp_ctrlpack_dmdspf (&v[1], &c1, &c1, &i__3, &c14, &sortie_ll,
			   &sortie_io);
    }
  else if (*ifich == 47)
    {
      Sciprintf
	("********END OF INTERSECTION  WITH STABILITY DOMAIN BOUNDS SEARCH ********");
    }
  return 0;
 L250:
  /*    messages de deg1l2 et degl2  */
  if (*ifich == 50)
    {
      Sciprintf (" Non convergence  ... look for next solution .");
    }
  else if (*ifich == 51)
    {
      Sciprintf
	("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
      Sciprintf (" Look for all minina of degree: %d\n", nq);
      Sciprintf
	("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
    }
  else if (*ifich == 52)
    {
      Sciprintf
	("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
      Sciprintf (" End of search degree %d minima\n", nq);
      Sciprintf
	("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
      mxsol = (int) (*tout);
      Sciprintf (" Q(0) :", 7L);
      nsp_ctrlpack_dmdspf (&tq[1], &c1, &c1, &nq, &c14, &sortie_ll,
			   &sortie_io);
      Sciprintf (" corresponding relatives errors");
      nsp_ctrlpack_dmdspf (&tq[mxsol + 1], &c1, &c1, neqbac, &c14,
			   &sortie_ll, &sortie_io);
    }
  else if (*ifich == 53)
    {
      Sciprintf
	("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
      Sciprintf (" End of search degree %d minima\n", nq);
      Sciprintf
	("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
      mxsol = (int) (*tout);
      Sciprintf (" corresponding denominators:");
      nsp_ctrlpack_dmdspf (&tq[1], &mxsol, neqbac, &nq, &c14, &sortie_ll,
			   &sortie_io);
      Sciprintf (" relatives errors");
      nsp_ctrlpack_dmdspf (&tq[mxsol * nq + 1], &mxsol, neqbac, &c1, &c14,
			   &sortie_ll, &sortie_io);
    }
  return 0;
 L300:
  /*messages de roogp  */
  if (*ifich == 60)
    {
      Sciprintf
	("Rootgp : No value found for Beta when looking for intersection with a complex facet");
      Sciprintf ("        Stop");
    }
  return 0;
 L350:
  /*messages de onface */
  if (*ifich == 70)
    {
      buf[3] = '\0';
      Sciprintf ("(%2d) Domain boundary reached,\n");
      Sciprintf ("    Order is deacreased by %s\n", buf);
    }
  else if (*ifich == 71)
    {
      Sciprintf ("Remainder:");
      nsp_ctrlpack_dmdspf (&tq[1], &c1, &c1, &nq, &c14, &sortie_ll,
			   &sortie_io);
    }
  return 0;
 L400:
  if (*ifich == 80)
    {
      Sciprintf ("Already reached minimum\n");
    }
  else if (*ifich == 81)
    {
      Sciprintf ("Preserve minimun in  tback\n");
    }
  return 0;
}



/*    x : tableau contenant les coefficients de la matrice x 
 *    nx : entier definissant le rangement dans x 
 *    m : nombre de ligne de la matrice 
 *    n : nombre de colonnes de la matrice 
 *    maxc : nombre de caracteres maximum autorise pour 
 *           representer un nombre 
 *    ll : longueur de ligne maximum admissible 
 *    lunit : etiquette logique du support d'edition 
 */

static int nsp_ctrlpack_dmdspf (const double *x,const int *nx,const int *m,const int *n,
				const int *maxc,const int *ll,const int *lunit)
{
  return 0;
}

/*
 *    jacl2 cree la matrice  jacobienne necessaire a Lsoda, 
 *    qui correspond en fait a la hessienne du probleme 
 *    d'approximation L2. 
 *
 *    entree : 
 *    - neq. tableau entier de taille 3+(nq+1)*(nq+2) 
 *        neq(1)=nq est le degre effectif du polynome q 
 *        neq(2)=ng est le nombre de coefficient de fourier 
 *        neq(3)=dgmax degre maximum pour q (l'adresse des coeff de 
 *              fourier dans tq est neq(3)+2 
 *        neq(4:(nq+1)*(nq+2)) tableau de travail entier 
 *    - t est une variable parametrique necessaire a Lsoda. 
 *    - tq. tableau reel de taille au moins 
 *              7+dgmax+5*nq+6*ng+nq*ng+nq**2*(ng+1) 
 *        tq(1:nq+1) est le tableau des coefficients du polynome q. 
 *        tq(dgmax+2:dgmax+ng+2) est le tableau des coefficients 
 *                     de fourier 
 *        tq(dgmax+ng+3:) est un tableau de travail de taille au moins 
 *                        5+5*nq+5*ng+nq*ng+nq**2*(ng+1) 
 *    - ml et mu sont les parametres du stockage par bande 
 *       de la matrice qui n a pas lieu ici ,ils donc ignores. 
 * 
 *    sortie : 
 *    - pd. est le tableau ou l on range la matrice pleine 
 *      dont les elements sont etablis par la sub. Hessien 
 *    - nrowpd. est le nombre de ligne du tableau pd 
 */

int
nsp_ctrlpack_jacl2 (int *neq, double *t, double *tq, int *ml, int *mu,
		    double *pd, int *nrowpd)
{
  int pd_dim1, pd_offset;
  --neq;
  --tq;
  pd_dim1 = *nrowpd;
  pd_offset = pd_dim1 + 1;
  pd -= pd_offset;

  /* Function Body */
  nsp_ctrlpack_hessl2 (&neq[1], &tq[1], &pd[pd_offset], nrowpd);
  /* nq = neq[1]; */
  /*     write(6,'(''jac='')') 
   *     do 10 i=1,nq 
   *        write(6,'(5(e10.3,2x))') (pd(i,j),j=1,nq) 
   *10   continue 
   * 
   */
  return 0;
}				/* jacl2_ */

/*
 *    jacl2 cree la matrice  jacobienne necessaire a Lsoda, 
 *    qui correspond en fait a la hessienne du probleme 
 *    d'approximation L2. 
 *
 *    entree : 
 *    - neq. tableau entier de taille 3+(nq+1)*(nq+2) 
 *        neq(1)=nq est le degre effectif du polynome q 
 *        neq(2)=ng est le nombre de coefficient de fourier 
 *        neq(3)=dgmax degre maximum pour q (l'adresse des coeff de 
 *              fourier dans tq est neq(3)+2 
 *        neq(4:(nq+1)*(nq+2)) tableau de travail entier 
 *    - t est une variable parametrique necessaire a Lsoda. 
 *    - tq. tableau reel de taille au moins 
 *              7+dgmax+5*nq+6*ng+nq*ng+nq**2*(ng+1) 
 *        tq(1:nq+1) est le tableau des coefficients du polynome q. 
 *        tq(dgmax+2:dgmax+ng+2) est le tableau des coefficients 
 *                     de fourier 
 *        tq(dgmax+ng+3:) est un tableau de travail de taille au moins 
 *                        5+5*nq+5*ng+nq*ng+nq**2*(ng+1) 
 *    - ml et mu sont les parametres du stockage par bande 
 *       de la matrice qui n a pas lieu ici ,ils donc ignores. 
 * 
 *    sortie : 
 *    - pd. est le tableau ou l on range la matrice pleine 
 *      dont les elements sont etablis par la sub. Hessien 
 *    - nrowpd. est le nombre de ligne du tableau pd 
 */

int
nsp_ctrlpack_jacl2n (int *neq, double *t, double *tq, int *ml, int *mu,
		     double *pd, int *nrowpd)
{
  int pd_dim1, pd_offset, i__1, i__2;
  int i__, j;
  int nq;

  --neq;
  --tq;
  pd_dim1 = *nrowpd;
  pd_offset = pd_dim1 + 1;
  pd -= pd_offset;

  /* Function Body */
  nsp_ctrlpack_hessl2 (&neq[1], &tq[1], &pd[pd_offset], nrowpd);
  nq = neq[1];
  i__1 = nq;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = nq;
      for (j = 1; j <= i__2; ++j)
	{
	  pd[i__ + j * pd_dim1] = -pd[i__ + j * pd_dim1];
	}
    }
  return 0;
}

/*
 *     Etablir la valeur de l'oppose du gradient au point q 
 *
 *    subroutine feq(neq,t,tq,tqdot) 
 *    - neq. tableau entier de taille 3+(nq+1)*(nq+2) 
 *        neq(1)=nq est le degre effectif du polynome tq (ou q). 
 *        neq(2)=ng est le nombre de coefficient de fourier 
 *        neq(3)=dgmax degre maximum pour q (l'adresse des coeff de fourier dans 
 *              tq est neq(3)+2 
 *    - t  . variable parametrique necessaire a l'execution de 
 *        la routine lsoda . 
 *    - tq. tableau reel de taille au moins 
 *              3+dgmax+nq+2*ng 
 *        tq(1:nq+1) est le tableau des coefficients du polynome q. 
 *        tq(dgmax+2:dgmax+ng+2) est le tableau des coefficients 
 *                     de fourier 
 *        tq(dgmax+ng+3:) est un tableau de travail de taille au moins 
 *                        nq+ng+1 
 *    Sortie : 
 *    - tqdot . tableau contenant les opposes des coordonnees du 
 *             gradient de la fonction PHI au point q 
 *Remarque 
 *    la structure particuliere  pour neq et tq est liee au fait que feq peut 
 *    etre appele comme un external de lsode 
 */

int nsp_ctrlpack_feq (const int *neq, const double *t, double *tq, double *tqdot)
{
  int ng, nq, iw, itg, itq;

  --tqdot;
  --tq;
  --neq;

  /* Function Body */
  nq = neq[1];
  ng = neq[2];
  /* 
   *    decoupage du tableau tq 
   */
  itq = 1;
  itg = itq + neq[3] + 1;
  iw = itg + ng + 1;
  nsp_ctrlpack_feq1 (&nq, t, &tq[1], &tq[itg], &ng, &tqdot[1], &tq[iw]);
  return 0;
}

/*
 *     Etablir la valeur  du gradient au point q 
 *
 *    subroutine feqn(neq,t,tq,tqdot) 
 *    - neq. tableau entier de taille 3+(nq+1)*(nq+2) 
 *        neq(1)=nq est le degre effectif du polynome tq (ou q). 
 *        neq(2)=ng est le nombre de coefficient de fourier 
 *        neq(3)=dgmax degre maximum pour q (l'adresse des coeff de fourier dans 
 *              tq est neq(3)+2 
 *    - t  . variable parametrique necessaire a l'execution de 
 *        la routine lsoda . 
 *    - tq. tableau reel de taille au moins 
 *              3+dgmax+nq+2*ng 
 *        tq(1:nq+1) est le tableau des coefficients du polynome q. 
 *        tq(dgmax+2:dgmax+ng+2) est le tableau des coefficients 
 *                     de fourier 
 *        tq(dgmax+ng+3:) est un tableau de travail de taille au moins 
 *                        nq+ng+1 
 *    Sortie : 
 *    - tqdot . tableau contenant les opposes des coordonnees du 
 *             gradient de la fonction PHI au point q 
 *Remarque 
 *    la structure particuliere  pour neq et tq est liee au fait que feq peut 
 *    etre appele comme un external de lsode 
 */

int nsp_ctrlpack_feqn (const int *neq,const double *t, double *tq, double *tqdot)
{
  int i__1;
  int i__, ng, nq, iw, itg, itq;

  /* Parameter adjustments */
  --tqdot;
  --tq;
  --neq;

  /* Function Body */
  nq = neq[1];
  ng = neq[2];
  /* 
   *    decoupage du tableau tq 
   */
  itq = 1;
  itg = itq + neq[3] + 1;
  iw = itg + ng + 1;
  nsp_ctrlpack_feq1 (&nq, t, &tq[1], &tq[itg], &ng, &tqdot[1], &tq[iw]);
  i__1 = nq;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      tqdot[i__] = -tqdot[i__];
      /* L10: */
    }
  return 0;
}				/* feqn_ */

int nsp_ctrlpack_feq1 (const int *nq,const double *t,double *tq, double *tg,const int *ng, double *tqdot, double *tr)
{
  int i__1;
  int  ltvq, i__;
  double y0;
  int nr, nv, ichoix;

  --tqdot;
  --tq;
  --tg;
  --tr;

  i__1 = *nq;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       *    -- calcul du terme general -- 
       * 
       */
      if (i__ == 1)
	{
	  nsp_ctrlpack_lq (nq, &tq[1], &tr[1], &tg[1], ng);
	  /*    .     tlq =tr(1:nq); tvq =tr(nq+1:nq+ng+1) 
	   */
	  ltvq = *nq + 1;
	  /* 
	   *    division de tvq par q 
	   */
	  nsp_ctrlpack_dpodiv (&tr[ltvq], &tq[1], ng, nq);
	  nv = *ng - *nq;
	}
      else
	{
	  ichoix = 1;
	  nsp_ctrlpack_mzdivq (&ichoix, &nv, &tr[ltvq], nq, &tq[1]);
	}
      /* 
       *    calcul de tvq~ sur place 
       */
      nr = *nq - 1;
      nsp_ctrlpack_tild (&nr, &tr[ltvq], &tr[1]);
      nsp_ctrlpack_calsca (nq, &tq[1], &tr[1], &y0, &tg[1], ng);
      /* 
       *    -- conclusion -- 
       * 
       */
      tqdot[i__] = y0 * -2.;
      /* 
       */
      /* L199: */
    }
  return 0;
}



/*
 *     Routine de recherche de minimum du probleme d'approximation L2 
 *      par lsoda ( Lsoda = routine de resolution d'equa diff ) 
 *
 *    subroutine optml2(feq,jacl2,neq,q,nch,w,iw) 
 * 
 *    external feq,jacl2 
 *    double precision q(*),w(*) 
 *    int nch,iw(*) 
 * 
 *    Entrees : 
 *    - feq est la subroutine qui calcule le gradient, 
 *       oppose de la derivee premiere de la fonction phi. 
 *    - neq. tableau entier de taille 3+(npara+1)*(npara+2) 
 *        neq(1)=nq est le degre effectif du polynome  q. 
 *        neq(2)=ng est le nombre de coefficient de fourier 
 *        neq(3)=dgmax degre maximum pour q (l'adresse des coeff de fourier dans 
 *              q est neq(3)+2 
 *    - neq est le degre du polynome q 
 *    - tq. tableau reel de taille au moins 
 *              6+dgmax+5*nq+6*ng+nq*ng+nq**2*(ng+1) 
 *        tq(1:nq+1) est le tableau des coefficients du polynome q. 
 *        tq(dgmax+2:dgmax+ng+2) est le tableau des coefficients 
 *                     de fourier 
 *        tq(dgmax+ng+3:) est un tableau de travail de taille au moins 
 *                        5+5*nq+5*ng+nq*ng+nq**2*(ng+1) 
 *    - nch est l indice (valant 1 ou 2) qui classifie l 
 *      appel comme etant soit celui de la recherche et de la 
 *      localisation d un minimum local, soit de la 
 *      confirmation d un minimum local. 
 * 
 *    Sorties : 
 *    - neq est toujours le degre du polynome q (il peut  avoir varie). 
 *    - q est le polynome (ou plutot le tableau contenant 
 *        ses coefficients) qui resulte de la recherche ,il peut 
 *        etre du meme degre que le polynome initial mais aussi 
 *        de degre inferieur dans le cas d'une sortie de face. 
 * 
 *    Tableau de travail 
 *    - w de taille 25+26*nq+ng+nq**2 
 *    - iw de taille 20+nq 
 */

int
nsp_ctrlpack_optml2 (ct_Feq feq, U_fp jacl2, int *neq, double *q, int *nch,
		     double *w, int *iw)
{
  int i__1, i__2;
  double d__1;

  int itol, iopt, ntol, liww;
  double tout;
  int i__, j, nqbac;
  double x;
  int ilcom;
  int latol, itask;
  int ipass, lqdot, nbout;
  int lrtol, lwork;
  double t0, dnorm0;
  int mf, ng;
  double ti;
  int nq, lw;
  double tt, xx[1];
  int nlsode, istate;
  double epstop;
  int job;
  int lqi, ltg, liw, lrw;
  double phi0;

  --iw;
  --w;
  --q;
  --neq;

  /* Function Body */
  nq = neq[1];
  ng = neq[2];
  ltg = neq[3] + 1;
  /* 
   *    taille des tableaux de travail necessaires a lsode 
   *Computing 2nd power 
   */
  i__1 = nq;
  lrw = i__1 * i__1 + nq * 9 + 22;
  liw = nq + 20;
  /*    decoupage du tableau de travail w 
   */
  lqi = 1;
  lqdot = lqi + nq + 1;
  latol = lqdot + nq;
  lrtol = latol + nq;
  lwork = lrtol + nq;
  /*Computing 2nd power 
   */
  i__1 = nq;
  /* lfree = lwork + 24 + nq * 22 + ng + i__1 * i__1; */
  /* 
   */
  lw = lwork + lrw;
  /*    decoupage du tableau de travail iw 
   */
  liww = 1;
  
  /* lifree = liww + liw; */
  /* 
   */
  nqbac = nq;
  /* 
   *    --- Initialisation de lsode ------------------------ 
   * 
   */
  if (*nch == 1)
    {
      temps = 0.;
    }
  t0 = temps;
  tt = .1;
  tout = t0 + tt;
  itol = 4;
  /* 
   */
  if (nq < 7)
    {
      ntol = (nq - 1) / 3 + 5;
    }
  else
    {
      ntol = (nq - 7) / 2 + 7;
    }
  i__1 = -ntol;
  d__1 = pow_di (c_b3, i__1);
  nsp_dset (&nq, &d__1, &w[lrtol], &c1);
  i__1 = -(ntol + 2);
  d__1 = pow_di (c_b3, i__1);
  nsp_dset (&nq, &d__1, &w[latol], &c1);
  /* 
   */
  itask = 1;
  if (*nch == 1)
    {
      istate = 1;
    }
  if (*nch == 2)
    {
      istate = 3;
    }
  iopt = 0;
  mf = 21;
  /* 
   *    --- Initialisation du nombre maximal d'iteration --- 
   * 
   */
  if (*nch == 1)
    {
      if (nq <= 11)
	{
	  nlsode = ((nq - 1) << 1) + 11;
	}
      else
	{
	  nlsode = 29;
	}
    }
  else
    {
      nlsode = 19;
    }
  ilcom = 0;
  ipass = 0;
  /* 
   *    --- Appel  de lsode -------------------------------- 
   * 
   */
 L210:
  i__1 = nlsode;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
    L220:
      ++ilcom;
      /* 
       *    -- Reinitialisation de la Tolerance -- 
       * 
       */
      if (ilcom == 2 && *nch == 1)
	{
	  nsp_dset (&nq, &c_b9, &w[lrtol], &c1);
	  nsp_dset (&nq, &c_b11, &w[latol], &c1);
	  istate = 3;
	}
      else if (ilcom == 2 && *nch == 2)
	{
	  w[lrtol] = 1e-8;
	  w[latol] = 1e-10;
	  w[lrtol + 1] = 1e-7;
	  w[latol + 1] = 1e-9;
	  w[lrtol + nq - 1] = 1e-5;
	  w[latol + nq - 1] = 1e-7;
	  i__2 = nq - 2;
	  for (j = 2; j <= i__2; ++j)
	    {
	      w[lrtol + j] = 1e-6;
	      w[latol + j] = 1e-8;
	      /* L240: */
	    }
	  istate = 3;
	}
      /* 
       *    -------------------------------------- 
       * 
       */
      i__2 = nq + 1;
      C2F (dcopy) (&i__2, &q[1], &c1, &w[lqi], &c1);
      ti = temps;
      /* touti = tout; */
      /* 
       */
      if (sortie_info > 1)
	{
	  static const int c__30 = 30;
	  nsp_ctrlpack_outl2 (&c__30, &nq, &nq, &q[1], xx, &temps, &tout);
	}
      /* 
       */
      C2F(lsode) ((S_fp) feq, &neq[1], &q[1], &temps, &tout,
		  &itol, &w[lrtol], &w[latol], &itask, &istate, &iopt,
		  &w[lwork], &lrw, &iw[liww], &liw, (U_fp) jacl2,
		  &mf,NULL);
      /* 
       */
      nsp_ctrlpack_front (&nq, &q[1], &nbout, &w[lw]);
      /* 
       */
      (*feq) (&neq[1], &temps, &q[1], &w[lqdot]);
      dnorm0 = C2F (dnrm2) (&nq, &w[lqdot], &c1);
      if (sortie_info > 1)
	{
	  static const int c__31 = 31;
	  nsp_ctrlpack_outl2 (&c__31, &nq, &nbout, &q[1], &dnorm0, &temps,
			      &tout);
	}
      /* 
       *    -- test pour degre1 ----------- 
       */
      if (arl2_comall > 0 && nq == 1 && nbout > 0)
	{
	  return 0;
	}
      /* 
       * 
       *    -- Istate de lsode ------------ 
       * 
       */
      if (istate == -5)
	{
	  if (sortie_info > 0)
	    {
	      static const int c__32 = 32;
	      nsp_ctrlpack_outl2 (&c__32, &nq, &nq, xx, xx, &x, &x);
	    }
	  C2F (dscal) (&nq, &c_b20, &w[lrtol], &c1);
	  C2F (dscal) (&nq, &c_b20, &w[latol], &c1);
	  if (temps == 0.)
	    {
	      istate = 1;
	    }
	  if (temps != 0.)
	    {
	      istate = 3;
	    }
	  ilcom = 0;
	  goto L220;
	}
      /* 
       */
      if (istate == -6)
	{
	  /*    echec de l'integration appel avec de nouvelles tolerances 
	   */
	  if (sortie_info > 0)
	    {
	      static const int c__33 = 33;
	      nsp_ctrlpack_outl2 (&c__33, &nq, &nq, xx, xx, &x, &x);
	    }
	  if (sortie_info > 1)
	    {
	      static const int c__34 = 34;
	      nsp_ctrlpack_outl2 (&c__34, &nq, &itol, &w[latol], &w[lrtol],
				  &temps, &tout);
	    }
	  iopt = 0;
	  itol = 4;
	  nsp_dset (&nq, &c_b26, &w[lrtol], &c1);
	  nsp_dset (&nq, &c_b26, &w[latol], &c1);
	  if (sortie_info > 1)
	    {
	      static const int c__35 = 35;
	      nsp_ctrlpack_outl2 (&c__35, &nq, &itol, &w[latol], &w[lrtol],
				  &x, &x);
	    }
	  if (sortie_info > 0)
	    {
	      static const int c__36 = 36;
	      nsp_ctrlpack_outl2 (&c__36, &nq, &nq, xx, xx, &x, &x);
	    }
	  istate = 3;
	  if (temps != tout)
	    {
	      goto L220;
	    }
	}
      /* 
       */
      if (istate < -1 && istate != -6 && istate != -5)
	{
	  if (sortie_info > 0)
	    {
	      static const int c__37 = 37;
 	      nsp_ctrlpack_outl2 (&c__37, &nq, &iopt, xx, xx, &x, &x);
	    }
	  *nch = 15;
	  return 0;
	}
      /* 
       *    ------------------------------- 
       * 
       *    -- Sortie de face ------------- 
       * 
       */
      if (nbout > 0 && nbout != 99)
	{
	  nsp_ctrlpack_domout (&neq[1], &q[1], &w[lqi], &nbout, &ti,
			       &temps, &itol, &w[lrtol], &w[latol],
			       &itask, &istate, &iopt, &w[lwork], &lrw,
			       &iw[liww], &liw, (U_fp) jacl2, &mf, &job);
	  nq = neq[1];
	  if (job == -1)
	    {
	      /*    anomalie dans la recherche de l'intersection 
	       */
	      *nch = 16;
	      return 0;
	    }
	  if (job == 1)
	    {
	      *nch = nq - nqbac;
	      return 0;
	    }
	}
      /* 
       *    ------------------------------- 
       * 
       *    -- test sur le gradient ------- 
       * 
       */
      epstop = pow_di (c_b26, *nch);
      (*feq) (&neq[1], &temps, &q[1], &w[lqdot]);
      dnorm0 = C2F (dnrm2) (&nq, &w[lqdot], &c1);
      if (dnorm0 < epstop)
	{
	  goto L299;
	}
      /* 
       *    ------------------------------- 
       * 
       *    -- Istate de lsode (suite) ---- 
       * 
       */
      if (istate == -1 && temps != tout)
	{
	  if (sortie_info > 0)
	    {
	      static const int c__38 = 38;
	      nsp_ctrlpack_outl2 (&c__38, &nq, &nq, xx, xx, &x, &x);
	    }
	  istate = 2;
	  goto L220;
	}
      /* 
       *    ------------------------------- 
       * 
       */
      tt = sqrt (10.) * tt;
      tout = t0 + tt;
      /* 
       */
      /* L290: */
    }
  /* 
   */
  if (*nch == 2 && dnorm0 > 1e-6)
    {
      ++ipass;
      if (ipass < 5)
	{
	  if (sortie_info > 0)
	    {
	      nsp_ctrlpack_lq (&nq, &q[1], &w[lw], &q[ltg], &ng);
	      x = sqrt (no2f_gnrm);
	      C2F (dscal) (&nq, &x, &w[lw], &c1);
	      nsp_ctrlpack_outl2 (&c14, &nq, &nq, &q[1], &w[lw], &x, &x);
	      phi0 = (d__1 =
		      nsp_ctrlpack_phi (&q[1], &nq, &q[ltg], &ng, &w[lw]),
		      Abs (d__1));
	      (*feq) (&neq[1], &temps, &q[1], &w[lqdot]);
	      nsp_ctrlpack_outl2 (&c17, &nq, &nq, &q[1], &w[lqdot], &phi0,
				  &x);
	    }
	  goto L210;
	}
      else
	{
	  if (sortie_info > 0)
	    {
	      static const int c__39 = 39;
	      nsp_ctrlpack_outl2 (&c__39, &nq, &nq, xx, xx, &x, &x);
	    }
	  *nch = 17;
	  return 0;
	}
    }
  /* 
   */
 L299:
  return 0;
}

/* Table of constant values */

/*
 *    Determiner la totalite des polynome de degre 1. 
 *
 *    sorties : 
 *    -imin. est le nombre de minimums obtenus. 
 *    -ta. est le tableau dans lequel sont conserves les 
 *       minimums. 
 *    tableaux de travail (dgmax=1) 
 *    - w :32+32*dgmax+7*ng+dgmax*ng+dgmax**2*(ng+2)+2*mxsol 
 *    -iw : 29+dgmax**2+4*dgmax+ mxsol 
 *remarque 
 *    on notera que le neq ieme coeff de chaque colonne 
 *    devant contenir le coeff du plus au degre qui est 
 *    toujours 1. contient en fait la valeur du critere 
 *    pour ce polynome. 
 */

int
nsp_ctrlpack_deg1l2 (double *tg, int *ng, int *imin, double *ta, int *mxsol,
		     double *w, int *iw, int *ierr)
{
  /* System generated locals */
  int ta_dim1, ta_offset, i__1;
  double d__1;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int lneq, lntb;
  int iback;
  double t;
  double x;
  int dgmax, icomp;
  int lqdot, lwopt;
  int neqbac;
  int ltback, lw;
  double xx[1];
  int minmax, liwopt, nch;
  int neq, ltg, ltq, lrw;
  double phi0;

  /* Parameter adjustments */
  --tg;
  ta_dim1 = *mxsol;
  ta_offset = ta_dim1 + 1;
  ta -= ta_offset;
  --w;
  --iw;

  /* Function Body */
  dgmax = 1;
  ltq = 1;
  /*Computing 2nd power 
   */
  i__1 = dgmax;
  lwopt =
    ltq + 6 + dgmax * 6 + *ng * 6 + dgmax * *ng + i__1 * i__1 * (*ng + 1);
  /*Computing 2nd power 
   */
  i__1 = dgmax;
  ltback = lwopt + 25 + dgmax * 26 + *ng + i__1 * i__1;
  /* 
   *    les lrw elements de w suivant w(lwopt) ne doivent pas etre modifies 
   *    d'un appel de optml2 a l'autre 
   *Computing 2nd power 
   */
  i__1 = dgmax;
  lrw = i__1 * i__1 + dgmax * 9 + 22;
  lw = lwopt + lrw;
  /* 
   */
  lneq = 1;
  liwopt = lneq + 3 + (dgmax + 1) * (dgmax + 2);
  lntb = liwopt + 20 + dgmax;
  /* 
   */
  minmax = -1;
  neq = 1;
  neqbac = 1;
  iback = 0;
  /* 
   */
  iw[lneq] = neq;
  iw[lneq + 1] = *ng;
  iw[lneq + 2] = dgmax;
  /* 
   */
  w[ltq] = .9999;
  w[ltq + 1] = 1.;
  ltg = ltq + 2;
  i__1 = *ng + 1;
  C2F (dcopy) (&i__1, &tg[1], &c1, &w[ltg], &c1);
  /* 
   */
  if (sortie_info > 0)
    {
      static const int c__51 = 51;
      nsp_ctrlpack_outl2 (&c__51, &neq, &neq, xx, xx, &x, &x);
    }
  for (icomp = 1; icomp <= 50; ++icomp)
    {
      if (minmax == -1)
	{
	  nch = 1;
	  nsp_ctrlpack_optml2 (nsp_ctrlpack_feq, nsp_ctrlpack_jacl2,
			       &iw[lneq], &w[ltq], &nch, &w[lwopt],
			       &iw[liwopt]);
	  if (sortie_info > 1)
	    {
	      nsp_ctrlpack_lq (&neq, &w[ltq], &w[lw], &w[ltg], ng);
	      x = sqrt (no2f_gnrm);
	      C2F (dscal) (&neq, &x, &w[lw], &c1);
	      nsp_ctrlpack_outl2 (&nch, &neq, &neqbac, &w[ltq], &w[lw], &x,
				  &x);
	      phi0 = (d__1 =
		      nsp_ctrlpack_phi (&w[ltq], &neq, &w[ltg], ng, &w[lw]),
		      Abs (d__1));
	      lqdot = lw;
	      nsp_ctrlpack_feq (&iw[lneq], &t, &w[ltq], &w[lqdot]);
	      nsp_ctrlpack_outl2 (&c17, &neq, &neq, &w[ltq], &w[lqdot],
				  &phi0, &x);
	    }
	  nch = 2;
	  nsp_ctrlpack_optml2 (nsp_ctrlpack_feq, nsp_ctrlpack_jacl2,
			       &iw[lneq], &w[ltq], &nch, &w[lwopt],
			       &iw[liwopt]);
	  if (sortie_info > 0)
	    {
	      nsp_ctrlpack_lq (&neq, &w[ltq], &w[lw], &w[ltg], ng);
	      x = sqrt (no2f_gnrm);
	      C2F (dscal) (&neq, &x, &w[lw], &c1);
	      nsp_ctrlpack_outl2 (&nch, &neq, &neqbac, &w[ltq], &w[lw], &x,
				  &x);
	      phi0 = (d__1 =
		      nsp_ctrlpack_phi (&w[ltq], &neq, &w[ltg], ng, &w[lw]),
		      Abs (d__1));
	      lqdot = lw;
	      nsp_ctrlpack_feq (&iw[lneq], &t, &w[ltq], &w[lqdot]);
	      nsp_ctrlpack_outl2 (&c17, &neq, &neq, &w[ltq], &w[lqdot],
				  &phi0, &x);
	    }
	  minmax = 1;
	}
      else
	{
	  nch = 1;
	  nsp_ctrlpack_optml2 (nsp_ctrlpack_feqn, nsp_ctrlpack_jacl2n,
			       &iw[lneq], &w[ltq], &nch, &w[lwopt],
			       &iw[liwopt]);
	  if (sortie_info > 1)
	    {
	      nsp_ctrlpack_lq (&neq, &w[ltq], &w[lw], &w[ltg], ng);
	      x = sqrt (no2f_gnrm);
	      C2F (dscal) (&neq, &x, &w[lw], &c1);
	      nsp_ctrlpack_outl2 (&nch, &neq, &neqbac, &w[ltq], &w[lw], &x,
				  &x);
	      phi0 = (d__1 =
		      nsp_ctrlpack_phi (&w[ltq], &neq, &w[ltg], ng, &w[lw]),
		      Abs (d__1));
	      lqdot = lw;
	      nsp_ctrlpack_feqn (&iw[lneq], &t, &w[ltq], &w[lqdot]);
	      nsp_ctrlpack_outl2 (&c17, &neq, &neq, &w[ltq], &w[lqdot],
				  &phi0, &x);
	    }
	  nch = 2;
	  nsp_ctrlpack_optml2 (nsp_ctrlpack_feqn, nsp_ctrlpack_jacl2n,
			       &iw[lneq], &w[ltq], &nch, &w[lwopt],
			       &iw[liwopt]);
	  if (sortie_info > 0)
	    {
	      nsp_ctrlpack_lq (&neq, &w[ltq], &w[lw], &w[ltg], ng);
	      x = sqrt (no2f_gnrm);
	      C2F (dscal) (&neq, &x, &w[lw], &c1);
	      nsp_ctrlpack_outl2 (&nch, &neq, &neqbac, &w[ltq], &w[lw], &x,
				  &x);
	      phi0 = (d__1 =
		      nsp_ctrlpack_phi (&w[ltq], &neq, &w[ltg], ng, &w[lw]),
		      Abs (d__1));
	      lqdot = lw;
	      nsp_ctrlpack_feqn (&iw[lneq], &t, &w[ltq], &w[lqdot]);
	      nsp_ctrlpack_outl2 (&c17, &neq, &neq, &w[ltq], &w[lqdot],
				  &phi0, &x);
	    }
	  minmax = -1;
	}
      if ((d__1 = w[ltq], Abs (d__1)) > 1.)
	{
	  goto L140;
	}
      if (minmax == 1)
	{
	  if (icomp == 1)
	    {
	      *imin = 1;
	      ta[*imin + ta_dim1] = w[ltq];
	      ta[*imin + (ta_dim1 << 1)] =
		nsp_ctrlpack_phi (&w[ltq], &neq, &tg[1], ng, &w[lwopt]);
	    }
	  else
	    {
	      nsp_ctrlpack_storl2 (&neq, &w[ltq], &w[ltg], ng, imin,
				   &ta[ta_offset], &iback, &iw[lntb],
				   &w[ltback], &nch, mxsol, &w[lwopt], ierr);
	      if (*ierr > 0)
		{
		  return 0;
		}
	    }
	}
      w[ltq] += -1e-5;
      /* L120: */
    }
  /* 
   */
 L140:
  if (sortie_info > 0)
    {
      static const int c__52 = 52;
      x = (double) (*mxsol);
      nsp_ctrlpack_outl2 (&c__52, &neq, imin, &ta[ta_offset], xx, &x, &x);
    }
  /* 
   */
  return 0;
}				/* deg1l2_ */

/* Table of constant values */


/*!but 
 *    Cette procedure a pour objectif de determiner le plus grand 
 *    nombre de minimums de degre "neq". 
 *!liste d'appel 
 *    subroutine degre (neq,imina,iminb,iminc,ta,tb,tc, 
 *   &     iback,ntback,tback) 
 * 
 *    Entree : 
 *    -neq. est le degre des polynomes parmi lesquels ont 
 *      recherche les minimums. 
 *    -imina. est le nombre de minimums de degre "neq-1" 
 *      contenus dans ta. 
 *    -iminb. est le nombre de minimums de degre "neq-2" 
 *      contenus dans tb. 
 *    -iminc. est le nombre de minimums de degre "neq-3" 
 *      contenus dans tc. 
 *    -ta. est le tableau contenant donc les minimums de degre 
 *      "neq-1" 
 *    -tb. est le tableau contenant donc les minimums de degre 
 *      "neq-2" 
 *    -tc. est le tableau contenant donc les minimums de degre 
 *      "neq-3" 
 *    -iback. est le nombre de minimums obtenus apres une 
 *      intersection avec la frontiere 
 *    -ntback est un tableau d'entier qui contient les degre 
 *      de ces minimums 
 *    -tback. est le tableau qui contient leurs coefficients, 
 *      ou ils sont ordonnes degre par degre. 
 * 
 *    Sortie : 
 *    -imina. est le nombre de minimums de degre neq que l'on 
 *      vient de determiner 
 *    -iminb. est le nombre de minimums de degre "neq-1" 
 *    -iminc. est le nombre de minimums de degre "neq-2" 
 *    -ta. contient les mins de degre neq, -tb. ceux de degre 
 *      neq-1 et tc ceux de degre neq-2 
 *    -iback,ntback,tback ont pu etre augmente des mins obtenus 
 *      apres intersection eventuelle avec la frontiere. 
 * 
 *    tableaux de travail 
 *     w : 33+33*neq+7*ng+neq*ng+neq**2*(ng+2) 
 *     iw :29+neq**2+4*neq 
 * 
 */

int
nsp_ctrlpack_degl2 (double *tg, int *ng, int *neq, int *imina, int *iminb,
		    int *iminc, double *ta, double *tb, double *tc,
		    int *iback, int *ntback, double *tback, int *mxsol,
		    double *w, int *iw, int *ierr)
{
  /* System generated locals */
  int ta_dim1, ta_offset, tb_dim1, tb_offset, tc_dim1, tc_offset, tback_dim1,
    tback_offset, i__1, i__2;
  double d__1;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int lneq;
  int j, k;
  double t;
  double x;
  int lqdot, imult, lwopt;
  int neqbac;
  int lw;
  double xx[1];
  int liwopt, nch;
  int ltg, ltq, ltr;
  double tms[2];
  int lrw;
  double tps[2], phi0;

  /* Parameter adjustments */
  --tg;
  tback_dim1 = *mxsol;
  tback_offset = tback_dim1 + 1;
  tback -= tback_offset;
  --ntback;
  tc_dim1 = *mxsol;
  tc_offset = tc_dim1 + 1;
  tc -= tc_offset;
  tb_dim1 = *mxsol;
  tb_offset = tb_dim1 + 1;
  tb -= tb_offset;
  ta_dim1 = *mxsol;
  ta_offset = ta_dim1 + 1;
  ta -= ta_offset;
  --w;
  --iw;

  /* Function Body */
  tps[0] = 1.;
  tps[1] = 1.;
  tms[0] = -1.;
  tms[1] = 1.;
  /* 
   * 
   *    -------- Reinitialisation des tableaux -------- 
   * 
   */
  if (*neq == 1)
    {
      goto L111;
    }
  /* 
   */
  i__1 = *iminb;
  for (j = 1; j <= i__1; ++j)
    {
      C2F (dcopy) (neq, &tb[j + tb_dim1], mxsol, &tc[j + tc_dim1], mxsol);
      /* L110: */
    }
  *iminc = *iminb;
  /* 
   */
 L111:
  i__1 = *imina;
  for (j = 1; j <= i__1; ++j)
    {
      C2F (dcopy) (neq, &ta[j + ta_dim1], mxsol, &tb[j + tb_dim1], mxsol);
      /* L120: */
    }
  *iminb = *imina;
  *imina = 0;
  ++(*neq);
  neqbac = *neq;
  /* 
   *Computing 2nd power 
   */
  i__1 = *neq;
  lrw = i__1 * i__1 + *neq * 9 + 22;
  /*    decoupage du tableau de travail w 
   */
  ltq = 1;
  /*Computing 2nd power 
   */
  i__1 = *neq;
  lwopt = ltq + 6 + *neq * 6 + *ng * 6 + *neq * *ng + i__1 * i__1 * (*ng + 1);
  /*Computing 2nd power 
   */
  i__1 = *neq;
  ltr = lwopt + 25 + *neq * 26 + *ng + i__1 * i__1;
  /* 
   *    les lrw elements de w suivant w(lwopt) ne doivent pas etre modifies 
   *    d'un appel de optml2 a l'autre 
   */
  lw = lwopt + lrw;
  ltg = ltq + *neq + 1;
  i__1 = *ng + 1;
  C2F (dcopy) (&i__1, &tg[1], &c1, &w[ltg], &c1);
  /*    decoupage du tableau de travail iw 
   */
  lneq = 1;
  liwopt = lneq + 3 + (*neq + 1) * (*neq + 2);
  /* 
   */
  iw[lneq] = *neq;
  iw[lneq + 1] = *ng;
  iw[lneq + 2] = *neq;
  if (sortie_info > 0)
    {
      static const int c__51 = 51;
      nsp_ctrlpack_outl2 (&c__51, neq, neq, xx, xx, &x, &x);
    }
  /* 
   *    -------- Boucles de calculs -------- 
   * 
   */
  i__1 = *iminb;
  for (k = 1; k <= i__1; ++k)
    {
      /* 
       */
      i__2 = *neq - 1;
      C2F (dcopy) (&i__2, &tb[k + tb_dim1], mxsol, &w[ltr], &c1);
      w[ltr + *neq - 1] = 1.;
      /* 
       */
      for (imult = 1; imult <= 2; ++imult)
	{
	  /* 
	   */
	  if (imult == 1)
	    {
	      i__2 = *neq - 1;
	      nsp_ctrlpack_dpmul1 (&w[ltr], &i__2, tps, &c1, &w[ltq]);
	    }
	  else if (imult == 2)
	    {
	      i__2 = *neq - 1;
	      nsp_ctrlpack_dpmul1 (&w[ltr], &i__2, tms, &c1, &w[ltq]);
	    }
	  /* 
	   */
	L140:
	  /* 
	   */
	  nch = 1;
	  nsp_ctrlpack_optml2 (nsp_ctrlpack_feq, nsp_ctrlpack_jacl2,
			       &iw[lneq], &w[ltq], &nch, &w[lwopt],
			       &iw[liwopt]);
	  *neq = iw[lneq];
	  if (sortie_info > 1)
	    {
	      nsp_ctrlpack_outl2 (&nch, &iw[lneq], &neqbac, &w[ltq], xx, &x,
				  &x);
	    }
	  if (sortie_info > 0)
	    {
	      nsp_ctrlpack_lq (neq, &w[ltq], &w[lw], &w[ltg], ng);
	      x = sqrt (no2f_gnrm);
	      C2F (dscal) (neq, &x, &w[lw], &c1);
	      nsp_ctrlpack_outl2 (&nch, neq, &neqbac, &w[ltq], &w[lw], &x,
				  &x);
	      phi0 = (d__1 =
		      nsp_ctrlpack_phi (&w[ltq], neq, &w[ltg], ng, &w[lw]),
		      Abs (d__1));
	      lqdot = lw;
	      nsp_ctrlpack_feq (&iw[lneq], &t, &w[ltq], &w[lqdot]);
	      nsp_ctrlpack_outl2 (&c17, neq, neq, &w[ltq], &w[lqdot], &phi0,
				  &x);
	    }
	  if (nch == 15 && arl2_comall == 0)
	    {
	      *ierr = 4;
	      return 0;
	    }
	  /* 
	   */
	  if (nch == -1)
	    {
	      goto L140;
	    }
	  if (nch == -2)
	    {
	      goto L140;
	    }
	  /* 
	   */
	  nch = 2;
	  nsp_ctrlpack_optml2 (nsp_ctrlpack_feq, nsp_ctrlpack_jacl2,
			       &iw[lneq], &w[ltq], &nch, &w[lwopt],
			       &iw[liwopt]);
	  *neq = iw[lneq];
	  if (sortie_info > 1)
	    {
	      nsp_ctrlpack_lq (neq, &w[ltq], &w[lw], &w[ltg], ng);
	      x = sqrt (no2f_gnrm);
	      C2F (dscal) (neq, &x, &w[lw], &c1);
	      nsp_ctrlpack_outl2 (&nch, neq, &neqbac, &w[ltq], &w[lw], &x,
				  &x);
	      phi0 = (d__1 =
		      nsp_ctrlpack_phi (&w[ltq], neq, &w[ltg], ng, &w[lw]),
		      Abs (d__1));
	      lqdot = lw;
	      nsp_ctrlpack_feq (&iw[lneq], &t, &w[ltq], &w[lqdot]);
	      nsp_ctrlpack_outl2 (&c17, neq, neq, &w[ltq], &w[lqdot], &phi0,
				  &x);
	    }
	  if (nch == 15 && arl2_comall == 0)
	    {
	      *ierr = 4;
	      return 0;
	    }
	  /* 
	   * 
	   */
	  if (nch == -1)
	    {
	      goto L140;
	    }
	  if (nch == -2)
	    {
	      goto L140;
	    }
	  /* 
	   */
	  if (nch == 15)
	    {
	      if (sortie_info > 0)
		{
		  static const int c__50 = 50;
		  nsp_ctrlpack_outl2 (&c__50, neq, neq, xx, xx, &x, &x);
		}
	      goto L170;
	    }
	  /* 
	   */
	  nch = *neq - neqbac;
	  if (nch == -2)
	    {
	      nsp_ctrlpack_storl2 (neq, &w[ltq], &w[ltg], ng, iminc,
				   &tc[tc_offset], iback, &ntback[1],
				   &tback[tback_offset], &nch, mxsol,
				   &w[lwopt], ierr);
	    }
	  else if (nch == -1)
	    {
	      nsp_ctrlpack_storl2 (neq, &w[ltq], &w[ltg], ng, iminb,
				   &tb[tb_offset], iback, &ntback[1],
				   &tback[tback_offset], &nch, mxsol,
				   &w[lwopt], ierr);
	    }
	  else
	    {
	      nsp_ctrlpack_storl2 (neq, &w[ltq], &w[ltg], ng, imina,
				   &ta[ta_offset], iback, &ntback[1],
				   &tback[tback_offset], &nch, mxsol,
				   &w[lwopt], ierr);
	    }
	  /* 
	   */
	L170:
	  *neq = neqbac;
	  iw[lneq] = *neq;
	  /* 
	   */
	  /* L180: */
	}
      /* L190: */
    }
  if (sortie_info > 0)
    {
      static const int c__53 = 53;
      x = (double) (*mxsol);
      nsp_ctrlpack_outl2 (&c__53, neq, imina, &ta[ta_offset], xx, &x, &x);
    }
  return 0;
}				/* degl2_ */

/* Table of constant values */


/*
 *    Lorsque un minimum local vient d'etre determine, cette 
 *    procedure est appelee afin de verifier son originalite, 
 *    et si elle est effective, de le stocker dans le tableau 
 *    en construction, correspondant au degre de la recherche 
 *    en cours. S'il n'est pas de ce degre, il est alors range 
 *    dans le tableau 'tback' qui contient tout minimum origi- 
 *    nal obtenu apres une sortie de face. 
 *
 *    entrees : 
 *    - neq. est le degre du minimum nouvellement obtenu. 
 *    - tq. est le tableau contenant ses coefficients 
 *    - imin. est le nombre des minimums de meme degre, 
 *       deja reveles. 
 *    - tabc. etant le tableau contenant ces minimums. 
 *    - iback. est le nombre de minimums de degre 
 *       quelconque, reveles apres une sortie de face. 
 *    - ntback. est un tableau entier unicolonne contenant 
 *       les degres de ces polynomes. 
 *    - tback. est le tableau ou sont stockes ces polynomes. 
 *       Ainsi, le ieme polynome, de degre ntback(i), a 
 *       ses coeff dans la ieme ligne, c-a-d de tback(i,0) 
 *       a tback(i,ntback(i)-1). 
 *    - nch. est un parametre entier indiquant s'il s'agit 
 *       d'un minimum de meme degre que celui de la recherche 
 *       en cours, ou bien d'une sortie de face. 
 * 
 *    sorties : 
 *    - peuvent etre modifies: imin, tabc, iback, ntback, 
 *       tback, suivant le tableau ou a ete stocke le minimum tq 
 */

int
nsp_ctrlpack_storl2 (int *neq, double *tq, double *tg, int *ng, int *imin,
		     double *tabc, int *iback, int *ntback, double *tback,
		     int *nch, int *mxsol, double *w, int *ierr)
{
  int tabc_dim1, tback_dim1, i__1, i__2;
  double d__1;
  int jinf;
  double paux;
  int jsup =0;
  double diff0;
  int j, i__;
  double x;
  int ij, im, in;
  double xx[1];

  /* Parameter adjustments */
  --tg;
  --ntback;
  tback_dim1 = *mxsol;
  --tback;
  tabc_dim1 = *mxsol;
  --tabc;
  --w;

  /* Function Body */
  *ierr = 0;
  if (*nch < -2)
    {
      goto L200;
    }
  if (*imin == 0)
    {
      goto L400;
    }
  /* 
   *    ---- test sur l'originalite du nouveau min ----------------------- 
   * 
   *    ---- par rapport a tabc. 
   * 
   */
  i__1 = *imin;
  for (im = 1; im <= i__1; ++im)
    {
      /* 
       */
      diff0 = 0.;
      i__2 = *neq - 1;
      for (ij = 0; ij <= i__2; ++ij)
	{
	  /*Computing 2nd power 
	   */
	  d__1 = tq[ij] - tabc[im + ij * tabc_dim1];
	  diff0 += d__1 * d__1;
	  /* L110: */
	}
      diff0 = sqrt (diff0);
      /* 
       */
      if (diff0 < .001)
	{
	  if (sortie_info > 0)
	    {
	      static const int c__80 = 80;
	      nsp_ctrlpack_outl2 (&c__80, &c0, &c0, xx, xx, &x, &x);
	    }
	  return 0;
	}
      /* 
       */
      /* L120: */
    }
  /* 
   *    ---- par rapport a tback. 
   * 
   *    - Situation des polynomes de meme degre. - 
   * 
   */
 L200:
  if (*nch < 0 && *iback > 0)
    {
      jsup = *iback + 1;
      jinf = 0;
      /* 
       */
      for (j = *iback; j >= 1; --j)
	{
	  if (jsup > j && ntback[j] > *neq)
	    {
	      jsup = j;
	    }
	  /* L210: */
	}
      i__1 = *iback;
      for (j = 1; j <= i__1; ++j)
	{
	  if (jinf < j && ntback[j] < *neq)
	    {
	      jinf = j;
	    }
	  /* L220: */
	}
      /* 
       *    - Controle de l'originalite. - 
       * 
       */
      if (jsup - jinf > 1)
	{
	  /* 
	   */
	  i__1 = jsup - 1;
	  for (j = jinf + 1; j <= i__1; ++j)
	    {
	      /* 
	       */
	      diff0 = 0.;
	      i__2 = *neq - 1;
	      for (i__ = 0; i__ <= i__2; ++i__)
		{
		  /*Computing 2nd power 
		   */
		  d__1 = tq[i__] - tback[j + i__ * tback_dim1];
		  diff0 += d__1 * d__1;
		  /* L230: */
		}
	      diff0 = sqrt (diff0);
	      /* 
	       */
	      if (diff0 < .001)
		{
		  if (sortie_info > 0)
		    {
		      static const int c__80 = 80;
		      nsp_ctrlpack_outl2 (&c__80, &c0, &c0, xx, xx, &x, &x);
		    }
		  return 0;
		}
	      /* 
	       */
	      /* L240: */
	    }
	}
    }
  /* 
   *    -------- classement du nouveau minimum ----- 
   *    ---- dans tback. 
   * 
   */
  if (*iback == *mxsol)
    {
      *ierr = 7;
      return 0;
    }
  if (*nch < 0)
    {
      /* 
       */
      if (*iback == 0)
	{
	  /* 
	   */
	  i__1 = *neq - 1;
	  for (i__ = 0; i__ <= i__1; ++i__)
	    {
	      tback[i__ * tback_dim1 + 1] = tq[i__];
	      /* L310: */
	    }
	  ntback[1] = *neq;
	  /* 
	   */
	}
      else if (jsup > *iback)
	{
	  /* 
	   */
	  i__1 = *neq - 1;
	  for (i__ = 0; i__ <= i__1; ++i__)
	    {
	      tback[jsup + i__ * tback_dim1] = tq[i__];
	      /* L330: */
	    }
	  ntback[*iback + 1] = *neq;
	  /* 
	   */
	}
      else
	{
	  /* 
	   */
	  i__1 = jsup;
	  for (j = *iback; j >= i__1; --j)
	    {
	      i__2 = ntback[j] - 1;
	      for (i__ = 0; i__ <= i__2; ++i__)
		{
		  tback[j + 1 + i__ * tback_dim1] =
		    tback[j + i__ * tback_dim1];
		  /* L340: */
		}
	      ntback[j + 1] = ntback[j];
	      /* L350: */
	    }
	  /* 
	   */
	  i__1 = *neq - 1;
	  for (i__ = 0; i__ <= i__1; ++i__)
	    {
	      tback[jsup + i__ * tback_dim1] = tq[i__];
	      /* L370: */
	    }
	  ntback[jsup] = *neq;
	  /* 
	   */
	}
      /* 
       */
      ++(*iback);
      if (sortie_info > 1)
	{
	  static const int c__81 = 81;
	  nsp_ctrlpack_outl2 (&c__81, neq, neq, xx, xx, &x, &x);
	}
      return 0;
      /* 
       */
    }
  /* 
   *    -------- dans tabc. 
   */
 L400:
  if (*imin == *mxsol)
    {
      *ierr = 7;
      return 0;
    }
  paux = nsp_ctrlpack_phi (tq, neq, &tg[1], ng, &w[1]);
  /* 
   */
  if (*imin == 0)
    {
      /* 
       */
      i__1 = *neq - 1;
      for (ij = 0; ij <= i__1; ++ij)
	{
	  tabc[ij * tabc_dim1 + 1] = tq[ij];
	  /* L410: */
	}
      tabc[*neq * tabc_dim1 + 1] = paux;
      ++(*imin);
      /* 
       */
    }
  else
    {
      /* 
       */
      for (im = *imin; im >= 1; --im)
	{
	  /* 
	   */
	  if (paux > tabc[im + *neq * tabc_dim1] && im == *imin)
	    {
	      /* 
	       */
	      i__1 = *neq - 1;
	      for (ij = 0; ij <= i__1; ++ij)
		{
		  tabc[*imin + 1 + ij * tabc_dim1] = tq[ij];
		  /* L420: */
		}
	      tabc[*imin + 1 + *neq * tabc_dim1] = paux;
	      ++(*imin);
	      return 0;
	      /* 
	       */
	    }
	  else if (paux > tabc[im + *neq * tabc_dim1])
	    {
	      /* 
	       */
	      i__1 = im + 1;
	      for (in = *imin; in >= i__1; --in)
		{
		  i__2 = *neq;
		  for (ij = 0; ij <= i__2; ++ij)
		    {
		      tabc[in + 1 + ij * tabc_dim1] =
			tabc[in + ij * tabc_dim1];
		      /* L430: */
		    }
		  /* L440: */
		}
	      i__1 = *neq - 1;
	      for (ij = 0; ij <= i__1; ++ij)
		{
		  tabc[im + 1 + ij * tabc_dim1] = tq[ij];
		  /* L450: */
		}
	      tabc[im + 1 + *neq * tabc_dim1] = paux;
	      ++(*imin);
	      return 0;
	      /* 
	       */
	    }
	  else if (im == 1)
	    {
	      /* 
	       */
	      for (in = *imin; in >= 1; --in)
		{
		  i__1 = *neq;
		  for (ij = 0; ij <= i__1; ++ij)
		    {
		      tabc[in + 1 + ij * tabc_dim1] =
			tabc[in + ij * tabc_dim1];
		      /* L460: */
		    }
		  /* L470: */
		}
	      i__1 = *neq - 1;
	      for (ij = 0; ij <= i__1; ++ij)
		{
		  tabc[ij * tabc_dim1 + 1] = tq[ij];
		  /* L480: */
		}
	      tabc[*neq * tabc_dim1 + 1] = paux;
	      ++(*imin);
	      /* 
	       */
	    }
	  /* 
	   */
	  /* L490: */
	}
      /* 
       */
    }
  /* 
   */
  return 0;
}

/*
 *    Calcule le produit scalaire entre une fonction de Hardi donnee 
 *    par ses coefficients de fourrier et une fonction rationnelle r/s 
 *
 *    subroutine calsca(ns,ts,tr,y0) 
 *    Entrees : 
 *    ng. est le plus grand indice (compte negativement) des 
 *        coefficients de fourrier de la fonction de Hardi u 
 *    tg. vecteur des coefficients de fourrier 
 *    ns. est le degre du denominateur (polynome monique) 
 *    ts. est le tableau des coefficients du denominateur 
 *    tr. est le tableau des coefficients du numerateur dont 
 *        le degre est inferieur a ns 
 * 
 *    sortie  : y0. contient la valeur du produit scalaire recherche. 
 */

int nsp_ctrlpack_calsca (const int *ns,const double *ts, const double *tr, double *y0, const double *tg, const int *ng)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, k;
  double x[41];
  int nu;
  double aux;

  nu = *ng + 1;
  i__1 = *ns - 1;
  for (i__ = 0; i__ <= i__1; ++i__)
    {
      x[i__] = 0.;
      /* L20: */
    }
  aux = x[*ns - 1];
  for (k = nu; k >= 1; --k)
    {
      for (i__ = *ns - 1; i__ >= 1; --i__)
	{
	  x[i__] = x[i__ - 1] - ts[i__] * aux + tr[i__] * tg[k - 1];
	  /* L29: */
	}
      x[0] = -ts[0] * aux + tr[0] * tg[k - 1];
      aux = x[*ns - 1];
      /* L30: */
    }
  *y0 = x[*ns - 1];
  return 0;
}				/* calsca_ */


/*!but 
 *    Elle etablit la valeur de la Hessienne, derivee 
 *      seconde de la fonction phi au point q . 
 *!liste d'appel 
 *    subroutine hessl2(neq,tq,pd,nrowpd) 
 *    Entree : 
 *    - neq. tableau entier de taille 3+(nq+1)*(nq+2) 
 *        neq(1)=nq est le degre effectif du polynome tq (ou q). 
 *        neq(2)=ng est le nombre de coefficient de fourier 
 *        neq(3)=dgmax degre maximum pour q (l'adresse des coeff de fourier dans 
 *              tq est neq(3)+2 
 *        neq(4:(nq+1)*(nq+2)) tableau de travail entier 
 *    - tq. tableau reel de taille au moins 
 *              6+dgmax+5*nq+6*ng+nq*ng+nq**2*(ng+1) 
 *        tq(1:nq+1) est le tableau des coefficients du polynome. 
 *        tq(dgmax+2:dgmax+ng+2) est le tableau des coefficients 
 *                     de fourier 
 *        tq(dgmax+ng+3:) est un tableau de travail de taille au moins 
 *                        5+5*nq+5*ng+nq*ng+nq**2*(ng+1) 
 *    Sortie : 
 *    - pd matrice hessienne 
 */

int nsp_ctrlpack_hessl2 (int *neq, double *tq, double *pd, int *nrowpd)
{
  /* System generated locals */
  int pd_dim1, pd_offset;

  /* Local variables */
  int itij, jmxnv, jmxnw, id1aux, id2aux, ng, nq; /* , iw, jw; */
  int itg, itp, itq, itr, itv, itw;

  /* Parameter adjustments */
  --neq;
  --tq;
  pd_dim1 = *nrowpd;
  pd_offset = pd_dim1 + 1;
  pd -= pd_offset;

  /* Function Body */
  nq = neq[1];
  ng = neq[2];
  /* 
   *    decoupage du tableau neq 
   */
  jmxnv = 4;
  jmxnw = jmxnv + (nq + 1);
  /*Computing 2nd power 
   */
  /* i__1 = nq + 1; */
  /* jw = jmxnw + i__1 * i__1;*/
  /* 
   *    decoupage du tableau tq 
   */
  itq = 1;
  itg = itq + neq[3] + 1;
  itr = itg + ng + 1;
  itp = itr + nq + ng + 1;
  itv = itp + nq + ng + 1;
  itw = itv + nq + ng + 1;
  itij = itw + nq + ng + 1;
  id1aux = itij + ng + 1;
  id2aux = id1aux + (ng + 1) * nq;
  /* iw = id2aux + nq * nq * (ng + 1); */
  nsp_ctrlpack_hl2 (&nq, &tq[1], &tq[itg], &ng, &pd[pd_offset], nrowpd,
		    &tq[itr], &tq[itp], &tq[itv], &tq[itw], &tq[itij],
		    &tq[id1aux], &tq[id2aux], &neq[jmxnv], &neq[jmxnw]);
  return 0;
}				/* hessl2_ */

int
nsp_ctrlpack_hl2 (int *nq, double *tq, double *tg, int *ng, double *pd,
		  int *nrowpd, double *tr, double *tp, double *tv, double *tw,
		  double *tij, double *d1aux, double *d2aux, int *maxnv,
		  int *maxnw)
{
  /* System generated locals */
  int pd_dim1, pd_offset, d1aux_dim1, d1aux_offset, d2aux_dim1, d2aux_dim2,
    d2aux_offset, maxnw_dim1, maxnw_offset, i__1, i__2, i__3;

  /* Local variables */
  int ltvq, i__, j, k, minij, maxij;
  double y1, y2;
  int ichoi1, ichoi2;
  int nw;
  int ichoix;
  int nv1, nv2;

  /*! 
   * 
   * 
   *    --- Calcul des derivees premieres de 'vq' --- 
   * 
   */
  /* Parameter adjustments */
  maxnw_dim1 = *nq;
  maxnw_offset = maxnw_dim1 + 1;
  maxnw -= maxnw_offset;
  --maxnv;
  --tq;
  d2aux_dim1 = *nq;
  d2aux_dim2 = *nq;
  d2aux_offset = d2aux_dim1 * (d2aux_dim2 + 1) + 1;
  d2aux -= d2aux_offset;
  d1aux_dim1 = *ng + 1;
  d1aux_offset = d1aux_dim1 + 1;
  d1aux -= d1aux_offset;
  --tij;
  --tw;
  --tv;
  --tp;
  --tr;
  --tg;
  pd_dim1 = *nrowpd;
  pd_offset = pd_dim1 + 1;
  pd -= pd_offset;

  /* Function Body */
  i__1 = *nq;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (i__ == 1)
	{
	  /*    .     division euclidienne de z^nq*g par q 
	   */
	  nsp_dset (nq, &c_b2, &tp[1], &c1);
	  i__2 = *ng + 1;
	  C2F (dcopy) (&i__2, &tg[1], &c1, &tp[*nq + 1], &c1);
	  i__2 = *nq + *ng;
	  nsp_ctrlpack_dpodiv (&tp[1], &tq[1], &i__2, nq);
	  nv1 = *ng;
	  /*    .     calcul de Lq et Vq 
	   */
	  nsp_ctrlpack_lq (nq, &tq[1], &tr[1], &tg[1], ng);
	  ltvq = *nq + 1;
	  /*    .     division euclidienne de Vq par q 
	   */
	  i__2 = *ng + 1;
	  C2F (dcopy) (&i__2, &tr[ltvq], &c1, &tv[1], &c1);
	  nsp_dset (nq, &c_b2, &tv[*ng + 2], &c1);
	  nsp_ctrlpack_dpodiv (&tv[1], &tq[1], ng, nq);
	  nv2 = *ng - *nq;
	}
      else
	{
	  ichoi1 = 1;
	  nsp_ctrlpack_dzdivq (&ichoi1, &nv1, &tp[1], nq, &tq[1]);
	  ichoi2 = 2;
	  nsp_ctrlpack_mzdivq (&ichoi2, &nv2, &tv[1], nq, &tq[1]);
	}
      maxnv[i__] = Max (nv1, nv2);
      i__2 = maxnv[i__] + 1;
      for (j = 1; j <= i__2; ++j)
	{
	  d1aux[j + i__ * d1aux_dim1] = tp[*nq + j] - tv[*nq + j];
	  /* L10: */
	}
      /* L20: */
    }
  /* 
   *    --- Calcul des derivees secondes de 'vq' --- 
   * 
   */
  i__1 = *nq;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *ng + *nq + 1;
      nsp_dset (&i__2, &c_b2, &tw[1], &c1);
      for (j = *nq; j >= 1; --j)
	{
	  if (j == *nq)
	    {
	      i__2 = maxnv[i__] + 1;
	      C2F (dcopy) (&i__2, &d1aux[i__ * d1aux_dim1 + 1], &c1,
			   &tw[*nq], &c1);
	      nw = maxnv[i__] + *nq - 1;
	      nsp_ctrlpack_dpodiv (&tw[1], &tq[1], &nw, nq);
	      nw -= *nq;
	    }
	  else
	    {
	      ichoix = 1;
	      nsp_ctrlpack_dzdivq (&ichoix, &nw, &tw[1], nq, &tq[1]);
	    }
	  i__2 = nw + 1;
	  for (k = 1; k <= i__2; ++k)
	    {
	      d2aux[i__ + (j + k * d2aux_dim2) * d2aux_dim1] = tw[*nq + k];
	      /* L30: */
	    }
	  maxnw[i__ + j * maxnw_dim1] = nw;
	  /* L40: */
	}
      /* L50: */
    }
  /* 
   *    --- Conclusion des calculs sur la hessienne --- 
   * 
   */
  i__1 = *nq;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = i__;
      for (j = 1; j <= i__2; ++j)
	{
	  nsp_ctrlpack_scapol (&maxnv[i__], &d1aux[i__ * d1aux_dim1 + 1],
			       &maxnv[j], &d1aux[j * d1aux_dim1 + 1], &y1);
	  /* 
	   */
	  if (maxnw[i__ + j * maxnw_dim1] > maxnw[j + i__ * maxnw_dim1])
	    {
	      maxij = maxnw[i__ + j * maxnw_dim1];
	      minij = maxnw[j + i__ * maxnw_dim1];
	      i__3 = maxij + 1;
	      for (k = minij + 2; k <= i__3; ++k)
		{
		  tij[k] = -d2aux[i__ + (j + k * d2aux_dim2) * d2aux_dim1];
		  /* L60: */
		}
	    }
	  else if (maxnw[i__ + j * maxnw_dim1] < maxnw[j + i__ * maxnw_dim1])
	    {
	      maxij = maxnw[j + i__ * maxnw_dim1];
	      minij = maxnw[i__ + j * maxnw_dim1];
	      i__3 = maxij + 1;
	      for (k = minij + 2; k <= i__3; ++k)
		{
		  tij[k] = -d2aux[j + (i__ + k * d2aux_dim2) * d2aux_dim1];
		  /* L70: */
		}
	    }
	  else
	    {
	      maxij = maxnw[i__ + j * maxnw_dim1];
	      minij = maxij;
	    }
	  /* 
	   */
	  i__3 = minij + 1;
	  for (k = 1; k <= i__3; ++k)
	    {
	      tij[k] =
		-d2aux[i__ + (j + k * d2aux_dim2) * d2aux_dim1] - d2aux[j +
									(i__ +
									 k *
									 d2aux_dim2)
									*
									d2aux_dim1];
	      /* L80: */
	    }
	  /* 
	   */
	  nsp_ctrlpack_scapol (&maxij, &tij[1], ng, &tr[ltvq], &y2);
	  if (i__ == j)
	    {
	      pd[i__ + i__ * pd_dim1] = (y1 + y2) * -2.;
	    }
	  else
	    {
	      pd[i__ + j * pd_dim1] = (y1 + y2) * -2.;
	      pd[j + i__ * pd_dim1] = (y1 + y2) * -2.;
	    }
	  /* L90: */
	}
      /* L100: */
    }
  return 0;
}				/* hl2_ */

/*!but 
 *    cette routine calcule, lorsque l'on connait le quotient et le reste 
 *       de la division par q d'un polynome, le reste et le quotient de 
 *       la division par q de ce    polynome multiplie par z. 
 *!liste d'appel 
 * 
 *     subroutine mzdivq(ichoix,nv,tv,nq,tq) 
 * 
 *    entree : 
 *    - ichoix. le nouveau reste ne sequential calculant 
 *         qu'avec le reste precedent, ce qui n'est pas le cas du 
 *         quotient, la possibilite est donnee de ne calculer que 
 *         ce reste. ichoix=1 .Si l'on desire calculer aussi le 
 *         quotient, ichoix=2. 
 *    - nv. est le degre du quotient entrant tv. 
 *    - tv. est le tableau contenant les coeff. du quotient. 
 *    - tr. est le tableau contenant les coeff. du reste de 
 *          degre nq-1. 
 *    - nq. est le degre du polynome tq. 
 *    - tq. est le tableau contenant les coeff. du pol. tq. 
 * 
 *    sortie : 
 *    - nv. est le degre du nouveau quotient. 
 *    - tv. contient les coeff. du nouveau quotient. 
 *    - tr. ceux du nouveau reste de degre toujours nq-1. 
 */

int nsp_ctrlpack_mzdivq (const int *ichoix, int *nv, double *tv,const int *nq,const double *tq)
{
  int i__1;
  double raux;
  int i__;

  raux = tv[*nq - 1];
  /* 
   *    -- Calcul du nouveau reste ------------- 
   * 
   */
  for (i__ = *nq - 1; i__ >= 1; --i__)
    {
      tv[i__] = tv[i__ - 1] - tq[i__] * raux;
      /* L20: */
    }
  /* 
   */
  tv[0] = -tq[0] * raux;
  /* 
   */
  if (*ichoix == 1)
    {
      return 0;
    }
  /* 
   *    -- Calcul du nouveau quotient ---------- 
   * 
   */
  i__1 = *nq;
  for (i__ = *nq + *nv; i__ >= i__1; --i__)
    {
      tv[i__ + 1] = tv[i__];
      /* L30: */
    }
  /* 
   */
  tv[*nq] = raux;
  ++(*nv);
  /* 
   */
  return 0;
}	


/*
 *    Etant sortie du domaine d'integration au cours 
 *    de l'execution de la routine Optm2, il s'agit ici de 
 *    gerer ou d'effectuer l'ensemble des taches necessaires 
 *    a l'obtention du point de la face par lequel la 
 *    'recherche' est passee. 
 *
 *    subroutine domout(neq,q,qi,nbout,ti,touti,itol,rtol,atol,itask, 
 *   *     istate,iopt,w,lrw,iw,liw,jacl2,mf,job) 
 * 
 *    double precision  atol(neq(1)+1),rtol(neq(1)+1),q(neq(1)+1), 
 *   *                  qi(neq(1)+1) 
 *    double precision w(*),iw(*) 
 * 
 *    Entree : 
 *    - neq. tableau entier de taille 3+(nq+1)*(nq+2) 
 *        neq(1)=nq est le degre effectif du polynome q 
 *        neq(2)=ng est le nombre de coefficient de fourier 
 *        neq(3)=dgmax degre maximum pour q (l'adresse des coeff de 
 *              fourier dans tq est neq(3)+2 
 *        neq(4:(nq+1)*(nq+2)) tableau de travail entier 
 *    - tq. tableau reel de taille au moins 
 *              7+dgmax+5*nq+6*ng+nq*ng+nq**2*(ng+1) 
 *        tq(1:nq+1) est le tableau des coefficients du polynome q. 
 *        tq(dgmax+2:dgmax+ng+2) est le tableau des coefficients 
 *                     de fourier 
 *        tq(dgmax+ng+3:) est un tableau de travail de taille au moins 
 *                        5+5*nq+5*ng+nq*ng+nq**2*(ng+1) 
 * 
 *    - toutes les variables et tableaux de variables necessaires a 
 *              l'execution de la routine Lsode 
 *    - qi. est le dernier point obtenu de la trajectoire 
 *       qui soit a l'interieur du domaine. 
 *    - q(1:nq+1). est celui precedemment calcule, qui se situe a 
 *      l'exterieur. 
 * 
 *    Sortie : 
 *    - q(1:nq+1). est cense etre le point correspondant a l'inter- 
 *       section entre la face et la trajectoire. 
 *    - job. est un parametre indiquant si le franchissement 
 *           est verifie. 
 *           si job=-1 pb de detection arret requis 
 * 
 *    Tableaux de travail 
 *    - w : 24+22*nq+ng+nq**2 
 *    - iw : 20+nq 
 *! 
 * 
 */

int
nsp_ctrlpack_domout (int *neq, double *q, double *qi, int *nbout, double *ti,
		     double *touti, int *itol, double *rtol, double *atol,
		     int *itask, int *istate, int *iopt, double *w, int *lrw,
		     int *iw, int *liw, U_fp jacl2, int *mf, int *job)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  int kmax, ierr;
  double tpas;
  int lqex;
  double tout, eps390;
  int k, nface;
  double t, x;
  double tsave;
  int nqmax;
  int nqsav, k0, ng;
  int lq, nq;
  double yf, yi;
  int lw;
  double xx[1];
  int nboute, newrap, lrwork;
  int ltg;

  /* Parameter adjustments */
  --iw;
  --w;
  --atol;
  --rtol;
  --qi;
  --q;
  --neq;

  /* Function Body */
  nq = neq[1];
  ng = neq[2];
  nqmax = neq[3];
  /* 
   */
  lq = 1;
  ltg = lq + nqmax + 1;
  /* 
   *Computing 2nd power 
   */
  i__1 = nq;
  *lrw = i__1 * i__1 + nq * 9 + 22;
  *liw = nq + 20;
  /* 
   */
  lrwork = 1;
  /*Computing 2nd power 
   */
  i__1 = nq;
  lw = lrwork + i__1 * i__1 + nq * 9 + 22;
  lqex = lw + nq * 12 + ng + 1;
  /* free = (double) (lqex + nq + 1); */
  /* 
   */
  tout = *touti;
  nboute = 0;
  /* 
   *    --- Etape d'approche de la frontiere ---------------------------- 
   * 
   */
  kmax = (int) (log ((tout - *ti) / .00625) / log (2.));
  k0 = 1;
  if (sortie_info > 1)
    {
      static const int c__40 = 40;
      nsp_ctrlpack_outl2 (&c__40, &nq, &kmax, xx, xx, &x, &x);
    }
 L314:
  i__1 = kmax;
  for (k = k0; k <= i__1; ++k)
    {
      tpas = (tout - *ti) / 2.;
      if (*nbout > 0)
	{
	  *istate = 1;
	  i__2 = nq + 1;
	  C2F (dcopy) (&i__2, &qi[1], &c1, &q[1], &c1);
	  t = *ti;
	  tout = *ti + tpas;
	}
      else
	{
	  i__2 = nq + 1;
	  C2F (dcopy) (&i__2, &q[1], &c1, &qi[1], &c1);
	  *ti = t;
	  tout = *ti + tpas;
	}
    L340:
      if (sortie_info > 1)
	{
	  static const int c__41 = 41;
	  nsp_ctrlpack_outl2 (&c__41, &nq, &nq, &q[1], xx, &t, &tout);
	}
      tsave = t;
      C2F(lsode) ((ode_f) nsp_ctrlpack_feq, &neq[1], &q[1], &t, &tout, itol,
		  &rtol[1], &atol[1], itask, istate, iopt, &w[lrwork],
		  lrw, &iw[1], liw, (U_fp) jacl2, mf,NULL);
      if (sortie_info > 1)
	{
	  static const int c__42 = 42;
	  nsp_ctrlpack_outl2 (&c__42, &nq, &nq, &q[1], xx, &t, &tout);
	}
      if (*istate == -1 && t != tout)
	{
	  if (sortie_info > 1)
	    {
	      static const int c__43 = 43;
	      nsp_ctrlpack_outl2 (&c__43, &nq, &nq, xx, xx, &x, &x);
	    }
	  if (t <= tsave)
	    {
	      *job = -1;
	      return 0;
	    }
	  *istate = 2;
	  goto L340;
	}
      nsp_ctrlpack_front (&nq, &q[1], nbout, &w[lw]);
      if (sortie_info > 1)
	{
	  static const int c__44 = 44;
	  nsp_ctrlpack_outl2 (&c__44, &nq, nbout, xx, xx, &x, &x);
	}
      if (*nbout > 0)
	{
	  nboute = *nbout;
	  i__2 = nq + 1;
	  C2F (dcopy) (&i__2, &q[1], &c1, &w[lqex], &c1);
	}
      if (*istate < 0)
	{
	  if (sortie_info > 1)
	    {
	      static const int c__45 = 45;
	      nsp_ctrlpack_outl2 (&c__45, &nq, istate, xx, xx, &x, &x);
	    }
	  *job = -1;
	  return 0;
	}
      if (k == kmax && nboute == 0 && tout != *touti)
	{
	  tout = *touti;
	  goto L340;
	}
      /* L380: */
    }
  /* 
   */
  if (nboute == 0)
    {
      *job = 0;
      return 0;
    }
  else if (nboute > 2)
    {
      newrap = 1;
      nqsav = nq;
      goto L390;
    }
  /* 
   */
  nsp_ctrlpack_watfac (&nq, &w[lqex], &nface, &newrap, &w[lw]);
  if (newrap == 1)
    {
      goto L390;
    }
  /* 
   */
  nqsav = nq;
  nsp_ctrlpack_onface (&nq, &q[1], &q[ltg], &ng, &nface, &ierr, &w[lw]);
  if (ierr != 0)
    {
      *job = -1;
      return 0;
    }
  yi = nsp_ctrlpack_phi (&qi[1], &nqsav, &q[ltg], &ng, &w[lw]);
  yf = nsp_ctrlpack_phi (&q[1], &nq, &q[ltg], &ng, &w[lw]);
  /* 
   */
  eps390 = 1e-8;
  if (yi < yf - eps390)
    {
      newrap = 1;
      goto L390;
    }
  /* 
   */
  if (sortie_info > 1)
    {
      static const int c__46 = 46;
      nsp_ctrlpack_outl2 (&c__46, &nq, &nface, &q[1], xx, &yi, &yf);
    }
  /* 
   */
  newrap = 0;
  /* 
   */
 L390:
  if (newrap == 1)
    {
      nq = nqsav;
      k0 = kmax;
      ++kmax;
      *nbout = 1;
      if (*ti + tpas * 2 <= *ti)
	{
	  *job = -1;
	  return 0;
	}
      tout = *ti + tpas * 2;
      if (sortie_info > 1)
	{
	  static const int c__47 = 47;
	  nsp_ctrlpack_outl2 (&c__47, &nq, &nq, xx, &qi[1], &x, &x);
	}
      goto L314;
    }
  /* 
   */
  neq[1] = nq;
  *job = 1;
  return 0;
  /* 
   */
}				/* domout_ */

/*!but 
 *    calcule ici les quotient et reste de la division 
 *      par q d'un polynome p, a partir des quotient et reste 
 *      de la division par q du produit de ce polynome par z. 
 *!liste d'appel 
 *    subroutine dzdivq(ichoix,nv,tv,nq,tq) 
 *    Entree : 
 *    - ichoix. prend la valeur 1 si l'on ne desire que 
 *      calculer le nouveau quotient (puisqu'il ne se calcule 
 *      qu'a partir du precedent. 2 sinon. 
 *    - nv. est le degre du quotient entrant tv. 
 *    - tv. est le tableau contenant les coeff. du quotient. 
 *    - tr. est le tableau contenant les coeff. du reste de 
 *      degre nq-1. 
 *    - nq. est le degre du polynome tq. 
 *    - tq. est le tableau contenant les coeff. du pol. tq. 
 * 
 *    sortie : 
 *    - nv. est le degre du nouveau quotient. 
 *    - tv. contient les coeff. du nouveau quotient. 
 *    - tr. ceux du nouveau reste de degre toujours nq-1. 
 * 
 */

int
nsp_ctrlpack_dzdivq (int *ichoix, int *nv, double *tv, int *nq, double *tq)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  double vaux;
  int i__;

  vaux = tv[*nq];
  /* 
   *    -- Calcul du nouveau quotient --------- 
   * 
   */
  i__1 = *nq + *nv - 1;
  for (i__ = *nq; i__ <= i__1; ++i__)
    {
      tv[i__] = tv[i__ + 1];
      /* L20: */
    }
  /* 
   */
  tv[*nq + *nv] = 0.;
  --(*nv);
  /* 
   */
  if (*ichoix == 1)
    {
      return 0;
    }
  /* 
   *    -- calcul du nouveau reste ------------ 
   * 
   */
  i__1 = *nq - 2;
  for (i__ = 0; i__ <= i__1; ++i__)
    {
      tv[i__] = vaux * tq[i__ + 1] + tv[i__ + 1];
      /* L30: */
    }
  /* 
   */
  tv[*nq - 1] = vaux;
  /* 
   */
  return 0;
}				/* dzdivq_ */


static int pow_ii(const int *ap, const int *bp);

/* Table of constant values */

/*!but 
 *    il est question ici de calculer (ou d estimer) 
 *    le polynome (ou point) qui se situe a l'intersection 
 *    de la recherche et de la face-frontiere du domaine. 
 *!liste d'appel 
 *    subroutine onface(nq,tq,nprox) 
 * 
 *    double precision tq(0:nq),w(*) 
 *    int nq,nprox,ierr 
 * 
 *    Entree : 
 *    - nq. est le degre du polynome q(z) avant toute 
 *       modification. 
 *    - tq. est le tableau de ses coefficients 
 *    - nprox. est l indice de la face par laquelle on estime 
 *       que la recherche a franchi la frontiere du domaine. 
 * 
 *    Sortie : 
 *    -nq. est alors le degre des polynomes de la face 
 *      traversee et donc du polynome intersection. Sa valeur 
 *      est inferieur de 1 ou 2 a sa valeur precedente. 
 *    - tq. contient en sortie les coefficients du polynome 
 *      intersection dans le domaine de la face traversee. 
 * 
 *    Tableau de travail 
 *    - w : 12*nq+ng+1 
 */


int
nsp_ctrlpack_onface (int *nq, double *tq, double *tg, int *ng, int *nprox,
		     int *ierr, double *w)
{
  /* System generated locals */
  int i__1, i__2, i__3;
  double d__1;

  /* Local variables */
  double srgd;
  int ndiv;
  double beta0 =0 ;
  int lrgd0, lrgd1;
  double auxt1, taux2[3], auxt2;
  int i__, j, k;
  double t;
  int lbeta;
  double x;
  int nbeta;
  int lqdot;
  int nqdot, lqaux;
  double t0, tmult, tabeta[3];
  int lw;
  double xx[1];
  int lgp, ngp;
  double srq, tps[2];
  int lgp1, lrq0, lrq1;

  /* Parameter adjustments */
  --tg;
  --w;

  /* Function Body */
  lqaux = 1;
  lqdot = lqaux;
  lrq0 = lqdot + *nq + 1;
  lrq1 = lrq0 + *nq;
  lrgd0 = lrq1 + *nq;
  lrgd1 = lrgd0 + *nq;
  lgp = lrgd1 + *nq;
  lgp1 = lgp + (*nq << 1) - 2;
  lbeta = lgp1;
  lw = lbeta + (*nq << 1) - 2;
  /* lfree = lw + *nq * 3 + *ng + 1; */
  /* 
   */
  /* nqvra = *nq; */
  /* 
   */
  tps[1] = 1.;
  tps[0] = 1.;
  /* 
   */
  if (*nprox != 0)
    {
      tps[0] = (double) (*nprox);
      /*    calcul du reste de la division de q par tps 
       */
      d__1 = -tps[0];
      nsp_ctrlpack_horner (tq, nq, &d__1, &c_b2, &srq, xx);
      /*    calcul du reste de la division de qdot  par 1+z 
       */
      nsp_ctrlpack_feq1 (nq, &t, tq, &tg[1], ng, &w[lqdot], &w[lw]);
      d__1 = -tps[0];
      nsp_ctrlpack_horner (&w[lqdot], nq, &d__1, &c_b2, &srgd, xx);
      /* 
       */
      d__1 = -srq / srgd;
      C2F (daxpy) (nq, &d__1, &w[lqdot], &c1, tq, &c1);
      /* 
       */
      nsp_ctrlpack_dpodiv (tq, tps, nq, &c1);
      if (sortie_info > 0)
	{
	  static const int c__70 = 70;
	  nsp_ctrlpack_outl2 (&c__70, &c1, &c1, xx, xx, &x, &x);
	}
      if (sortie_info > 1)
	{
	  static const int c__71 = 71;
	  nsp_ctrlpack_outl2 (&c__71, &c1, &c1, tq, xx, &x, &x);
	}
      C2F (dcopy) (nq, &tq[1], &c1, tq, &c1);
      --(*nq);
      /* 
       */
    }
  else if (*nprox == 0)
    {
      /* 
       */
      taux2[2] = 1.;
      taux2[1] = 0.;
      taux2[0] = 1.;
      /* 
       */
      i__1 = *nq + 1;
      C2F (dcopy) (&i__1, tq, &c1, &w[lqaux], &c1);
      i__1 = *nq - 2;
      for (ndiv = 0; ndiv <= i__1; ++ndiv)
	{
	  i__2 = *nq - ndiv;
	  nsp_ctrlpack_dpodiv (&w[lqaux], taux2, &i__2, &c2);
	  w[lrq1 + ndiv] = w[lqaux + 1];
	  w[lrq0 + ndiv] = w[lqaux];
	  /* 
	   */
	  i__2 = *nq - ndiv;
	  for (j = 2; j <= i__2; ++j)
	    {
	      w[lqaux + j - 1] = w[lqaux + j];
	      /* L180: */
	    }
	  w[lqaux] = 0.;
	  /* L200: */
	}
      w[lrq1 - 1 + *nq] = w[lqaux + 1];
      w[lrq0 - 1 + *nq] = w[lqaux];
      /* 
       */
      nsp_ctrlpack_feq1 (nq, &t, tq, &tg[1], ng, &w[lqaux], &w[lw]);
      nqdot = *nq - 1;
      /* 
       */
      i__1 = nqdot - 2;
      for (ndiv = 0; ndiv <= i__1; ++ndiv)
	{
	  i__2 = nqdot - ndiv;
	  nsp_ctrlpack_dpodiv (&w[lqaux], taux2, &i__2, &c2);
	  w[lrgd1 + ndiv] = w[lqaux + 1];
	  w[lrgd0 + ndiv] = w[lqaux];
	  /* 
	   */
	  i__2 = nqdot - ndiv;
	  for (j = 2; j <= i__2; ++j)
	    {
	      w[lqaux + j - 1] = w[lqaux + j];
	      /* L220: */
	    }
	  w[lqaux] = 0.;
	  /* L240: */
	}
      w[lrgd1 - 1 + nqdot] = w[lqaux + 1];
      w[lrgd0 - 1 + nqdot] = w[lqaux];
      /* 
       *    - construction du polynome gp(z) dont on cherchera une racine 
       *    comprise entre -2 et +2 ----------------------------- 
       * 
       */
      i__1 = (*nq << 1) - 2;
      nsp_dset (&i__1, &c_b2, &w[lgp], &c1);
      i__1 = (*nq << 1) - 2;
      nsp_dset (&i__1, &c_b2, &w[lgp1], &c1);
      /* 
       */
      i__1 = *nq;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = nqdot;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      k = i__ + j - 2;
	      w[lgp + k] +=
		pow_ii (&c_n1, &k) * w[lrq0 - 1 + j] * w[lrgd1 - 1 + i__];
	      w[lgp1 + k] +=
		pow_ii (&c_n1, &k) * w[lrq1 - 1 + j] * w[lrgd0 - 1 + i__];
	      /* L258: */
	    }
	  /* L260: */
	}
      /* 
       */
      i__1 = (*nq << 1) - 2;
      nsp_calpack_ddif (&i__1, &w[lgp1], &c1, &w[lgp], &c1);
      ngp = (*nq << 1) - 3;
      nsp_ctrlpack_rootgp (&ngp, &w[lgp], &nbeta, &w[lbeta], ierr, &w[lw]);
      if (*ierr != 0)
	{
	  return 0;
	}
      /* 
       */
      i__1 = nbeta;
      for (k = 1; k <= i__1; ++k)
	{
	  /* 
	   *    - calcul de t (coeff multiplicateur) - 
	   * 
	   */
	  auxt1 = 0.;
	  i__2 = *nq;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      d__1 = -w[lbeta - 1 + k];
	      i__3 = i__ - 1;
	      auxt1 += w[lrq1 - 1 + i__] * pow_di (d__1, i__3);
	      /* L280: */
	    }
	  /* 
	   */
	  auxt2 = 0.;
	  i__2 = nqdot;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      d__1 = -w[lbeta - 1 + k];
	      i__3 = i__ - 1;
	      auxt2 += w[lrgd1 - 1 + i__] * pow_di (d__1, i__3);
	      /* L290: */
	    }
	  /* 
	   */
	  tmult = -auxt1 / auxt2;
	  /* 
	   */
	  if (k == 1)
	    {
	      t0 = tmult;
	      beta0 = w[lbeta];
	    }
	  else if (Abs (tmult) < Abs (t0))
	    {
	      t0 = tmult;
	      beta0 = w[lbeta - 1 + k];
	    }
	  /* 
	   */
	  /* L299: */
	}
      /* 
       */
      nsp_ctrlpack_feq1 (nq, &t, tq, &tg[1], ng, &w[lqdot], &w[lw]);
      C2F (daxpy) (nq, &t0, &w[lqdot], &c1, tq, &c1);
      /* 
       */
      tabeta[2] = 1.;
      tabeta[1] = beta0;
      tabeta[0] = 1.;
      nsp_ctrlpack_dpodiv (tq, tabeta, nq, &c2);
      if (sortie_info > 0)
	{
	  static const int c__70 = 70;
	  nsp_ctrlpack_outl2 (&c__70, &c2, &c2, xx, xx, &x, &x);
	}
      if (sortie_info > 1)
	{
	  static const int c__71 = 71;
	  nsp_ctrlpack_outl2 (&c__71, &c2, &c2, tq, xx, &x, &x);
	}
      /* 
       */
      i__1 = *nq - 1;
      C2F (dcopy) (&i__1, &tq[2], &c1, tq, &c1);
      *nq += -2;
      /* 
       */
    }
  /* 
   */
  return 0;
}				/* onface_ */


static int pow_ii(const int *ap, const int *bp)
{
  int pow, x, n;
  unsigned long u;

  x = *ap;
  n = *bp;

  if (n <= 0) {
    if (n == 0 || x == 1)
      return 1;
    if (x != -1)
      return x == 0 ? 1/x : 0;
    n = -n;
  }
  u = n;
  for(pow = 1; ; )
    {
      if(u & 01)
	pow *= x;
      if(u >>= 1)
	x *= x;
      else
	break;
    }
  return(pow);
}


struct
{
  int info, i1;
} arl2c_;

#define arl2c_1 arl2c_

/* 
 * 
 *    Entree : - gpp. est le tableau contenant les coeff du polynome 
 *             gpp(z) et dont le degre est ngp. 
 *             - ngp. est le degre de gp(z). 
 *             - w tableau de travail de taille 3*ngp+1 
 *    Sortie : - beta. est le tableau contenant les racines du 
 *             polynome gpp(z) reelles comprises entre -2 et 2. 
 *             - nbeta. est le nombre de ces racines. 
 * 
 */

int
nsp_ctrlpack_rootgp (int *ngp, double *gpp, int *nbeta, double *beta,
		     int *ierr, double *w)
{
  int i__1;
  double d__1;

  int fail;
  int kpol, j;/*  kfree; */
  int kzi, kzr;
  
  --gpp;
  --beta;
  --w;

  /* Function Body */
  kpol = 1;
  kzr = kpol + *ngp + 1;
  kzi = kzr + *ngp;

  i__1 = *ngp + 1;
  C2F (dcopy) (&i__1, &gpp[1], &c_n1, &w[kpol], &c1);
  nsp_ctrlpack_rpoly (&w[kpol], ngp, &w[kzr], &w[kzi], &fail);
  *nbeta = 0;
  i__1 = *ngp - 1;
  for (j = 0; j <= i__1; ++j)
    {
      if (w[kzi + j] == 0. && (d__1 = w[kzr + j], Abs (d__1)) <= 2.)
	{
	  ++(*nbeta);
	  beta[*nbeta] = w[kzr + j];
	}
      /* L110: */
    }
  if (*nbeta == 0)
    {
      *ierr = 4;
      return 0;
    }
  return 0;
}

