#ifndef INTEG_H
#define INTEG_H

#include <nsp/math.h>
#include <nsp/machine.h>
#include <nsp/sciio.h>
#include <nsp/interf.h>
#include <nsp/blas.h>
#include <nsp/ode_solvers.h>

#define FORTRAN_NAMES

#ifdef FORTRAN_NAMES 
#define nsp_twodq C2F(twodq)
#define nsp_ode_ainvg C2F(ainvg)
#define nsp_ode_bnorm C2F(bnorm)
#define nsp_ode_cfode C2F(cfode)
#define nsp_ode_daxpy C2F(daxpy)
#define nsp_ode_dcnst0 C2F(dcnst0)
#define nsp_ode_dcnstr C2F(dcnstr)
#define nsp_ode_dcopy C2F(dcopy)
#define nsp_ode_ddaini C2F(ddaini)
#define nsp_ode_ddanrm C2F(ddanrm)
#define nsp_ode_ddaskr C2F(ddaskr)
#define nsp_ode_ddaslv C2F(ddaslv)
#define nsp_ode_ddasrt C2F(ddasrt)
#define nsp_ode_ddassl C2F(ddassl)
#define nsp_ode_ddastp C2F(ddastp)
#define nsp_ode_ddatrp C2F(ddatrp)
#define nsp_ode_ddatrp1 C2F(ddatrp1)
#define nsp_ode_ddawts C2F(ddawts)
#define nsp_ode_ddawts1 C2F(ddawts1)
#define nsp_ode_ddot C2F(ddot)
#define nsp_ode_ddwnrm C2F(ddwnrm)
#define nsp_ode_dgbfa C2F(dgbfa)
#define nsp_ode_dgbsl C2F(dgbsl)
#define nsp_ode_dhels C2F(dhels)
#define nsp_ode_dheqr C2F(dheqr)
#define nsp_ode_dinvwt C2F(dinvwt)
/* #define nsp_ode_dlamch C2F(dlamch) */
#define nsp_ode_dorth C2F(dorth)
#define nsp_ode_drchek C2F(drchek)
#define nsp_ode_droots C2F(droots)
#define nsp_ode_droots0 C2F(droots0)
#define nsp_ode_droots1 C2F(droots1)
#define nsp_ode_droots2 C2F(droots2)
#define nsp_ode_dscal C2F(dscal)
#define nsp_ode_dslvd C2F(dslvd)
#define nsp_ode_dyypnw C2F(dyypnw)
#define nsp_ode_ewset C2F(ewset)
#define nsp_ode_fehl2 C2F(fehl2)
#define nsp_ode_fnorm C2F(fnorm)
#define nsp_ode_greatr C2F(greatr)
#define nsp_ode_idamax C2F(idamax)
#define nsp_ode_intdy C2F(intdy)
#define nsp_ode_lsdisc C2F(lsdisc)
#define nsp_ode_lsoda C2F(lsoda)
#define nsp_ode_lsodar C2F(lsodar)
#define nsp_ode_lsodar2 C2F(lsodar2)
#define nsp_ode_lsode C2F(lsode)
#define nsp_ode_lsodi C2F(lsodi)
#define nsp_ode_lsrgk C2F(lsrgk)
#define nsp_ode_nspdqagie C2F(nspdqagie)
#define nsp_ode_nspdqagse C2F(nspdqagse)
#define nsp_ode_nspdqelg C2F(nspdqelg)
#define nsp_ode_nspdqk15i C2F(nspdqk15i)
#define nsp_ode_nspdqk21 C2F(nspdqk21)
#define nsp_ode_nspdqk21b C2F(nspdqk21b)
#define nsp_ode_nspdqpsrt C2F(nspdqpsrt)
#define nsp_ode_odeint C2F(odeint)
#define nsp_ode_prepj C2F(prepj)
#define nsp_ode_prepji C2F(prepji)
#define nsp_ode_prja C2F(prja)
#define nsp_ode_rchek C2F(rchek)
#define nsp_ode_rchek2 C2F(rchek2)
#define nsp_ode_rk4 C2F(rk4)
#define nsp_ode_rkf45 C2F(rkf45)
#define nsp_ode_rkqc C2F(rkqc)
#define nsp_ode_rksimp C2F(rksimp)
#define nsp_ode_roots C2F(roots)
#define nsp_ode_roots2 C2F(roots2)
#define nsp_ode_rscar1 C2F(rscar1)
#define nsp_ode_rscma1 C2F(rscma1)
#define nsp_ode_rscom1 C2F(rscom1)
#define nsp_ode_solsy C2F(solsy)
#define nsp_ode_stoda C2F(stoda)
#define nsp_ode_stode C2F(stode)
#define nsp_ode_stodi C2F(stodi)
#define nsp_ode_svcar1 C2F(svcar1)
#define nsp_ode_svcma1 C2F(svcma1)
#define nsp_ode_svcom1 C2F(svcom1)
#define nsp_ode_vmnorm C2F(vmnorm)
#define nsp_ode_vnorm C2F(vnorm)
#define nsp_ode_xermsg C2F(xermsg)
#define nsp_ode_xerrwd C2F(xerrwd)
#define nsp_ode_xsetf1 C2F(xsetf1)
#define nsp_ode_xsetun1 C2F(xsetun1)
#endif 

/* lsoda */

typedef int (*xxode_f) (const int *neq,const double *t ,const double *y,double *yp, void *param);
typedef int (*xxode_jac)(const int *neq,const double * t,const double * y,const int* ml,
			 const int *mu, double * p, const int *nrowp, void *param);

/* dassrt and dassl */

typedef int (*dassl_g) (const int *neq, double *,double *y,int *ng,double *g,
			double * rpar,  int *ipar);

typedef int (*dassl_jac) (const double *x, double *y, double * yprime, double * wm,
			  double * cj, double * rpar,int * ipar);

typedef int (*dassl_res) (const double *x, double *y, double * yprime,
			  double * e, int * ires, double * rpar,int * ipar);

/* lsodi */

typedef int (*lsodi_res)(const int *neq,const double *t,const double *y,const double *s,double *r,
		     int *ires );


typedef int (*Dgbydy)(const int *neq,const double *t,const double *y,const double *s,
		      const int* ml,const int *mu,double * p, const int *nrowp );

typedef int (*Pjac)(int *neq, double *y, double *yh, int *nyh,
		    double *ewt, double *rtem, double *savr,
		    double *s, double *wm, int *iwm,lsodi_res  res,
		    Dgbydy jac, ode_jac adda, void *param);

typedef int (*Slvs)(double *wm, int *iwm, double *x, double *tem);


typedef int (*Pjac1)(int *neq, double *y, double *yh, int *nyh,
		     double *ewt, double *ftem, double *savf,
		     double *wm, int *iwm, ode_f f, ode_jac jac,
		     void *param);

/* lsodar */

typedef int (*lsodar_g)(const int *neq,const double *t,const  double *y,const int *ng,
			double *gout,double *param);


typedef int (*Dres)(double *T,double * Y,double * YPRIME,double * CJ,double *DELTA,
		    int *IRES,double * RPAR,int * IPAR) ;

typedef int (*Psol)(int *neq, double *T,double * Y,double * YPRIME,double *savr,
		    double *wk,double *cj,double *wght, double *wp, int *iwp, 
		    double *b, double *eplin, int *ier,double * RPAR,int * IPAR) ;

typedef int (*Rt)(int *neq, double *T,double * Y,double * YP,int *nrt,
		  double *rval,double * RPAR,int * IPAR) ;

typedef int  (*Jacd) (double *x,double *y, double *yp,double *,double * cj,double *rpar,int *ipar);

typedef int (*Jack) (Dres res,int *ires,int *neq,double *x,
		     double *y,double * yprime,double * wt,
		     double *delta,double * r__,double * h__,double * cj,
		     double *wm,int *iwm, int * ierpj,double *rpar,int *ipar);

typedef int (*Dnedd) (double *x, double *y, double *yprime, int *neq, Dres res,
		      Jacd jacd, double *pdum, double *h__, double *wt,
		      int *jstart, int *idid, double *rpar, int *ipar,
		      double *phi, double *gamma, double *dumsvr, double *delta,
		      double *e, double *wm, int *iwm, double *cj, double *cjold,
		      double *cjlast, double *s, double *uround, double *dume,
		      double *dums, double *dumr, double *epcon, int *jcalc,
		      int *jfdum, int *kp1, int *nonneg, int *ntype, int *iernls);


typedef int (*Dnedk)  (double *x, double *y, double *yprime, int *neq, Dres res,
		       Jack jack, Psol psol, double *h__, double *wt, int *jstart,
		       int *idid, double *rpar, int *ipar, double *phi,
		       double *gamma, double *savr, double *delta, double *e,
		       double *wm, int *iwm, double *cj, double *cjold,
		       double *cjlast, double *s, double *uround, double *epli,
		       double *sqrtn, double *rsqrtn, double *epcon, int *jcalc,
		       int *jflg, int *kp1, int *nonneg, int *ntype, int *iernls);

typedef union { Jacd d; Jack k;} UJac;
typedef union { Dnedd d ; Dnedk k ;} Dned_dk ;
typedef union { Psol psol ; double *pdum;} Dnedd_arg;

typedef int (*Ddasid) (double *x, double *y, double *yprime, int *neq, int *icopt,
		       int *id, Dres res, Jacd jacd, double *pdum, double *h__,
		       double *tscale, double *wt, int *jsdum, double *rpar,
		       int *ipar, double *dumsvr, double *delta, double *r__,
		       double *yic, double *ypic, double *dumpwk, double *wm,
		       int *iwm, double *cj, double *uround, double *dume,
		       double *dums, double *dumr, double *epcon, double *ratemx,
		       double *stptol, int *jfdum, int *icnflg, int *icnstr,
		       int *iernls);

typedef int (*Ddasik) (double *x, double *y, double *yprime, int *neq, int *icopt,
		       int *id, Dres res, Jack jack, Psol psol, double *h__,
		       double *tscale, double *wt, int *jskip, double *rpar,
		       int *ipar, double *savr, double *delta, double *r__,
		       double *yic, double *ypic, double *pwk, double *wm,
		       int *iwm, double *cj, double *uround, double *epli,
		       double *sqrtn, double *rsqrtn, double *epcon,
		       double *ratemx, double *stptol, int *jflg, int *icnflg,
		       int *icnstr, int *iernls);

typedef union { Ddasid d ; Ddasik k ;} Nsik ;
typedef union { double *d; Psol k;} UPsol;

/* prototypes */

extern int C2F(ddaskr) (Dres res, int *neq, double *t, double *y, double *yprime,
			double *tout, int *info, double *rtol, double *atol,
			int *idid, double *rwork, int *lrw, int *iwork, int *liw,
			double *rpar, int *ipar, UJac jac, UPsol psol, Rt rt,
			int *nrt, int *jroot);


extern int C2F(dgefa) (double *a, int *lda, int *n, int *ipvt,
		       int *info);
extern int C2F(dgesl) (double *a, int *lda, int *n, int *ipvt,
		       double *b, int *job);

extern int C2F(lsodar2) (ode_f f, int *neq, double *y, double *t, double *tout,
			 int *itol, double *rtol, double *atol, int *itask,
			 int *istate, int *iopt, double *rwork, int *lrw,
			 int *iwork, int *liw, ode_jac jac, int *jt, lsodar_g g, int *ng,
			 int *jroot, void *param);

extern int C2F(xerrwv)(char *msg, int *nmes, int *nerr, int *iert, int *ni, int *i1, int *i2, 
		       int *nr, double *r1, double *r2, unsigned int  msg_len);

extern int C2F(xerrwvb)(char *msg, int *nmes, int *nerr, int *iert, int *ni,
			int *i1, int *i2, 
			int *nr, double *r1, double *r2, unsigned int  msg_len);

extern int nsp_colnew_appsln(double *x, double *z__, double *fspace, int *ispace);


/* */

extern int nsp_ode_lsodi (lsodi_res res, ode_jac adda, Dgbydy jac, int *neq, double *y,
			     double *ydoti, double *t, double *tout, int *itol,
			     double *rtol, double *atol, int *itask, int *istate,
			     int *iopt, double *rwork, int *lrw, int *iwork, int *liw,
			     int *mf, void *parm);

extern int nsp_ode_stodi (int *neq, double *y, double *yh, int *nyh, double *yh1,
			     double *ewt, double *savf, double *savr, double *acor,
			     double *wm, int *iwm, lsodi_res res, ode_jac adda, Dgbydy jac,
			     Pjac pjac, Slvs slvs, void *param);

extern int nsp_ode_prepji(int *neq, double *y, double *yh, int *nyh,
			     double *ewt, double *rtem, double *savr,
			     double *s, double *wm, int *iwm,lsodi_res  res,
			     Dgbydy jac, ode_jac adda, void *param);

extern int nsp_ode_prepj (int *neq, double *y, double *yh, int *nyh, double *ewt,
			     double *ftem, double *savf, double *wm, int *iwm, ode_f f,
			     ode_jac jac, void *param);


/* */

extern int nsp_ode_ainvg (lsodi_res res, ode_jac adda, int *neq, double *t, double *y,
			     double *ydot, int *miter, int *ml, int *mu, double *pw,
			     int *ipvt, int *ier, void *param);

extern int nsp_ode_cfode (const int *meth, double *elco, double *tesco);


extern int nsp_ode_xerrwd (char *msg, int *nmes, int *nerr, int *level,
			      int *ni, int *i1, int *i2, int *nr, double *r1,
			      double *r2, long int msg_len);


extern int nsp_ode_xsetf1 (int *mflag);
extern int nsp_ode_xsetun1 (int *lun);


extern int nsp_ode_droots2 (int *nrt, double *hmin, int *jflag, double *x0,
			       double *x1, double *r0, double *r1, double *rx,
			       double *x, int *jroot);

extern int nsp_ode_droots1 (int *nrt, double *hmin, int *jflag, double *x0,
			       double *x1, double *r0, double *r1, double *rx,
			       double *x, int *jroot);

extern int nsp_ode_droots0 (int *nrt, double *hmin, int *jflag, double *x0,
			       double *x1, double *r0, double *r1, double *rx,
			       double *x, int *jroot);

extern int nsp_ode_dyypnw (int *neq, double *y, double *yprime, double *cj,
			      double *rl, double *p, int *icopt, int *id,
			      double *ynew, double *ypnew);
extern int nsp_ode_dcnstr (int *neq, double *y, double *ynew, int *icnstr,
			      double *tau, double *rlx, int *iret, int *ivar);
extern int nsp_ode_dcnst0 (int *neq, double *y, int *icnstr, int *iret);
extern int nsp_ode_ddawts1 (int *neq, int *iwt, double *rtol, double *atol,
			       double *y, double *wt, double *rpar,
			       int *ipar);
extern int nsp_ode_dinvwt (int *neq, double *wt, int *ier);
extern int nsp_ode_ddatrp1 (double *x, double *xout, double *yout,
			       double *ypout, int *neq, int *kold,
			       double *phi, double *psi);
extern double nsp_ode_ddwnrm (int *neq, double *v, double *rwt,
				 double *rpar, int *ipar);


extern int nsp_ode_dslvd (int *neq, double *delta, double *wm, int *iwm);
extern int nsp_ode_dorth (double *vnew, double *v, double *hes, int *n,
			     int *ll, int *ldhes, int *kmp, double *snormw);
extern int nsp_ode_dheqr (double *a, int *lda, int *n, double *q,
			     int *info, int *ijob);
extern int nsp_ode_dhels (double *a, int *lda, int *n, double *q,
			     double *b);
extern int nsp_ode_ddasrt (dassl_res res, int *neq, double *t, double *y,
			      double *yprime, double *tout, int *info,
			      double *rtol, double *atol, int *idid,
			      double *rwork, int *lrw, int *iwork, int *liw,
			      double *rpar, int *ipar, dassl_jac jac, dassl_g g,
			      int *ng, int *jroot);
extern int nsp_ode_drchek (int *job, dassl_g g, int *ng, int *neq, double *tn,
			      double *tout, double *y, double *yp,
			      double *phi, double *psi, int *kold, double *g0,
			      double *g1, double *gx, int *jroot, int *irt,
			      double *uround, int *info3, double *rwork,
			      int *iwork, double *rpar, int *ipar);
extern int nsp_ode_droots (int *ng, double *hmin, int *jflag, double *x0,
			      double *x1, double *g0, double *g1, double *gx,
			      double *x, int *jroot, int *imax, int *last,
			      double *alpha, double *x2);
extern int nsp_ode_ddaini (double *x, double *y, double *yprime, int *neq,
			      dassl_res res, dassl_jac jac, double *h__, double *wt,
			      int *idid, double *rpar, int *ipar, double *phi,
			      double *delta, double *e, double *wm, int *iwm,
			      double *hmin, double *uround, int *nonneg,
			      int *ntemp);

extern double nsp_ode_ddanrm (int *neq, double *v, double *wt,
				 double *rpar, int *ipar);
extern int nsp_ode_ddaslv (int *neq, double *delta, double *wm, int *iwm);
extern int nsp_ode_ddassl (dassl_res res, int *neq, double *t, double *y,
			      double *yprime, double *tout, int *info,
			      double *rtol, double *atol, int *idid,
			      double *rwork, int *lrw, int *iwork, int *liw,
			      double *rpar, int *ipar, dassl_jac jac);
extern int nsp_ode_ddatrp (double *x, double *xout, double *yout,
			      double *ypout, int *neq, int *kold, double *phi,
			      double *psi);
extern int nsp_ode_ddawts (int *neq, int *iwt, double *rtol, double *atol,
			      double *y, double *wt, double *rpar, int *ipar);
extern int nsp_ode_dgbfa (double *abd, int *lda, int *n, int *ml, int *mu,
			     int *ipvt, int *info);
extern int nsp_ode_dgbsl (double *abd, int *lda,const int *n,const int *ml,const int *mu,
			     int *ipvt, double *b,const int *job);
extern int nsp_dgefa (double *a, int *lda, int *n, int *ipvt,
		      int *info);
extern int nsp_dgesl (double *a, int *lda, int *n, int *ipvt,
		      double *b, int *job);

extern int
nsp_ode_ewset (const int *n,const int *itol, double *rtol,const double *atol,
	       const double *ycur, double *ewt);

extern double nsp_ode_fnorm (int *n, double *a, double *w);

extern int nsp_ode_intdy (double *t, int *k, double *yh, int *nyh,
			     double *dky, int *iflag);

extern int nsp_ode_lsdisc (ode_f f, int *neq, double *y, double *t,
			      double *tout, double *rwork, int *lrw,
			      int *istate, void *param);

extern int nsp_ode_lsoda (ode_f f, int *neq, double *y, double *t,
			  double *tout, int *itol, double *rtol,
			  const double *atol, int *itask, int *istate, int *iopt,
			  double *rwork, int *lrw, int *iwork, int *liw,
			  ode_jac jac, int *jt, void *param);

extern int nsp_ode_lsodar (ode_f f, int *neq, double *y, double *t, double *tout,
			      int *itol, double *rtol, double *atol, int *itask,
			      int *istate, int *iopt, double *rwork, int *lrw,
			      int *iwork, int *liw, ode_jac jac, int *jt, lsodar_g g, int *ng,
			      int *jroot, double *param);

extern int nsp_ode_lsode (ode_f f, int *neq, double *y, double *t, double *tout,
			     int *itol, double *rtol,const double *atol, int *itask,
			     int *istate, int *iopt, double *rwork, int *lrw, int *iwork,
			     int *liw, ode_jac jac, int *mf, void *param);

extern int nsp_ode_prja (int *neq, double *y, double *yh, int *nyh,
			    double *ewt, double *ftem, double *savf,
			    double *wm, int *iwm, ode_f f, ode_jac jac, void *param);

extern int nsp_ode_rchek (int *job, lsodar_g g, int *neq, double *y,
			     double *yh, int *nyh, double *g0, double *g1,
			     double *gx, int *jroot, int *irt, double *param);
extern int nsp_ode_rchek2 (int *job, lsodar_g g, int *neq, double *y,
			      double *yh, int *nyh, double *g0, double *g1,
			      double *gx, int *jroot, int *irt, int *iwork, double *param);

extern int nsp_ode_roots (int *ng, double *hmin, int *jflag, double *x0,
			     double *x1, double *g0, double *g1, double *gx,
			     double *x, int *jroot);
extern int nsp_ode_roots2 (int *ng, double *hmin, int *jflag, double *x0,
			      double *x1, double *g0, double *g1, double *gx,
			      double *x, int *jroot);
extern int nsp_ode_rscar1 (double *rsav, double *isav);
extern int nsp_ode_rscma1 (double *rsav, double *isav);
extern int nsp_ode_rscom1 (double *rsav, double *isav);
extern int nsp_ode_solsy (double *wm, int *iwm, double *x, double *tem);

extern int nsp_ode_stoda (int *neq, double *y, double *yh, int *nyh, double *yh1,
			     double *ewt, double *savf, double *acor, double *wm,
			     int *iwm, ode_f f, ode_jac jac, Pjac1 pjac, Slvs slvs,
			     void *param);

extern int nsp_ode_stode (int *neq, double *y, double *yh, int *nyh,
			     double *yh1, double *ewt, double *savf,
			     double *acor, double *wm, int *iwm, ode_f f,
			     ode_jac jac, Pjac1 pjac,Slvs slvs, void *param);

extern int nsp_ode_svcar1 (double *rsav, double *isav);
extern int nsp_ode_svcma1 (double *rsav, double *isav);
extern int nsp_ode_svcom1 (double *rsav, double *isav);

extern int nsp_ode_greatr (double *a, double *b, int *nwds);

extern double nsp_ode_vmnorm (const int *n,const double *v,const double *w);
extern double nsp_ode_vnorm ( const int *n,const double *v,const  double *w);
/* extern double nsp_ode_dlamch (char *, long int); */
extern int nsp_ode_dcopy (int *, double *, int *,  double *, int *);
extern int nsp_ode_dscal (int *, double *, double *, int *);
extern double nsp_ode_ddot (int *, double *, int *, double *, int *);
extern int nsp_ode_daxpy (int *, double *, double *, int *, double *, int *);
extern double nsp_ode_ddanrm (int *, double *, double *, double *, int *);
extern int nsp_ode_droots (int *, double *, int *, double *, double *, double *,
			      double *, double *, double *, int *, int *, int *,
			      double *, double *);
extern double nsp_ode_ddanrm (int *, double *, double *, double *, int *);
extern int nsp_ode_ddaslv (int *, double *, double *,   int *);
extern double nsp_ode_ddanrm (int *, double *, double *, double *, int *);
extern int nsp_ode_xermsg (char *, char *, char *, int *, int *, long int, long int,  long int);
extern double nsp_ode_ddanrm (int *, double *, double *, double *, int *);
extern int nsp_ode_ddaslv (int *, double *, double *,    int *);
extern int nsp_ode_idamax (int *, double *, int *);
extern double nsp_ode_bnorm (const int *n,const double *a,const int *nra,
				const int *ml,const int *mu, const double *w);

extern int
nsp_ode_ddastp (double *x, double *y, double *yprime, int *neq, dassl_res res,
		   dassl_jac jac, double *h__, double *wt, int *jstart, int *idid,
		   double *rpar, int *ipar, double *phi, double *delta,
		   double *e, double *wm, int *iwm, double *alpha,
		   double *beta, double *gamma, double *psi, double *sigma,
		   double *cj, double *cjold, double *hold, double *s,
		   double *hmin, double *uround, int *iphase, int *jcalc,
		   int *k, int *kold, int *ns, int *nonneg, int *ntemp);

/* mini fortran utilities 
 */

extern double d_sign(const double *a, const double *b);
extern int i_sign(const int *a,const  int *b);

#define s_copy(str1,str2,l1,l2) strncpy(str1,str2,l1)

/* shared data. 
 */


typedef union {
  struct 
  {
    double tret, rowns[209], ccmax, el0, h__, hmin, hmxi, hu, rc, tn, uround;
    int illin, init, lyh, lewt, lacor, lsavf, lwm, liwm, mxstep, mxhnil, nhnil,
      ntrep, nslast, nyh, iowns[6], icf, ierpj, iersl, jcur, jstart, kflag, l,
      meth, miter, maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu;
  } _1;
  struct
  {
    double rownd, conit, crate, el[13], elco[13*12] , hold,
      rmax, tesco[3*12], ccmax, el0, h__, hmin, hmxi, hu, rc,
      tn, uround;
    int iownd[14], ialth, ipup, lmax, meo, nqnyh, nslp, icf, ierpj, iersl, jcur,
      jstart, kflag, l, meth, miter, maxord, maxcor, msbp, mxncf, n, nq, nst,
      nfe, nje, nqu;
  } _2;
} ode_ls0001; 


extern ode_ls0001 ls0001_;

/* used to save previous structure */

typedef struct __ls0001 ls0001;

struct __ls0001 
{
  double rls[219];
  int ils[39];
};


typedef union {
  struct
  {
    double rownr3[2], t0, tlast, toutc;
    int lg0, lg1, lgx, iownr3[2], irfnd, itaskc, ngc, nge;
  } _1 ;
  struct
  {
    double alpha, x2, rdum3[3];
    int iownd3[3], imax, last, idum3[4];
  } _2;
  struct
  {
    double rlsr[5];
    int ilsr[9];
  } _3;
} ode_lsr;

extern ode_lsr lsr001_;


typedef union {
  struct
  {
    double tsw, rowns2[20], pdnorm;
    int insufr, insufi, ixpr, iowns2[2], jtyp, mused, mxordn, mxords;
  } _1;
  struct
  {
    double rownd2, pdest, pdlast, ratio, cm1[12], cm2[5], pdnorm;
    int iownd2[3], icount, irflag, jtyp, mused, mxordn, mxords;
  } _2;
  struct
  {
    double rlsa[22];
    int ilsa[9];
  } _3;
} ode_lsa;

extern ode_lsa lsa001_;


/* used to transmit errors to caller  */

typedef struct _ode_err ode_err;
struct _ode_err
{
  int iero;
};

/* export this symbol */
INLIBNSP ode_err ierode_;
#define ierode_1 C2F(ierode)

typedef struct _ode_callerid ode_callerid;
struct _ode_callerid 
{
  int fcallerid;
};

extern ode_callerid callerid_;
#define callerid_1 callerid_

#endif 
