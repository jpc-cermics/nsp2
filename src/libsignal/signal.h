#ifndef _signal_H
#define _signal_H

#include "nsp/object.h"
#include "nsp/blas.h"
#include "nsp/lapack-c.h"
#include "nsp/matutil.h"
#include "../libcalelm/calpack.h"
#include "../libcontrol/ctrlpack.h"

typedef int (*F_dget) (double *, int *, int *);

extern int signal_recbez (double *p1, int *n1, double *p2, int *n2, double *best,
			   int *ipb, double *w, double *err);

extern int signal_sfact1 (double *b,const int *n, double *w,const int *maxit, int *ierr);

extern int signal_sfact2 (double *b,const int *l, const int *n, double *matg,const int *maxit, int *ierr);


extern void signal_conv2_separable_R(double *R, int nR, double *C, int mC, double *A, int mA, int nA, double *Out, int mOut, int nOut, int edgM, int edgN, double *T);
extern void signal_conv2_separable_C(double *Rr, double *Ri, int nR, double *Cr, double *Ci, int mC, double *Ar, double *Ai, int mA, int nA, double *Outr, double *Outi, int mOut, int nOut, int edgM, int edgN, double *Tr, double *Ti);
extern void signal_conv2_R(double *A, int mA, int nA, double *B, int mB, int nB, double *Out, int mOut, int nOut, int edgM, int edgN);
extern void signal_conv2_C(double *Ar, double *Ai, int mA, int nA, double *Br, double *Bi, int mB, int nB, double *Outr, double *Outi, int mOut, int nOut, int edgM, int edgN);

extern int signal_transn (int *ityp, double *om, int *norma, double *vsn,
			  double *vd, double *a);

extern int signal_dpsimp (double *a,const int *na,double *b,const int *nb,
			  double *a1, int *na1, double *b1, int *nb1, double *w, int *ierr);

extern int signal_cmpse2 (int *m, int *n, int *mode, F_dget * dgetx, F_dget * dgety,
			  double *xa, double *xr, double *xi, double *zr, double *zi,
			  int *ierr);

extern int signal_amell (double *du, double *dk, double *dsn2, int *n);
extern int signal_bounn (double *adeg, double *acap12, double *vsn);
extern int signal_cheby (int *ordr, int *demi, int *ieo, double *dp,
			 double *x0, double *tam, double *win);

extern int signal_cmpse3 (int *m, int *n, int *mode, double *x, double *y,
			  double *xr, double *xi, double *zr, double *zi,
			  int *ierr, int *ichaud, int *nbx);
extern int signal_coeft (int *ordre, double *poler, double *polei,
			 double *gain);
extern int signal_compel (const double *dk, double *dellk);
extern int signal_degree (int *iapro, double *vsn, double *acap12,
			  double *adeg);

extern int signal_deli11 (const double *x,const double *ck, double *res);
extern int signal_deli1 (int *n, double *resv, double *xv, double *ck);

extern int signal_deli2 (const int *nn, double *resv,const double *xxv,const double *ck);
  
extern int signal_delip (const int *n, double *resr, double *resi, const double *x,
			 const double *ck);

extern double signal_dellk (double *dk);
extern int signal_desi00 (int *ityp, double *om, int *norma, double *edeg,
			  int *ndeg, double *adeg, double *vsn, double *vd,
			  double *a);
extern int signal_desi01 (int *maxdeg, int *iapro, double *edeg, int *ndeg,
			  double *adelp, double *adels, double *vsn,
			  double *adeg, double *gd1, double *gd2,
			  double *acap12, int *ierr);
extern int signal_desi11 (int *nmaxi, int *maxdeg, double *vsn, int *ndeg,
			  double *gd1, double *gd2, double *adelta, int *nzm,
			  double *sm, int *nzero, double *pren, double *pimn,
			  double *ugc, double *ogc, int *nj, int *nh);
extern int signal_desi12 (int *nmaxi, int *maxdeg, int *iapro, int *ndeg,
			  double *vsn, double *gd1, double *gd2,
			  double *adelta, int *nzm, double *sm, int *nzero,
			  double *pren, double *pimn, double *ugc,
			  double *ack, int *nj, int *nh);
extern int signal_desi14 (int *nmaxi, int *maxdeg, int *ndeg, double *vsn,
			  double *gd1, double *gd2, double *adelta, int *nzm,
			  double *sm, int *nzero, double *dsk, double *ugc,
			  double *ogc, double *ack, int *nj, int *nh,
			  double *dk, double *dks, double *dcap02,
			  double *dcap04);
extern int signal_desi21 (int *ndeg, double *adelp, double *adels,
			  double *adelta, double *pren, double *pimn,
			  double *ugc, double *ogc, int *nj, double *acx,
			  double *ac, double *rdelp, double *rdels,
			  double *sfa, double *spr, double *spi);
extern int signal_desi22 (int *iapro, int *ndeg, double *adelp, double *adels,
			  double *adelta, double *vsn, double *pren,
			  double *pimn, double *ugc, double *ogc, double *ack,
			  int *nj, int *nh, double *acx, double *ac,
			  double *rdels, double *sfa, double *spr,
			  double *spi);
extern int signal_desi24 (int *ndeg, double *adelp, double *adels,
			  double *adelta, double *dsk, double *ugc,
			  double *ogc, double *ack, int *nj, int *nh,
			  double *dk, double *dks, double *dcap02,
			  double *dcap04, double *acx, double *ac,
			  double *rdelp, double *rdels, double *sfa,
			  double *spr, double *spi);
extern int signal_desia (int *nmaxi, int *maxdeg, int *ityp, int *iapro,
			 double *om, int *norma, double *edeg, int *ndeg,
			 double *adelp, double *adels, int *nbn, int *nzero,
			 int *nzm, double *vsn, double *a, double *adelta,
			 double *adeg, double *sm, double *pren, double *pimn,
			 double *ugc, double *ogc, double *ack, double *zm,
			 double *zzr, double *zzi, double *rom, double *b2,
			 double *b1, double *b0, double *dk, double *dks,
			 double *dcap02, double *dcap04, double *vsnn,
			 int *ndegn, int *nh, double *vd, int *nze,
			 int *ierr);
extern int signal_desib (int *nmaxi, int *maxdeg, double *vsnn, int *ndegn,
			 int *nbn, int *ityp, int *iapro, double *om, int *nh,
			 double *adelp, double *adels, double *vd, double *a,
			 double *adelta, double *pren, double *pimn,
			 double *ugc, double *ogc, double *ack, double *dk,
			 double *dks, double *dcap02, double *dcap04,
			 double *acx, double *spr, double *spi, double *zpr,
			 double *zpi, int *nb, double *fact, double *c1,
			 double *c0, double *sm, int *ierr, int *ndeg);

extern int signal_dfft2 (double *a, double *b, int *nseg, int *n, int *nspn,
			 int *isn, int *ierr, int *iw, int *lw);

extern int signal_dfftbi (double *a, double *b, int *nseg, int *n, int *nspn,
			  int *isn, int *ierr, int *lout, int *lnow,
			  int *lused, int *lmax, int *lbook, double *rstak,
			  int *istak);
extern int signal_dfftmx (double *a, double *b, int *ntot, int *n, int *nspan,
			  int *isn, int *m, int *kt, double *wt, double *ck,
			  double *bt, double *sk, int *np, int *nfac);

extern double signal_dsn2 (const double *du,const double *dk,const double *dq);
extern int signal_filbut (double *fmin, double *fmax, int *atmin, int *atmax,
			  int *ordre, double *fc, double *gain, double *poler,
			  double *polei, int *err);
extern int signal_freque (double *fmin, double *fmax, int *atmin, int *atmax,
			  int *ordre, double *fc, int *err);
extern int signal_hammin (int *ordr, int *demi, int *ieo, double *alph,
			  double *win);
extern double signal_ino (double *x);
extern int signal_nstabl (double *a, int *n, double *w, int *ist);
extern int signal_parcha (int *iapro, double *adeg, double *adelp,
			  double *adels, double *vsn, double *gd1,
			  double *gd2, double *acap12);

extern int signal_poles (int *ordre, double *fc, double *poler,
			 double *polei);

extern int signal_remez (int *ngr, int *nfc, int *iext, double *ad, double *x,
			 double *y, double *des, double *grid, double *wt,
			 double *a, double *p, double *q, double *alpha);

extern int signal_romeg (const int *nmaxi,const int *maxdeg,const int *ityp,const int *nzm,
			 const double *zm, double *rom);

extern int signal_rpem (double *theta, double *p, int *n, double *u,
			double *y, double *lambda, double *k, double *c__,
			int *istab2, double *v, double *eps, double *eps1,
			int *idim, double *fi, double *psi, double *tstab,
			double *work, double *f, double *g, double *l);

extern int signal_snell (double *dsn2, double *du, double *dk, double *dq);
extern double signal_sn (double *y, double *a, double *ak1, double *ak3);
extern int signal_syredi (int *maxdeg, int *ityp, int *iapro, double *om,
			  double *adelp, double *adels, int *ndeg, int *nb,
			  double *fact, double *b2, double *b1, double *b0,
			  double *c1, double *c0, double *zzr, double *zzi,
			  double *zpr, double *zpi, int *ierr, double *spr,
			  double *spi, double *pren, double *pimn, double *zm,
			  double *sm, double *rom, int *nzero, int *nze);
extern int signal_tg02_ad (int *ix, int *n, double *u, double *s, double *d__,
			   double *x, double *v);
extern int signal_tscccf (const double *x,const double *y,const int *n, double *cxy,
			  double *xymean,const int *lag, int *ierr);

extern double signal_coshin (double *);
extern int signal_fft842 (const int *,const int *, double *, double *, int *);
extern double signal_dellk (double *);
extern double signal_arcosh (double *);
extern double signal_arsinh (double *);
extern int signal_ouch (void);
extern int signal_nstabl (double *, int *, double *, int *);
#endif
