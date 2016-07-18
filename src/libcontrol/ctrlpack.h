#ifndef CTRLPACK_H
#define CTRLPACK_H

#include "nsp/object.h" 
#include "nsp/blas.h" 
#include "nsp/lapack-c.h" 
#include "nsp/matutil.h" 
#include "../libcalelm/calpack.h"

typedef struct _pade_data pade_data;

struct _pade_data 
{
  double b[41];
  int n;
};

extern pade_data pade_dcoeff;

typedef  int  (*ct_Ftest) (int *ls, double *alpha, double *beta, double *s, double *p);

#define pow_di nsp_pow_di 

#define U_fp void *
#define intg_f void *
#define S_fp void *

extern int nsp_ctrlpack_arl2a(double *f, int *nf, double *ta, int *mxsol, int *imina, int *nall, int *inf, int *ierr, int *ilog, double *w, int *iw);
extern int nsp_ctrlpack_arl2(double *f, int *nf, double *num, double *tq, int *dgmin, int *dgmax, double *errl2, double *w, int *iw, int *inf, int *ierr, int *ilog);
extern int nsp_ctrlpack_lq (const int *nq, const double *tq, double *tr, double *tg,const int *ng);
extern double nsp_ctrlpack_phi (double *tq,const int *nq, double *tg,const int *ng, double *w);
extern int nsp_ctrlpack_dpodiv(double *a, const double *b,const int *na,const int *nb);

extern int nsp_ctrlpack_balanc(int *nm, int *n, double *a, int *low, int *igh, double *scale);
extern int nsp_ctrlpack_balbak(int *nm, int *n, int *low, int *igh, double *scale, int *m, double *z__);
extern int nsp_ctrlpack_bdiag(int *lda, int *n, double *a, double *epsshr, double *rmax, double *er, double *ei, int *bs, double *x, double *xi, double *scale, int *job, int *fail);
extern int nsp_ctrlpack_bezous(double *a, int *n, double *c__, double *w, int *ierr);
extern int nsp_ctrlpack_calcsc(int *type__);
extern int nsp_ctrlpack_cbal(int *nm, int *n, double *ar, double *ai, int *low, int *igh, double *scale);
extern int nsp_ctrlpack_cerr(double *a, double *w, int *ia, int *n, int *ndng, int *m, int *maxc);
extern int nsp_ctrlpack_coef(int *ierr);
extern int nsp_ctrlpack_comqr3(int *nm, int *n, int *low, int *igh, double *hr, double *hi, double *wr, double *wi, double *zr, double *zi, int *ierr, int *job);
extern int nsp_ctrlpack_corth(int *nm, int *n, int *low, int *igh, double *ar, double *ai, double *ortr, double *orti);
extern int nsp_ctrlpack_cortr(int *nm, int *n, int *low, int *igh, double *hr, double *hi, double *ortr, double *orti, double *zr, double *zi);
extern int nsp_ctrlpack_dclmat(int *ia, int *n, double *a, double *b, int *ib, double *w, double *c__, int *ndng);
extern int nsp_ctrlpack_deg1l2(double *tg, int *ng, int *imin, double *ta, int *mxsol, double *w, int *iw, int *ierr);
extern int nsp_ctrlpack_degl2(double *tg, int *ng, int *neq, int *imina, int *iminb, int *iminc, double *ta, double *tb, double *tc, int *iback, int *ntback, double *tback, int *mxsol, double *w, int *iw, int *ierr);
extern int nsp_ctrlpack_dexpm1(int *ia, int *n, double *a, double *ea, int *iea, double *w, int *iw, int *ierr);
extern int nsp_ctrlpack_dfrmg(int *job, int *na, int *nb, int *nc, int *l, int *m, int *n, double *a, double *b, double *c__, double *freqr, double *freqi, double *gr, double *gi, double *rcond, double *w, int *ipvt);
extern int nsp_ctrlpack_dgbfa(double *abd, int *lda, int *n, int *ml, int *mu, int *ipvt, int *info);
extern int nsp_ctrlpack_dgbsl(double *abd, int *lda, int *n, int *ml, int *mu, int *ipvt, double *b, int *job);
extern int nsp_ctrlpack_dgeco(double *a, int *lda, int *n, int *ipvt, double *rcond, double *z__);
extern int nsp_ctrlpack_dgedi(double *a, int *lda, int *n, int *ipvt, double *det, double *work, int *job);
extern int nsp_ctrlpack_dgefa(double *a, int *lda, int *n, int *ipvt, int *info);
extern int nsp_ctrlpack_dgesl(double *a, int *lda, int *n, int *ipvt, double *b, int *job);
extern int nsp_ctrlpack_dhetr(int *na, int *nb, int *nc, int *l, int *m, int *n, int *low, int *igh, double *a, double *b, double *c__, double *ort);
extern int nsp_ctrlpack_dlslv(double *a, int *na, int *n, double *b, int *nb, int *m, double *w, double *rcond, int *ierr, int *job);
extern int nsp_ctrlpack_dpofa(double *a, int *lda, int *n, int *info);
extern int nsp_ctrlpack_dqrdc(double *x, int *ldx, int *n, int *p, double *qraux, int *jpvt, double *work, int *job);
extern int nsp_ctrlpack_dqrsl(double *x, int *ldx, int *n, int *k, double *qraux, double *y, double *qy, double *qty, double *b, double *rsd, double *xb, int *job, int *info);
extern int nsp_ctrlpack_dqrsm(double *x, int *ldx, int *n, int *p, double *y, int *ldy, int *nc, double *b, int *ldb, int *k, int *jpvt, double *qraux, double *work);
extern int nsp_ctrlpack_drref(double *a, int *lda, int *m, int *n, double *eps);


extern int nsp_ctrlpack_dsubsp(int *nmax, int *n, double *a, double *b, double *z__, ct_Ftest ftest, double *eps, int *ndim, int *fail, int *ind);
extern int nsp_ctrlpack_dsvdc(double *x, int *ldx, int *n, int *p, double *s, double *e, double *u, int *ldu, double *v, int *ldv, double *work, int *job, int *info);
extern int nsp_ctrlpack_dzdivq(int *ichoix, int *nv, double *tv, int *nq, double *tq);
extern int nsp_ctrlpack_ereduc(double *e, int *m, int *n, double *q, double *z__, int *istair, int *ranke, double *tol);
extern int nsp_ctrlpack_exch(int *nmax, int *n, double *a, double *z__, int *l, int *ls1, int *ls2);
extern int nsp_ctrlpack_exchqz(int *nmax, int *n, double *a, double *b, double *z__, int *l, int *ls1, int *ls2, double *eps, int *fail);

extern int nsp_ctrlpack_expan (const double *a,const int *la, const double *b,
			       const int *lb, double *c, const int *nmax);

extern int nsp_ctrlpack_find(int *lsize, double *alpha, double *beta, double *s, double *p);
extern int nsp_ctrlpack_folhp(int *ls, double *alpha, double *beta, double *s, double *p);
extern int nsp_ctrlpack_fout(int *lsize, double *alpha, double *beta, double *s, double *p);
extern int nsp_ctrlpack_fstair(double *a, double *e, double *q, double *z__, int *m, int *n, int *istair, int *ranke, double *tol, int *nblcks, int *imuk, int *inuk, int *imuk0, int *inuk0, int *mnei, double *wrk, int *iwrk, int *ierr);
extern int nsp_ctrlpack_squaek(double *a, int *lda, double *e, double *q, int *ldq, double *z__, int *ldz, int *m, int *n, int *nblcks, int *inuk, int *imuk, int *mnei);
extern int nsp_ctrlpack_triaak(double *a, int *lda, double *e, double *z__, int *ldz, int *n, int *nra, int *nca, int *ifira, int *ifica);
extern int nsp_ctrlpack_triaek(double *a, int *lda, double *e, double *q, int *ldq, int *m, int *n, int *nre, int *nce, int *ifire, int *ifice, int *ifica);
extern int nsp_ctrlpack_trired(double *a, int *lda, double *e, double *q, int *ldq, double *z__, int *ldz, int *m, int *n, int *nblcks, int *inuk, int *imuk, int *ierr);
extern int nsp_ctrlpack_bae(double *a, int *lda, double *e, double *q, int *ldq, double *z__, int *ldz, int *m, int *n, int *istair, int *ifira, int *ifica, int *nca, int *rank, double *wrk, int *iwrk, double *tol);
extern int nsp_ctrlpack_dgiv(double *da, double *db, double *dc, double *ds);
extern int nsp_ctrlpack_drot(int *n, double *x, int *incx, double *y, int *incy, double *c__, double *s);
extern int nsp_ctrlpack_fxshfr(int *l2, int *nz);
extern int nsp_ctrlpack_giv(double *sa, double *sb, double *sc, double *ss);
extern int nsp_ctrlpack_hl2(int *nq, double *tq, double *tg, int *ng, double *pd, int *nrowpd, double *tr, double *tp, double *tv, double *tw, double *tij, double *d1aux, double *d2aux, int *maxnv, int *maxnw);
extern int nsp_ctrlpack_hhdml(int *ktrans, int *nrowa, int *ncola, int *ioff, int *joff, int *nrowbl, int *ncolbl, double *x, int *nx, double *qraux, double *a, int *na, int *mode, int *ierr);
extern int nsp_ctrlpack_hqror2(int *nm, int *n, int *low, int *igh, double *h__, double *wr, double *wi, double *z__, int *ierr, int *job);
extern int nsp_ctrlpack_cdiv(double *ar, double *ai, double *br, double *bi, double *cr, double *ci);
extern int nsp_ctrlpack_htribk(int *nm, int *n, double *ar, double *ai, double *tau, int *m, double *zr, double *zi);
extern int nsp_ctrlpack_htridi(int *nm, int *n, double *ar, double *ai, double *d__, double *e, double *e2, double *tau);
extern int nsp_ctrlpack_imtql3(int *nm, int *n, double *d__, double *e, double *z__, int *ierr, int *job);
extern int nsp_ctrlpack_inva(int *nmax, int *n, double *a, double *z__, ct_Ftest ftest, double *eps, int *ndim, int *fail, int *ind);
extern int nsp_ctrlpack_invtpl(double *t, int *n, int *m, double *tm1, int *ierr);
extern int nsp_ctrlpack_irow1(int *i__, int *m);
extern int nsp_ctrlpack_irow2(int *i__, int *m);
extern int nsp_ctrlpack_lrow2(int *i__, int *m);
extern int nsp_ctrlpack_lybad(int *n, double *a, int *na, double *c__, double *x, double *u, double *eps, double *wrk, int *mode, int *ierr);
extern int nsp_ctrlpack_lybsc(int *n, double *a, int *na, double *c__, double *x, double *u, double *eps, double *wrk, int *mode, int *ierr);
extern int nsp_ctrlpack_lycsr(int *n, double *a, int *na, double *c__, int *ierr);
extern int nsp_ctrlpack_lydsr(int *n, double *a, int *na, double *c__, int *ierr);
extern int nsp_ctrlpack_newest(int *type__, double *uu, double *vv);
extern int nsp_ctrlpack_nextk(int *type__);
extern int nsp_ctrlpack_onface(int *nq, double *tq, double *tg, int *ng, int *nprox, int *ierr, double *w);
extern int nsp_ctrlpack_orthes(int *nm, int *n, int *low, int *igh, double *a, double *ort);
extern int nsp_ctrlpack_ortran(int *nm, int *n, int *low, int *igh, double *a, double *ort, double *z__);
extern int nsp_ctrlpack_pade(double *a, int *ia, int *n, double *ea, int *iea, double *alpha, double *wk, int *ipvt, int *ierr);
extern int nsp_ctrlpack_polmc(int *nm, int *ng, int *n, int *m, double *a, double *b, double *g, double *wr, double *wi, double *z__, int *inc, int *invr, int *ierr, int *jpvt, double *rm1, double *rm2, double *rv1, double *rv2, double *rv3, double *rv4);
extern int nsp_ctrlpack_proj2(double *f, int *nn, double *am, int *n, int *np1, int *np2, double *pf, double *w);

extern int
nsp_ctrlpack_qhesz (const int *nm,const int *n, double *a, double *b, const int *matq,
		    double *q, const int *matz, double *z__);

int
nsp_ctrlpack_qitz (const int *nm, const int *n, double *a, double *b, double *eps1,
		   const int *matq, double *q,const int *matz, double *z__, int *ierr);

extern int nsp_ctrlpack_quadit(double *uu, double *vv, int *nz);
extern int nsp_ctrlpack_quad(double *a, double *b1, double *c__, double *sr, double *si, double *lr, double *li);
extern int nsp_ctrlpack_quadsd(int *nn, double *u, double *v, double *p, double *q, double *a, double *b);

extern int
nsp_ctrlpack_qvalz (const int *nm, const int *n, double *a, double *b, const double *epsb,
		    double *alfr, double *alfi, double *beta, const int *matq,
		    double *q, const int *matz, double *z__);

extern int nsp_ctrlpack_qvecz(int *nm, int *n, double *a, double *b, double *epsb, double *alfr, double *alfi, double *beta, double *z__);
extern int nsp_ctrlpack_qzk(double *q, double *a, int *n, int *kmax, double *c__);
extern int nsp_ctrlpack_realit(double *sss, int *nz, int *iflag);
extern int nsp_ctrlpack_reduc2(int *n, int *ma, double *a, int *mb, double *b, int *low, int *igh, double *cscale, double *wk);
extern int nsp_ctrlpack_dlald2(int *ltran, double *t, int *ldt, double *b, int *ldb, double *scale, double *x, int *ldx, double *xnorm, int *info);
extern int nsp_ctrlpack_dlaly2(int *ltran, double *t, int *ldt, double *b, int *ldb, double *scale, double *x, int *ldx, double *xnorm, int *info);
extern int nsp_ctrlpack_dlasd2(int *ltranl, int *ltranr, int *isgn, int *n1, int *n2, double *tl, int *ldtl, double *tr, int *ldtr, double *b, int *ldb, double *scale, double *x, int *ldx, double *xnorm, int *info);
extern int nsp_ctrlpack_lypcfr(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *t, int *ldt, double *u, int *ldu, double *x, int *ldx, double *scale, double *ferr, double *work, int *lwork, int *iwork, int *info, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_lypcrc(char *fact, char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *t, int *ldt, double *u, int *ldu, double *x, int *ldx, double *scale, double *rcond, double *work, int *lwork, int *iwork, int *info, long int fact_len, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_lypcsl(char *fact, char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *t, int *ldt, double *u, int *ldu, double *wr, double *wi, double *x, int *ldx, double *scale, double *rcond, double *ferr, double *work, int *lwork, int *iwork, int *info, long int fact_len, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_lypctr(char *trana, int *n, double *a, int *lda, double *c__, int *ldc, double *scale, int *info, long int trana_len);
extern int nsp_ctrlpack_lypdfr(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *t, int *ldt, double *u, int *ldu, double *x, int *ldx, double *scale, double *ferr, double *work, int *lwork, int *iwork, int *info, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_lypdrc(char *fact, char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *t, int *ldt, double *u, int *ldu, double *x, int *ldx, double *scale, double *rcond, double *work, int *lwork, int *iwork, int *info, long int fact_len, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_lypdsl(char *fact, char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *t, int *ldt, double *u, int *ldu, double *wr, double *wi, double *x, int *ldx, double *scale, double *rcond, double *ferr, double *work, int *lwork, int *iwork, int *info, long int fact_len, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_lypdtr(char *trana, int *n, double *a, int *lda, double *c__, int *ldc, double *scale, double *work, int *info, long int trana_len);
extern int nsp_ctrlpack_riccfr(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *d__, int *ldd, double *x, int *ldx, double *t, int *ldt, double *u, int *ldu, double *ferr, double *work, int *lwork, int *iwork, int *info, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_riccmf(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *d__, int *ldd, double *x, int *ldx, double *wr, double *wi, double *rcond, double *ferr, double *work, int *lwork, int *iwork, int *info, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_riccms(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *d__, int *ldd, double *x, int *ldx, double *wr, double *wi, double *rcond, double *ferr, double *work, int *lwork, int *iwork, int *info, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_riccrc(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *d__, int *ldd, double *x, int *ldx, double *rcond, double *t, int *ldt, double *u, int *ldu, double *wr, double *wi, double *work, int *lwork, int *iwork, int *info, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_riccsl(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *d__, int *ldd, double *x, int *ldx, double *wr, double *wi, double *rcond, double *ferr, double *work, int *lwork, int *iwork, int *bwork, int *info, long int trana_len, long int uplo_len);

extern int nsp_ctrlpack_selneg(const double *wr,const double *wi);

extern int nsp_ctrlpack_ricdfr(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *x, int *ldx, double *ac, int *ldac, double *t, int *ldt, double *u, int *ldu, double *wferr, double *ferr, double *work, int *lwork, int *iwork, int *info, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_ricdmf(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *d__, int *ldd, double *x, int *ldx, double *wr, double *wi, double *rcond, double *ferr, double *work, int *lwork, int *iwork, int *info, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_ricdrc(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *d__, int *ldd, double *x, int *ldx, double *rcond, double *ac, int *ldac, double *t, int *ldt, double *u, int *ldu, double *wr, double *wi, double *wferr, double *work, int *lwork, int *iwork, int *info, long int trana_len, long int uplo_len);
extern int nsp_ctrlpack_ricdsl(char *trana, int *n, double *a, int *lda, char *uplo, double *c__, int *ldc, double *d__, int *ldd, double *x, int *ldx, double *wr, double *wi, double *rcond, double *ferr, double *work, int *lwork, int *iwork, int *bwork, int *info, long int trana_len, long int uplo_len);

extern int nsp_ctrlpack_selmlo(const double *alphar,const double *alphai,const double *beta);

extern int nsp_ctrlpack_ricd(int *nf, int *nn, double *f, int *n, double *h__, double *g, double *cond, double *x, double *z__, int *nz, double *w, double *eps, int *ipvt, double *wrk1, double *wrk2, int *ierr);
extern int nsp_ctrlpack_rilac(int *n, int *nn, double *a, int *na, double *c__, double *d__, double *rcond, double *x, double *w, int *nnw, double *z__, double *eps, int *iwrk, double *wrk1, double *wrk2, int *ierr);
extern int nsp_ctrlpack_rootgp(int *ngp, double *gpp, int *nbeta, double *beta, int *ierr, double *w);
extern int nsp_ctrlpack_rpoly (const double *op, int *degree, double *zeror, double *zeroi, int *fail);
extern int nsp_ctrlpack_rtitr(int *nin, int *nout, int *nu, double *num, int *inum, int *dgnum, double *den, int *iden, int *dgden, double *up, double *u, int *iu, double *yp, double *y, int *iy, int *job, int *iw, double *w, int *ierr);
extern int nsp_ctrlpack_scaleg(int *n, int *ma, double *a, int *mb, double *b, int *low, int *igh, double *cscale, double *cperm, double *wk);
extern int nsp_ctrlpack_shrslv(double *a, double *b, double *c__, int *m, int *n, int *na, int *nb, int *nc, double *eps, double *cond, double *rmax, int *fail);
extern int nsp_ctrlpack_split(double *a, double *v, int *n, int *l, double *e1, double *e2, int *na, int *nv);
extern int nsp_ctrlpack_ssxmc(int *n, int *m, double *a, int *na, double *b, int *ncont, int *indcon, int *nblk, double *z__, double *wrka, double *wrk1, double *wrk2, int *iwrk, double *tol, int *mode);
int
nsp_ctrlpack_sszer (const int *n,const int *m,const int *p, double *a,const int *na, double *b,
		    double *c__, const int *nc, double *d__, const double *deps,
		    double *zeror, double *zeroi, int *nu, int *irank,
		    double *af, const int *naf, double *bf, const int *mplusn,
		    double *wrka, double *wrk1, const int *nwrk1, double *wrk2,
		    const int *nwrk2, int *ierr);

extern int nsp_ctrlpack_storl2(int *neq, double *tq, double *tg, int *ng, int *imin, double *tabc, int *iback, int *ntback, double *tback, int *nch, int *mxsol, double *w, int *ierr);
extern int nsp_ctrlpack_sybad(int *n, int *m, double *a, int *na, double *b, int *nb, double *c__, int *nc, double *u, double *v, double *eps, double *wrk, int *mode, int *ierr);
extern int nsp_ctrlpack_sydsr(int *n, int *m, double *a, int *na, double *b, int *nb, double *c__, int *nc, int *ierr);
extern int nsp_ctrlpack_syhsc(int *n, int *m, double *a, int *na, double *b, int *mb, double *c__, double *z__, double *eps, double *wrk1, int *nwrk1, double *wrk2, int *nwrk2, int *iwrk, int *niwrk, int *ierr);
extern int nsp_ctrlpack_transf(double *a, double *ort, int *it1, double *c__, double *v, int *it2, int *m, int *n, int *mdim, int *ndim, double *d__, int *nwrk1);
extern int nsp_ctrlpack_nsolve(double *a, double *b, double *c__, double *d__, int *nwrk1, int *ndim, int *n, int *mdim, int *m, int *ind, int *ipr, int *niwrk, double *reps, int *ierr);
extern int nsp_ctrlpack_hesolv(double *d__, int *nwrk1, int *ipr, int *niwrk, int *m, double *reps, int *ierr);
extern int nsp_ctrlpack_backsb(double *c__, double *b, int *ind, int *n, int *m, int *mdim, int *ndim);
extern int nsp_ctrlpack_n2solv(double *a, double *b, double *c__, double *d__, int *nwrk1, int *ndim, int *n, int *mdim, int *m, int *ind, int *ipr, int *niwrk, double *reps, int *ierr);
extern int nsp_ctrlpack_h2solv(double *d__, int *nwrk1, int *ipr, int *niwrk, int *m, double *reps, int *ierr);
extern int nsp_ctrlpack_backs2(double *c__, double *b, int *ind, int *n, int *m, int *mdim, int *ndim);
extern int nsp_ctrlpack_tql2(int *nm, int *n, double *d__, double *e, double *z__, int *job, int *ierr);
extern int nsp_ctrlpack_tred2(int *nm, int *n, double *a, double *d__, double *e, double *z__);
extern int nsp_ctrlpack_wbalin(int *max__, int *n, int *low, int *igh, double *scale, double *ear, double *eai);
extern int nsp_ctrlpack_wbdiag(int *lda, int *n, double *ar, double *ai, double *rmax, double *er, double *ei, int *bs, double *xr, double *xi, double *yr, double *yi, double *scale, int *job, int *fail);
extern int nsp_ctrlpack_wcerr(double *ar, double *ai, double *w, int *ia, int *n, int *ndng, int *m, int *maxc);
extern int nsp_ctrlpack_wclmat(int *ia, int *n, double *ar, double *ai, double *br, double *bi, int *ib, double *w, double *c__, int *ndng);
extern int nsp_ctrlpack_wdegre(double *ar, double *ai, int *majo, int *nvrai);
extern int nsp_ctrlpack_wesidu(double *pr, double *pi, int *np, double *ar, double *ai, int *na, double *br, double *bi, int *nb, double *vr, double *vi, double *tol, int *ierr);
extern int nsp_ctrlpack_residu(double *p, int *np, double *a, int *na, double *b, int *nb, double *v, double *tol, int *ierr);

extern int nsp_ctrlpack_wexchn(double *ar, double *ai, double *vr, double *vi, int *n, int *l, int *fail, int *na, int *nv);
extern int nsp_ctrlpack_wexpm1(int *n, double *ar, double *ai, int *ia, double *ear, double *eai, int *iea, double *w, int *iw, int *ierr);
extern int nsp_ctrlpack_wgeco(double *ar, double *ai, int *lda, int *n, int *ipvt, double *rcond, double *zr, double *zi);
extern int nsp_ctrlpack_wgedi(double *ar, double *ai, int *lda, int *n, int *ipvt, double *detr, double *deti, double *workr, double *worki, int *job);
extern int nsp_ctrlpack_wgefa(double *ar, double *ai, int *lda, int *n, int *ipvt, int *info);
extern int nsp_ctrlpack_wgesl(double *ar, double *ai, int *lda, int *n, int *ipvt, double *br, double *bi, int *job);
extern int nsp_ctrlpack_wlslv(double *ar, double *ai, int *na, int *n, double *br, double *bi, int *nb, int *m, double *w, double *rcond, int *ierr, int *job);
extern int nsp_ctrlpack_wpade(double *ar, double *ai, int *ia, int *n, double *ear, double *eai, int *iea, double *alpha, double *w, int *ipvt, int *ierr);
extern int nsp_ctrlpack_wpofa(double *ar, double *ai, int *lda, int *n, int *info);
extern int nsp_ctrlpack_wqrdc(double *xr, double *xi, int *ldx, int *n, int *p, double *qrauxr, double *qrauxi, int *jpvt, double *workr, double *worki, int *job);
extern int nsp_ctrlpack_wqrsl(double *xr, double *xi, int *ldx, int *n, int *k, double *qrauxr, double *qrauxi, double *yr, double *yi, double *qyr, double *qyi, double *qtyr, double *qtyi, double *br, double *bi, double *rsdr, double *rsdi, double *xbr, double *xbi, int *job, int *info);
extern int nsp_ctrlpack_wrref(double *ar, double *ai, int *lda, int *m, int *n, double *eps);
extern int nsp_ctrlpack_wshrsl(double *ar, double *ai, double *br, double *bi, double *cr, double *ci, int *m, int *n, int *na, int *nb, int *nc, double *eps, double *rmax, int *fail);
extern int nsp_ctrlpack_wsvdc(double *xr, double *xi, int *ldx, int *n, int *p, double *sr, double *si, double *er, double *ei, double *ur, double *ui, int *ldu, double *vr, double *vi, int *ldv, double *workr, double *worki, int *job, int *info);

extern int nsp_ctrlpack_dgelsy1(int *m, int *n, int *nrhs, double *a, int *lda, double *b, int *ldb, int *jpvt, double *rcond,int *rank, double *work, int *lwork, int *info);

extern int nsp_ctrlpack_zgelsy1(int *m, int *n, int *nrhs, doubleC * a, int *lda,doubleC * b, int *ldb, int *jpvt, double *rcond,int *rank, doubleC * work, int *lwork, double *rwork, int *info);



#endif /*  CTRLPACK_H */
