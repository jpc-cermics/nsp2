#ifndef NSP_INC_INTEG
#define NSP_INC_INTEG

typedef int (*lsoda_f)(int * neq,double *t,double *y,double *ydot);
typedef int (*lsoda_j)(int * neq,double *t,double *y,int *ml,int *mu,double *pd,
		   int *nrpd);

extern int C2F(lsoda) (lsoda_f f, int *neq, double *y, double *t, double *tout, int *itol, double *rtol, double *atol, int *itask, int *istate, int *iopt, double *rwork, int *lrw, int *iwork, int *liw,lsoda_j jac, int *jt);

extern int C2F(rscma1) (double *rsav, double *isav);

extern int C2F(svcma1) (double *rsav, double *isav);

/* 

extern int C2F(ainvg) (S_fp res, S_fp adda, int *neq, double *t, double *y, double *ydot, int *miter, int *ml, int *mu, double *pw, int *ipvt, int *ier);
extern double C2F(bnorm) (int *n, double *a, int *nra, int *ml, int *mu, double *w);
extern int C2F(cfode) (int *meth, double *elco, double *tesco);
extern int C2F(colnew) (int *ncomp, int *m, double *aleft, double *aright, double *zeta, int *ipar, int *ltol, double *tol, double *fixpnt, int *ispace, double *fspace, int *iflag, U_fp fsub, U_fp dfsub, U_fp gsub, U_fp dgsub, U_fp guess);
extern int C2F(colsys) (int *ncomp, int *m, double *aleft, double *aright, double *zeta, int *ipar, int *ltol, double *tol, double *fixpnt, int *ispace, double *fspace, int *iflag, U_fp fsub, U_fp dfsub, U_fp gsub, U_fp dgsub, U_fp guess);
extern int C2F(contrl) (double *xi, double *xiold, double *z__, double *dmz, double *rhs, double *delz, double *deldmz, double *dqz, double *dqdmz, double *g, double *w, double *v, double *valstr, double *slope, double *scale, double *dscale, double *accum, int *ipvtg, int *integs, int *ipvtw, int *nfxpnt, double *fixpnt, int *iflag, U_fp fsub, U_fp dfsub, U_fp gsub, U_fp dgsub, U_fp guess);
extern int C2F(skale) (int *n, int *mstar, int *kd, double *z__, double *xi, double *scale, double *dscale);
extern int C2F(newmsh) (int *mode, double *xi, double *xiold, double *z__, double *dmz, double *valstr, double *slope, double *accum, int *nfxpnt, double *fixpnt);
extern int C2F(consts) (int *k, double *rho, double *coef);
extern int C2F(errchk) (double *xi, double *z__, double *dmz, double *valstr, int *ifin);
extern int C2F(lsyslv) (int *msing, double *xi, double *xiold, double *z__, double *dmz, double *delz, double *deldmz, double *g, double *w, double *v, double *rhs, double *dmzo, int *integs, int *ipvtg, int *ipvtw, double *rnorm, int *mode, S_fp fsub, U_fp dfsub, S_fp gsub, U_fp dgsub, S_fp guess);
extern int C2F(gderiv) (double *gi, int *nrow, int *irow, double *zval, double *dgz, int *mode, S_fp dgsub);
extern int C2F(vwblok) (double *xcol, double *hrho, int *jj, double *wi, double *vi, int *ipvtw, int *kd, double *zval, double *df, double *acol, double *dmzo, int *ncomp, S_fp dfsub, int *msing);
extern int C2F(gblock) (double *h__, double *gi, int *nrow, int *irow, double *wi, double *vi, int *kd, double *rhsz, double *rhsdmz, int *ipvtw, int *mode);
extern int C2F(appsln) (double *x, double *z__, double *fspace, int *ispace);
extern int C2F(approx) (int *i__, double *x, double *zval, double *a, double *coef, double *xi, int *n, double *z__, double *dmz, int *k, int *ncomp, int *mmax, int *m, int *mstar, int *mode, double *dmval, int *modm);
extern int C2F(rkbas) (double *s, double *coef, int *k, int *m, double *rkb, double *dm, int *mode);
extern int C2F(vmonde) (double *rho, double *coef, int *k);
extern int C2F(horder) (int *i__, double *uhigh, double *hi, double *dmz, int *ncomp, int *k);
extern int C2F(dmzsol) (int *kd, int *mstar, int *n, double *v, double *z__, double *dmz);
extern int C2F(fcblok) (double *bloks, int *integs, int *nbloks, int *ipivot, double *scrtch, int *info);
extern int C2F(factrb) (double *w, int *ipivot, double *d__, int *nrow, int *ncol, int *last, int *info);
extern int C2F(shiftb) (double *ai, int *nrowi, int *ncoli, int *last, double *ai1, int *nrowi1, int *ncoli1);
extern int C2F(sbblok) (double *bloks, int *integs, int *nbloks, int *ipivot, double *x);
extern int C2F(subfor) (double *w, int *ipivot, int *nrow, int *last, double *x);
extern int C2F(subbak) (double *w, int *nrow, int *ncol, int *last, double *x);
extern int C2F(dcutet) (U_fp funsub, int *numfun, double *ver, int *numtet, int *minpts, int *maxpts, double *epsabs, double *epsrel, int *lenver, int *nw, int *restar, double *result, double *abserr, int *neval, int *ifail, double *work, int *iwork);
extern int C2F(dchtet) (int *numfun, int *mdiv, double *ver, int *numtet, int *minpts, int *maxpts, double *epsabs, double *epsrel, int *lenver, int *nw, int *restar, int *maxsub, int *minsub, int *ifail);
extern int C2F(dadtet) (int *numfun, int *mdiv, double *ver, int *numtet, int *minsub, int *maxsub, U_fp funsub, double *epsabs, double *epsrel, int *lenver, int *restar, int *lenw, double *result, double *abserr, int *neval, int *nsub, int *ifail, double *values, double *errors, double *greate, double *work2, double *work3, int *list, int *vacant);
extern int C2F(dtrtet) (int *dvflag, int *sbrgns, double *greate, int *list, int *new__);
extern int C2F(drltet) (double *ver, int *numfun, U_fp funsub, double *null, double *basval, double *rgnerr, double *greate, double *sumval);
extern int C2F(dortet) (int *type__, double *gener, double *ver, int *numfun, S_fp funsub, double *sumval, double *work);
extern int C2F(ddasrt) (U_fp res, int *neq, double *t, double *y, double *yprime, double *tout, int *info, double *rtol, double *atol, int *idid, double *rwork, int *lrw, int *iwork, int *liw, double *rpar, int *ipar, U_fp jac, U_fp g, int *ng, int *jroot);
extern int C2F(drchek) (int *job, S_fp g, int *ng, int *neq, double *tn, double *tout, double *y, double *yp, double *phi, double *psi, int *kold, double *g0, double *g1, double *gx, int *jroot, int *irt, double *uround, int *info3, double *rwork, int *iwork, double *rpar, int *ipar);
extern int C2F(droots) (int *ng, double *hmin, int *jflag, double *x0, double *x1, double *g0, double *g1, double *gx, double *x, int *jroot, int *imax, int *last, double *alpha, double *x2);
extern int C2F(ddaini) (double *x, double *y, double *yprime, int *neq, S_fp res, U_fp jac, double *h__, double *wt, int *idid, double *rpar, int *ipar, double *phi, double *delta, double *e, double *wm, int *iwm, double *hmin, double *uround, int *nonneg, int *ntemp);
extern int C2F(ddajac) (int *neq, double *x, double *y, double *yprime, double *delta, double *cj, double *h__, int *ier, double *wt, double *e, double *wm, int *iwm, S_fp res, int *ires, double *uround, S_fp jac, double *rpar, int *ipar, int *ntemp);
extern double C2F(ddanrm) (int *neq, double *v, double *wt, double *rpar, int *ipar);
extern int C2F(ddaslv) (int *neq, double *delta, double *wm, int *iwm);
extern int C2F(ddassl) (S_fp res, int *neq, double *t, double *y, double *yprime, double *tout, int *info, double *rtol, double *atol, int *idid, double *rwork, int *lrw, int *iwork, int *liw, double *rpar, int *ipar, U_fp jac);
extern int C2F(ddastp) (double *x, double *y, double *yprime, int *neq, S_fp res, S_fp jac, double *h__, double *wt, int *jstart, int *idid, double *rpar, int *ipar, double *phi, double *delta, double *e, double *wm, int *iwm, double *alpha, double *beta, double *gamma, double *psi, double *sigma, double *cj, double *cjold, double *hold, double *s, double *hmin, double *uround, int *iphase, int *jcalc, int *k, int *kold, int *ns, int *nonneg, int *ntemp);
extern int C2F(ddatrp) (double *x, double *xout, double *yout, double *ypout, int *neq, int *kold, double *phi, double *psi);
extern int C2F(ddawts) (int *neq, int *iwt, double *rtol, double *atol, double *y, double *wt, double *rpar, int *ipar);
extern int C2F(xerhlt) (char *messg, int messg_len);
extern int C2F(xermsg) (char *librar, char *subrou, char *messg, int *nerr, int *level, int librar_len, int subrou_len, int messg_len);
extern int C2F(xerprn) (char *prefix, int *npref, char *messg, int *nwrap, int prefix_len, int messg_len);
extern int C2F(xgetua) (int *iunita, int *n);
extern int C2F(xsetua) (int *iunita, int *n);
extern int C2F(dqag0) (D_fp f, double *a, double *b, double *epsabs, double *epsrel, double *result, double *abserr, double *work, int *lwork, int *iwork, int *liwork, int *ifail);
extern int C2F(dqags) (D_fp f, double *a, double *b, double *epsabs, double *epsrel, double *alist__, double *blist, double *elist, double *rlist, int *limit, int *iord, int *liord, double *result, double *abserr, int *ier);
extern int C2F(epsalg) (int *n, double *epstab, double *result, double *abserr, double *res3la, int *nres);
extern int C2F(ewset) (int *n, int *itol, double *rtol, double *atol, double *ycur, double *ewt);
extern double C2F(fnorm) (int *n, double *a, double *w);
extern int C2F(intdy) (double *t, int *k, double *yh, int *nyh, double *dky, int *iflag);
extern int C2F(lsdisc) (S_fp f, int *neq, double *y, double *t, double *tout, double *rwork, int *lrw, int *istate);
extern int C2F(lsodar) (S_fp f, int *neq, double *y, double *t, double *tout, int *itol, double *rtol, double *atol, int *itask, int *istate, int *iopt, double *rwork, int *lrw, int *iwork, int *liw, U_fp jac, int *jt, U_fp g, int *ng, int *jroot);
extern int C2F(lsode) (S_fp f, int *neq, double *y, double *t, double *tout, int *itol, double *rtol, double *atol, int *itask, int *istate, int *iopt, double *rwork, int *lrw, int *iwork, int *liw, U_fp jac, int *mf);
extern int C2F(lsodi) (S_fp res, U_fp adda, U_fp jac, int *neq, double *y, double *ydoti, double *t, double *tout, int *itol, double *rtol, double *atol, int *itask, int *istate, int *iopt, double *rwork, int *lrw, int *iwork, int *liw, int *mf);
extern int C2F(order) (int *limit, int *last, int *maxerr, double *ermax, double *elist, int *iord, int *liord, int *nrmax);
extern int C2F(prepj) (int *neq, double *y, double *yh, int *nyh, double *ewt, double *ftem, double *savf, double *wm, int *iwm, S_fp f, S_fp jac);
extern int C2F(prepji) (int *neq, double *y, double *yh, int *nyh, double *ewt, double *rtem, double *savr, double *s, double *wm, int *iwm, S_fp res, S_fp jac, S_fp adda);
extern int C2F(prja) (int *neq, double *y, double *yh, int *nyh, double *ewt, double *ftem, double *savf, double *wm, int *iwm, S_fp f, S_fp jac);
extern int C2F(quarul) (D_fp f, double *a, double *b, double *result, double *abserr, double *resabs, double *resasc);
extern int C2F(rchek) (int *job, S_fp g, int *neq, double *y, double *yh, int *nyh, double *g0, double *g1, double *gx, int *jroot, int *irt);
extern int C2F(lsrgk) (U_fp f, int *neq, double *y, double *t, double *tout, int *itol, double *rtol, double *atol, int *itask, int *istate, int *iopt, double *rwork, int *lrw, int *iwork, int *liw, U_fp jac, int *mf);
extern int C2F(odeint) (double *ystart, int *nvar, double *x1, double *x2, double *eps, double *h1, double *hmin, int *nok, int *nbad, S_fp derivs, S_fp rkqc);
extern int C2F(rk4) (double *y, double *dydx, int *n, double *x, double *h__, double *yout, S_fp derivs);
extern int C2F(rkqc) (double *y, double *dydx, int *n, double *x, double *htry, double *eps, double *yscal, double *hdid, double *hnext, S_fp derivs);
extern int C2F(rkf45) (U_fp fydot, int *neqn, double *y, double *t, double *tout, int *itol, double *rerr, double *aerr__, int *itask, int *iflag, int *iopt, double *work, int *lrw, int *iwork, int *liw, real *bjac, int *mf);
extern int C2F(rkfs) (S_fp fydot, int *neqn, double *y, double *t, double *tout, double *rerr, double *aerr__, int *iflag, double *yp, double *h__, double *f1, double *f2, double *f3, double *f4, double *f5, double *savre, double *savae, double *savey, int *nfe, int *kop, int *init, int *jflag, int *kflag);
extern int C2F(fehl) (S_fp fydot, int *neqn, double *y, double *t, double *h__, double *yp, double *f1, double *f2, double *f3, double *f4, double *f5, double *s, double *savey);
extern int C2F(rksimp) (U_fp fydot2, int *neqn, double *y, double *t, double *tout, int *itol, double *rerr, double *aerr__, int *itask, int *iflag, int *iopt, double *work, int *lrw, int *iwork, int *liw, real *bjac, int *mf);
extern int C2F(fehl2) (S_fp fydot2, int *neqn, double *y, double *t, double *h__, double *yp, double *f1, double *f2, double *f3, double *f4, double *f5, double *s);
extern int C2F(roots) (int *ng, double *hmin, int *jflag, double *x0, double *x1, double *g0, double *g1, double *gx, double *x, int *jroot);
extern int C2F(rscar1) (double *rsav, double *isav);

extern int C2F(rscom1) (double *rsav, double *isav);
extern int C2F(solsy) (double *wm, int *iwm, double *x, double *tem);
extern int C2F(stoda) (int *neq, double *y, double *yh, int *nyh, double *yh1, double *ewt, double *savf, double *acor, double *wm, int *iwm, S_fp f, U_fp jac, S_fp pjac, S_fp slvs);
extern int C2F(stode) (int *neq, double *y, double *yh, int *nyh, double *yh1, double *ewt, double *savf, double *acor, double *wm, int *iwm, S_fp f, U_fp jac, S_fp pjac, S_fp slvs);
extern int C2F(stodi) (int *neq, double *y, double *yh, int *nyh, double *yh1, double *ewt, double *savf, double *savr, double *acor, double *wm, int *iwm, S_fp res, U_fp adda, U_fp jac, S_fp pjac, S_fp slvs);
extern int C2F(svcar1) (double *rsav, double *isav);
extern int C2F(svcma1) (double *rsav, double *isav);
extern int C2F(svcom1) (double *rsav, double *isav);
extern int C2F(twodq) (D_fp f, int *n, double *x, double *y, double *tol, int *iclose, int *maxtri, int *mevals, double *result, double *error, int *nu, int *nd, int *nevals, int *iflag, double *data, int *iwork);
extern int C2F(greatr) (double *a, double *b, int *nwds);
extern int C2F(hinitd) (int *nmax, int *nwds, int *n, int *t);
extern int C2F(hinitu) (int *nmax, int *nwds, int *n, int *t);
extern int C2F(hpacc) (int *nmax, int *nwds, double *data, int *n, int *t, double *xnode, int *k);
extern int C2F(hpdel) (int *nmax, int *nwds, double *data, int *n, int *t, L_fp hpfun, int *k);
extern int C2F(hpgro) (int *nmax, int *nwds, double *data, int *n, int *t, L_fp hpfun, int *i__);
extern int C2F(hpins) (int *nmax, int *nwds, double *data, int *n, int *t, double *xnode, L_fp hpfun);
extern int C2F(lqm0) (D_fp f, double *u, double *v, double *res8, double *est);
extern int C2F(lqm1) (D_fp f, double *u, double *v, double *res11, double *est);
extern int C2F(tridv) (double *node, double *node1, double *node2, double *coef, int *rank);
extern int C2F(integxerror) (char *messg, int *nmessg, int *nerr, int *level, int messg_len);
extern double C2F(vmnorm) (int *n, double *v, double *w);
extern double C2F(vnorm) (int *n, double *v, double *w);
extern int C2F(xerrwv) (char *msg, int *nmes, int *nerr, int *iert, int *ni, int *i1, int *i2, int *nr, double *r1, double *r2, int msg_len);
extern int C2F(xsetf) (int *mflag);
extern int C2F(xsetun) (int *lun);

*/




#endif /* NSP_INC_   **/
