#ifndef RANDLIB_H
#define RANDLIB_H

#include <nsp/math.h>
#include <nsp/sciio.h>

extern   double pow_di (double *, int *);

#define D_SIGN(a,b) ( b >= 0 ? (a >= 0 ? a : - a) : -(a >= 0 ? a : -a))
#define D_INT(x) ( (x>0) ? floor(x) : -floor(- x) )

extern double cdf_algdiv (double *a, double *b);
extern double cdf_alngam (double *x);
extern double cdf_alnrel (double *a);
extern double cdf_bcorr (double *a0, double *b0);
extern double cdf_betaln (double *a0, double *b0);
extern double cdf_brcmp1 (int *mu, double *a, double *b, double *x, double *y);
extern double cdf_brcomp (double *a, double *b, double *x, double *y);
extern double cdf_dbetrm (double *a, double *b);
extern double cdf_dcddexpm1 (double *x);
extern double cdf_devlpl (const double *a,const int *n, double *x);
extern double cdf_dinvnr (double *p, double *q);
extern double cdf_dlamch (char *, long int);
extern double cdf_dln1px (double *a);
extern double cdf_dlnbet (double *a0, double *b0);
extern double cdf_dlngam (double *a);
extern double cdf_dstrem (double *z__);
extern double cdf_dt1 (double *p, double *q, double *df);
extern double cdf_erf (double *x);
extern double cdf_erfc (int ind, double *x);
/* extern double cdf_esum (int *mu, double *x); */
extern double cdf_exparg (const int l);
extern double cdf_gam1 (double *a);
extern double cdf_gamln (double *a);
extern double cdf_gamln1 (double *a);
extern double cdf_gamma (double *a);
extern double cdf_gsumln (double *a, double *b);
extern double cdf_psi1 (double *xx);
extern double cdf_rcomp (double *a, double *x);
extern double cdf_rexp (double *x);
extern double cdf_rlog (double *x);
extern double cdf_rlog1 (double *x);
extern double cdf_spmpar (int );
extern double cdf_stvaln (double *p);
extern int cdf_bratio (double *a, double *b, double *x, double *y, double *w, double *w1, int *ierr);
extern int cdf_cdfbet (int *which, double *p, double *q, double *x, double *y, double *a, double *b, int *status, double *bound);
extern int cdf_cdfbin (int *which, double *p, double *q, double *s, double *xn, double *pr, double *ompr, int *status, double *bound);
extern int cdf_cdfchi (int *which, double *p, double *q, double *x, double *df, int *status, double *bound);
extern int cdf_cdfchn (int *which, double *p, double *q, double *x, double *df, double *pnonc, int *status, double *bound);
extern int cdf_cdff (int *which, double *p, double *q, double *f, double *dfn, double *dfd, int *status, double *bound);
extern int cdf_cdffnc (int *which, double *p, double *q, double *f, double *dfn, double *dfd, double *phonc, int *status, double *bound);
extern int cdf_cdfgam (int *which, double *p, double *q, double *x, double *shape, double *scale, int *status, double *bound);
extern int cdf_cdfnbn (int *which, double *p, double *q, double *s, double *xn, double *pr, double *ompr, int *status, double *bound);
extern int cdf_cdfnor (int *which, double *p, double *q, double *x, double *mean, double *sd, int *status, double *bound);
extern int cdf_cdfpoi (int *which, double *p, double *q, double *s, double *xlam, int *status, double *bound);
extern int cdf_cdft (int *which, double *p, double *q, double *t, double *df, int *status, double *bound);
extern int cdf_cumbet (double *x, double *y, double *a, double *b, double *cum, double *ccum);
extern int cdf_cumbin (double *s, double *xn, double *pr, double *ompr, double *cum, double *ccum);
extern int cdf_cumchi (double *x, double *df, double *cum, double *ccum);
extern int cdf_cumchn (double *x, double *df, double *pnonc, double *cum, double *ccum);
extern int cdf_cumf (double *f, double *dfn, double *dfd, double *cum, double *ccum);
extern int cdf_cumfnc (double *f, double *dfn, double *dfd, double *pnonc, double *cum, double *ccum);
extern int cdf_cumgam (double *x, double *a, double *cum, double *ccum);
extern int cdf_cumnbn (double *s, double *xn, double *pr, double *ompr, double *cum, double *ccum);
extern int cdf_cumnor (double *arg, double *result, double *ccum);
extern int cdf_cumpoi (double *s, double *xlam, double *cum, double *ccum);
extern int cdf_cumt (double *t, double *df, double *cum, double *ccum);
extern int cdf_dinvr (int *status, double *x, double *fx, int *qleft, int *qhi);

extern int cdf_dstinv (const double *zsmall,const double *zbig,const double *zabsst,const double *zrelst,
		       const double *zstpmu,const double *zabsto,const  double *zrelto);


extern int cdf_dstzr (double *zxlo, double *zxhi,const double *zabstl,const double *zreltl);
extern int cdf_dzror (int *status, double *x, double *fx, double *xlo, double *xhi, int *qleft, int *qhi);
extern int cdf_gaminv (double *a, double *x, double *x0, double *p, double *q, int *ierr);
extern int cdf_grat1 (double *a, double *x, double *r__, double *p, double *q, double *eps);
extern int cdf_gratio (double *a, double *x, double *ans, double *qans,const int *ind);
extern int cdf_ipmpar (const int *);

#endif /* RANDLIB_H */
