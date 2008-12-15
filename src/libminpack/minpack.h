#ifndef MINPACKLIB_H
#define MINPACKLIB_H

#include <nsp/math.h>


typedef int (*minpack_fcn1)(const int *n,double *x,double *fvec,int *iflag,void *data);
typedef int (*minpack_fcn2)(const int *m,const int *n,double *x,double *fvec,int *iflag,void *data);
typedef int (*minpack_fcn3)(const int *n,double *x,double *fvec,double *fjac,int *ldfjac,int *iflag,void *data);
typedef int (*minpack_fcn4)(const int *m,const int *n,double *x,double *fvec,double *fjac,int *ldfjac,
			    int *iflag,void *data);
typedef int (*minpack_fcn5)(const int *m,const int *n,double *x,double *fvec, double *fjrow,int *iflag,void *data);
typedef int (*minpack_fcn6)(const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *hybr_obj_d);
typedef int (*minpack_fcn7)(const int *m,const int *n,double *x,double *fjac,int *ldfjac, int *iflag, void *hybr_obj_d);

extern int minpack_chkder (int *m, int *n, double *x, double *fvec,
			   double *fjac, int *ldfjac, double *xp,
			   double *fvecp, int *mode, double *err);

int minpack_dogleg (int n,const double *r, int lr, const double *diag,const  double *qtb,
		    double delta, double *x, double *wa1, double *wa2);

extern double minpack_dpmpar (int i);
extern double minpack_enorm (int n,const double *x);

extern int minpack_fdjac1 (minpack_fcn1 fcn, int *n,double *x,const double *fvec, double *fjac,
			   const int *ldfjac, int *iflag,const int *ml,const int *mu, double epsfcn,
			   double *wa1, double *wa2, void *data);

extern int minpack_fdjac2 (minpack_fcn2 fcn,const int *m,const int *n, double *x,const double *fvec,
			   double *fjac,const int *ldfjac, int *iflag, double epsfcn,
			   double *wa,void *data);

extern int minpack_hybrd1 (minpack_fcn1 fcn, int *n, double *x, double *fvec,
			   double *tol, double *tolf, int *info, double *wa, int *lwa,void *);

extern int minpack_hybrj1 (minpack_fcn3 fcn, int *n, double *x, double *fvec,
			   double *fjac, int *ldfjac, double *tol, double *tolf, int *info,
			   double *wa, int *lwa,void *data);
extern int minpack_hybrj (minpack_fcn3 fcn, int *n, double *x, double *fvec,
			  double *fjac, int *ldfjac, double *xtol, double *ftol,
			  int *maxfev, double *diag, int *mode,
			  const double *factor, int *nprint, int *info, int *nfev,
			  int *njev, double *r__, int *lr, double *qtf,
			  double *wa1, double *wa2, double *wa3, double *wa4,void *data);
extern int minpack_lmder1 (minpack_fcn4 fcn, int *m, int *n, double *x, double *fvec,
			   double *fjac, int *ldfjac, double *tol, int *info,
			   int *ipvt, double *wa, int *lwa,void *data);
extern int minpack_lmder (minpack_fcn4 fcn, int *m, int *n, double *x, double *fvec,
			  double *fjac, int *ldfjac, double *ftol,
			  double *xtol, double *gtol, int *maxfev,
			  double *diag, int *mode, const double *factor,
			  int *nprint, int *info, int *nfev, int *njev,
			  int *ipvt, double *qtf, double *wa1, double *wa2,
			  double *wa3, double *wa4,void *data);
extern int minpack_lmdif1 (minpack_fcn2 fcn, int *m, int *n, double *x, double *fvec,
			   double *tol, int *info, int *iwa, double *wa,
			   int *lwa,void *data);
extern int minpack_lmdif (minpack_fcn2 fcn, int *m, int *n, double *x, double *fvec,
			  double *ftol, double *xtol, double *gtol,
			  int *maxfev, double *epsfcn, double *diag,
			  int *mode, const double *factor, int *nprint, int *info,
			  int *nfev, double *fjac, int *ldfjac, int *ipvt,
			  double *qtf, double *wa1, double *wa2, double *wa3,
			  double *wa4,void *data);

extern int minpack_lmpar (int *n, double *r__, int *ldr, int *ipvt,
			  double *diag, double *qtb, double *delta,
			  double *par, double *x, double *sdiag, double *wa1,
			  double *wa2);

extern int minpack_lmstr1 (minpack_fcn5 fcn, int *m, int *n, double *x, double *fvec,
			   double *fjac, int *ldfjac, double *tol, int *info,
			   int *ipvt, double *wa, int *lwa,void *data);
extern int minpack_qform (int *m, int *n, double *q, int *ldq, double *wa);
extern int minpack_qrfac (int *m, int *n, double *a, int *lda, int *pivot,
			  int *ipvt, int *lipvt, double *rdiag,
			  double *acnorm, double *wa);
extern int minpack_qrsolv (int *n, double *r__, int *ldr, int *ipvt,
			   double *diag, double *qtb, double *x,
			   double *sdiag, double *wa);
extern int minpack_r1mpyq (int *m, int *n, double *a, int *lda, double *v,
			   double *w);
extern int minpack_r1updt (int *m, int *n, double *s, int *ls, double *u,
			   double *v, double *w, int *sing);
extern int minpack_rwupdt (int *n, double *r__, int *ldr, double *w,
			   double *b, double *alpha, double *cos__,
			   double *sin__);

extern int minpack_hybrd (minpack_fcn1 fcn, int *n, double *x, double *fvec, double *xtol, double *ftol,
			  int *maxfev, int *ml, int *mu, double *epsfcn, double *diag,
			  int *mode,const double *factor, int *nprint, int *info, int *nfev,
			  double *fjac, int *ldfjac, double *r__, int *lr, double *qtf,
			  double *wa1, double *wa2, double *wa3, double *wa4,void *data);

int minpack_hybrd_eval (minpack_fcn1 fcn, int *n, double *x, double *fvec, 
			int *maxfev, int *ml, int *mu, double *epsfcn, 
			int *info, int *nfev,
			double *fjac, int *ldfjac, double *wa1, double *wa2, void *data);

extern int minpack_lmstr (minpack_fcn5 fcn, int *m, int *n, double *x, double *fvec,
			  double *fjac, int *ldfjac, double *ftol, double *xtol,
			  double *gtol, int *maxfev, double *diag, int *mode,
			  const double *factor, int *nprint, int *info, int *nfev, int *njev,
			  int *ipvt, double *qtf, double *wa1, double *wa2, double *wa3,
			  double *wa4,void *data);


int minpack_rosenbrock (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_rosenbrock (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_powell_singular (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_powell_singular (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_powell_badly_scaled (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_powell_badly_scaled (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_wood (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_wood (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_helical_valley (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_helical_valley (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_watson (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_watson (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_chebyquad (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_chebyquad (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_brown (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_brown (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_discrete_boundary (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_discrete_boundary (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_discrete_integral (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_discrete_integral (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_trigonometric (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_trigonometric (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_variably_dimensioned(const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_variably_dimensioned (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_broyden_tridiagonal (const int *n, double *x, double *fvec, int *iflag, void *data);
int minpack_jac_broyden_tridiagonal (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_broyden_banded (const int *n, double *x, double *fvec, int *iflag, void *data );
int minpack_jac_broyden_banded (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
int minpack_fsol1 (const int *n, double *x, double *fvec, int *iflag, void *data );
int minpack_jac_fsol1 (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);

int minpack_initpt (const int *n, double *x, int *nprob, double *factor);

int minpack_lmdif2 (minpack_fcn2 fcn, int *m, int *n, double *x, double *fvec,
		    double *tol, int *info, int *iwa, double *wa, int *lwa,
		    double *wb, int *lwb,
		    void *data);

#endif 
