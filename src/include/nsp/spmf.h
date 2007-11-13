#ifndef NSP_INC_LIBSPMF
#define NSP_INC_LIBSPMF

extern double nsp_log1p(double x);
extern double nsp_sinpi(double x);
extern double nsp_gamma(double x);
extern double nsp_lngamma(double x);
extern int nsp_kcdf(double x, double *res, int n);
extern double nsp_kcdflim(double x, double *q);
extern double marsaglia_K(double x, int n);

#endif 
