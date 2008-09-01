#ifndef NSP_INC_LIBSPMF
#define NSP_INC_LIBSPMF

/* from dbinom 's Caterine Loader package */

typedef double NTYPE;
#define PIx2 6.283185307179586476925286        /* 2*pi */
#define HF_LG_PIx2  0.918938533204672741780329736406    /* 0.5*log(2*pi) */

#define LOG_ZERO -1e1000   /* to get -inf in double (was -1e-100 in the original code) */
#define D_0 ((give_log) ? LOG_ZERO : 0.0)
#define D_1 ((give_log) ? 0.0 : 1.0)
#define DEXP(x)   ((give_log) ? (x) : exp(x))
#define FEXP(f,x) ((give_log) ? -0.5*log(f)+(x) : exp(x)/sqrt(f))
#define STIRLERR(n) nsp_stirlerr(n)
#define FORCE_INT(x) floor(x+0.5)
#define NOT_NNEG_INT(x) ((x<0.0) || (fabs((x) - floor((x)+0.5)) > 1e-7))
#define INVALID_PARAMS -1.0  /* to be able to detect bad range parameters (was 0.0 original code) */
extern double nsp_stirlerr(NTYPE n);
extern double nsp_bd0(NTYPE x, double np);
extern double nsp_dbinom_raw(NTYPE x, NTYPE n, double p, double q, int give_log);
extern double nsp_dpois_raw(NTYPE x, double lambda, int give_log);
extern double nsp_pdf_beta(double x, NTYPE a, NTYPE b, int give_log);
extern double nsp_pdf_binom(NTYPE x, NTYPE n, double p, int give_log);
extern double nsp_pdf_f(double x, NTYPE m, NTYPE n, int give_log);
extern double nsp_pdf_gamma(double x, NTYPE r, double lambda, int give_log);
extern double nsp_pdf_hyper(NTYPE x, NTYPE r, NTYPE b, NTYPE n, int give_log);
extern double nsp_pdf_nbinom(NTYPE x, NTYPE n, double p, int give_log);
extern double nsp_pdf_pois(NTYPE x, double lambda, int give_log);
extern double nsp_pdf_t(double x, NTYPE df, int give_log);


/* from spmf.c */
extern double lnp1m1(double s);
extern double nsp_log1p(double x);
extern double nsp_sinpi(double x);
extern double nsp_gamma(double x);
extern double nsp_lngamma(double x);
extern int nsp_kcdf(double x, double *res, int n);
extern double nsp_kcdflim(double x, double *q);
extern double marsaglia_K(double x, int n);
extern double nsp_hypot(double x, double y);
extern void nsp_primefactors(unsigned int n, unsigned int *factors, int *powers, int *nb_factors);
extern int nsp_isprime(unsigned int n);
extern int nsp_primes(int n, int **Primes, int *nb_primes);
extern void nsp_convhull2d(int n, double *x, double *y, int *nhull, int *ind, int *p);
extern double nsp_pdf_normal(double x, double mu, double sigma);
extern double nsp_pdf_lognormal(double x, double mu, double sigma);
extern double nsp_pdf_chi2(double x, double nu);
extern double nsp_pdf_exp(double x, double Av);
extern double nsp_pdf_cauchy(double x, double sigma);
extern double nsp_pdf_pareto(double x, double a, double b);
extern double nsp_pdf_laplace(double x, double a);
extern double nsp_pdf_logistic(double x, double a, double b);
extern double nsp_pdf_weibull(double x, double a, double b);
extern double nsp_pdf_rayleigh(double x, double sigma);
extern double nsp_pdf_tailrayleigh(double x, double sigma, double a);
extern double nsp_pdf_geometric(double x, double p);


typedef struct Dxblk2_ Dxblk2; 

struct Dxblk2_ {
  int iflag, l, l2, kmax, nbitsf;
  double radix, radixl, rad2l, dlg10r;
  
};

extern int dxlegf(Dxblk2 *dxblk2, double *dnu1, int *nudiff, int *mu1, int *mu2, double *x, int *id, double *pqa, int *ipqa, int *ierror);


#endif 
