#ifndef NSP_RANDLIB_H
#define NSP_RANDLIB_H

#include <nsp/math.h> 
#include <nsp/sciio.h> 


typedef struct _PoissonStruct PoissonStruct;
#define SMALL_MEAN_POISSON_DIRECT_ALGO 15  /* 15 is the minimum value */
#define SMALL_MEAN_POISSON 35
struct _PoissonStruct
{
  double mu;   /* main parameter */
  /* parameters used for mu <= SMALL_MEAN_POISSON */
  double cumpr[SMALL_MEAN_POISSON+4]; 
  int taille;
  int K;
  /* parameters used for mu > SMALL_MEAN_POISSON */
  double smu;
  double a;
  double aa;
  double b;
  double c;
  double invalpha;
  double vr;
  double urvr;
  double invvr;
} ;


typedef struct _BinomialStruct BinomialStruct;

#define SMALL_MEAN_BINOMIAL_DIRECT_ALGO 15
#define SMALL_MEAN_BINOMIAL 25
struct _BinomialStruct
{
  int n;
  double p;
  int flipped;
  double r;
  /* parameters used for np <= SMALL_MEAN_BINOMIAL */
  double cumpr[SMALL_MEAN_BINOMIAL+4];
  int taille;
  int K;
  /* parameters used for np > SMALL_MEAN_BINOMIAL */
  int m;
  double npq;
  double a;
  double aa;
  double b;
  double c;
  double alpha;
  double vr;
  double urvr;
  double invvr;
  double h;
} ;


typedef struct _GammaStruct GammaStruct;

struct _GammaStruct
{
  double a;   /* main parameter */
  double c;
  double d;
} ;


typedef struct _BetaStruct BetaStruct;

struct _BetaStruct
{  
  GammaStruct a;
  GammaStruct b;
} ;

typedef struct _NbnStruct NbnStruct;

struct _NbnStruct
{
  double r;   /* main parameter */
  double p;   /* main parameter */
  double coef;
  GammaStruct G;
} ;

typedef struct _Chi2Struct Chi2Struct;

struct _Chi2Struct
{
  double nu;   /* main parameter */
  GammaStruct G;
} ;

typedef struct _NcChi2Struct NcChi2Struct;

struct _NcChi2Struct
{
  double nu;     /* main parameter */
  double xnonc;  /* main parameter */
  double sqrt_xnonc;
  GammaStruct G;
} ;


typedef struct _FStruct FStruct;

struct _FStruct
{  
  double nu1;
  double nu2;
  GammaStruct G1;
  GammaStruct G2;
} ;

typedef struct _NcFStruct NcFStruct;

struct _NcFStruct
{  
  NcChi2Struct X;
  Chi2Struct Y;
} ;


typedef struct _GeomStruct GeomStruct;

struct _GeomStruct
{
  double p;    /* main parameter */
  double inv_ln_1_m_p; 
} ;



/* supposed to be in fortran library */
extern double pow_di(double *ap, int *bp);

#define D_SIGN(a,b) ( b >= 0 ? (a >= 0 ? a : - a) : -(a >= 0 ? a : -a))
#define D_INT(x) ( (x>0) ? floor(x) : -floor(- x) )

extern void nsp_set_current_gen(int new_id);
extern int nsp_get_current_gen(void);
extern int nsp_rand_geom_init(double p, GeomStruct *G);
extern unsigned int nsp_rand_geom(GeomStruct *G);
extern unsigned int nsp_rand_geom_direct(double p);
extern int nsp_rand_ncchi2_init(double nu, double xnonc, NcChi2Struct *C);
extern double nsp_rand_ncchi2(NcChi2Struct *C);
extern double nsp_rand_ncchi2_direct(double nu, double xnonc);
extern int rand_ignuin (int a, int b);
extern double rand_ranf (void);
extern unsigned long int rand_lgi(void);
extern double nsp_rand_exp_core(void);
extern double nsp_rand_exp(double tau);
extern double nsp_rand_nor_core();
extern double nsp_rand_nor(double mu, double sigma);
extern int nsp_rand_gamma_init(double a, GammaStruct *G);
extern double nsp_rand_gamma(GammaStruct *G);
extern double nsp_rand_gamma_direct(double a);
extern int nsp_rand_beta_init(double a, double b, BetaStruct *B);
extern double nsp_rand_beta(BetaStruct *B);
extern double nsp_rand_beta_direct(double a, double b);
extern int nsp_rand_chi2_init(double nu, Chi2Struct *C);
extern double nsp_rand_chi2(Chi2Struct *C);
extern double nsp_rand_chi2_direct(double nu);
extern int nsp_rand_F_init(double nu1, double nu2, FStruct *F);
extern double nsp_rand_F(FStruct *F);
extern double nsp_rand_F_direct(double nu1, double nu2);
extern int nsp_rand_ncF_init(double nu1, double nu2, double xnonc, NcFStruct *E);
extern double nsp_rand_ncF(NcFStruct *E);
extern double nsp_rand_ncF_direct(double nu1, double nu2, double xnonc);
extern void rand_genprm (double *array, int larray);
extern void nsp_rand_prm (int *array, int n, int base);
extern void nsp_rand_smpl_bis(int *p, int n, int N, int base);
extern void nsp_rand_smpl(int *p, int n, int N, int base, int *head, int *next);
extern int nsp_rand_poisson_init(double mu, PoissonStruct *P);
extern int nsp_rand_poisson(PoissonStruct *P);
extern int nsp_rand_poisson_direct(double mu);
extern int nsp_rand_binomial_init(int n, double p, BinomialStruct *B);
extern int nsp_rand_binomial(BinomialStruct *B);
extern int nsp_rand_binomial_direct(int n, double p);
extern int nsp_rand_nbn_init(double r, double p, NbnStruct *N);
extern int nsp_rand_nbn(NbnStruct *N);
extern int nsp_rand_nbn_direct(double r, double p);
extern int rand_phrtsd(char *phrase,int *seed1, int *seed2) ;
extern int nsp_alias_method(double *p, double *q, int *j, int n);
extern int nsp_guide_table_method(double *p, int inc, double *q, int *key, int n);
extern int nsp_guide_table_method_bis(double *p, double *q, int *key, int n);
extern int nsp_rand_discrete_guide(double *q, int *key, int n);
extern int nsp_rand_discrete_alias(double *q, int *j, int n);
extern void rand_unf_01_and_uin_0_127_and_sign(double *u, int *k_7bits, int *k_1bits);
extern void rand_unf_01_and_uin_0_127(double *u, int *k_7bits);
extern void nsp_rand_multinomial_bis(double *q, int *key, int *ix, int ncat, int n);
extern void nsp_rand_multinomial(double *p, int *ix, int ncat, int n);
extern int nsp_markov_setup(double *p, double *q, int *key, int n);
extern void nsp_rand_markov(double *q, int *key, double *X0, double *X, int n, int X0mn, int m);
extern void nsp_rand_ndgauss(double *Mean, double *C, double *res, int n);
extern void nsp_rand_sphere(double *res, int n);
extern void nsp_rand_in_sphere(double *res, int n);
extern void nsp_rand_simplex(double *res, int m, int n);
extern double nsp_rand_cauchy(double sigma);
extern double nsp_rand_pareto(double a, double b);
extern double nsp_rand_logistic(double a, double b);
extern double nsp_rand_rayleigh(double sigma);
extern double nsp_rand_tailrayleigh(double sigma, double a);
extern double nsp_rand_weibull(double a, double b);
extern double nsp_rand_laplace(double a);
extern double nsp_rand_lognormal(double mu, double sigma);



#endif /*  NSP_RANDLIB_H */
