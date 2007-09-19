#ifndef RANDLIB_H
#define RANDLIB_H

#include <nsp/math.h> 
#include <nsp/sciio.h> 


typedef struct _PoissonStruct PoissonStruct;

#define SMALL_MEAN_POISSON 25
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

#define SMALL_MEAN_BINOMIAL 15
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
  /* parameters used for np >= SMALL_MEAN_BINOMIAL */
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
  double p;   /* main parameter */
  int n;      /* main parameter */
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


#define CheckScalarOrDims(fname,pos1,o1,m1,n1) if ( (o1->mn != 1) && (o1->m != m1 || o1->n != n1) ) \
   { Scierror("%s: argument %d should be a scalar or of size %dx%d\n",fname,pos1,m1,n1); \
     return RET_BUG;} 


/* supposed to be in fortran library */
extern double pow_di(double *ap, int *bp);

#define D_SIGN(a,b) ( b >= 0 ? (a >= 0 ? a : - a) : -(a >= 0 ? a : -a))
#define D_INT(x) ( (x>0) ? floor(x) : -floor(- x) )

extern void nsp_set_current_gen(int new_id);
extern int nsp_get_current_gen(void);
extern int nsp_rand_geom_init(double p, GeomStruct *G);
extern unsigned int nsp_rand_geom(GeomStruct *G);
extern unsigned int nsp_rand_geom_direct(double p);
extern double rand_genbet (double *aa, double *bb);
extern double rand_genchi (double df);
extern double rand_genexp (double av);
extern double rand_genf (double dfn, double dfd);
extern double rand_gengam (double a, double r__);
extern double rand_gennch (double *df, double *xnonc);
extern int nsp_rand_ncchi2_init(double nu, double xnonc, NcChi2Struct *C);
extern double nsp_rand_ncchi2(NcChi2Struct *C);
extern double nsp_rand_ncchi2_direct(double nu, double xnonc);
extern double rand_gennf (double *dfn, double *dfd, double *xnonc);
extern double rand_gennor (double av, double sd);
extern double rand_genunf (double *low, double *high);
extern int rand_ignuin (int a, int b);
extern double rand_ranf (void);
extern unsigned long int rand_lgi(void);
extern double rand_sdot (int *n, double *sx, int *incx, double *sy, int *incy);
extern double nsp_rand_exp_core(void);
extern double rand_sexpo (void);
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
extern double rand_sgamma (double a);
extern double rand_snorm (void);
extern int rand_genmn (double *parm, double *x, double *work);
extern int rand_genmul (int *n, double *p, int *ncat, int *ix);
extern void rand_genprm (double *array, int larray);
extern int nsp_rand_poisson_init(double mu, PoissonStruct *P);
extern int nsp_rand_poisson(PoissonStruct *P);
extern int nsp_rand_poisson_direct(double mu);
extern int nsp_rand_binomial_init(int n, double p, BinomialStruct *B);
extern int nsp_rand_binomial(BinomialStruct *B);
extern int nsp_rand_binomial_direct(int n, double p);
extern int rand_ignbin (int *n, double *pp);
extern int rand_ignnbn (int n, double p);
extern int nsp_rand_nbn_init(int n, double p, NbnStruct *N);
extern int nsp_rand_nbn(NbnStruct *N);
extern int nsp_rand_nbn_direct(int n, double p);
extern int rand_ignpoi (double mu);
extern int rand_lennob (char *string, long int string_len);
extern int rand_phrtsd(char *phrase,int *seed1, int *seed2) ;
extern int rand_setgmn (double *meanv, double *covm, int *ldcovm, int *p,double *parm, int *ierr);
extern int nsp_alias_method(double *p, double *q, int *j, int n);
extern int nsp_guide_table_method(double *p, int inc, double *q, int *key, int n);
extern int nsp_guide_table_method_bis(double *p, double *q, int *key, int n);
extern int nsp_rand_discrete_guide(double *q, int *key, int n);
extern int nsp_rand_discrete_alias(double *q, int *j, int n);
extern int nsp_rand_discrete(double *p, double *q, double *Res, int *key, int n, int mn);
extern void rand_unf_01_and_uin_0_127_and_sign(double *u, int *k_7bits, int *k_1bits);
extern void rand_unf_01_and_uin_0_127(double *u, int *k_7bits);
extern double logp1(double x);
extern void nsp_rand_multinomial1(double *q, int *key, int *ix, int ncat, int n);
extern void nsp_rand_multinomial2(double *p, int *ix, int ncat, int n);
extern int nsp_verify_probability_vector(double *p, int n);
extern int nsp_markov_setup(double *p, double *q, int *key, int n);
extern void nsp_rand_markov(double *q, int *key, double *X0, double *X, int n, int nn, int m);
extern int nsp_verif_markov_initial_state(double *X0, int mnX0, int n);
extern void nsp_rand_ndgauss(double *Mean, double *C, double *res, int n);
extern void nsp_rand_sphere(double *res, int n);

#endif /*  RANDLIB_H */
