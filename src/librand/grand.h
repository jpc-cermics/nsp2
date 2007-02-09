#ifndef RANDLIB_H
#define RANDLIB_H

#include <nsp/math.h> 
#include <nsp/sciio.h> 


/* supposed to be in fortran library */
extern double pow_di(double *ap, int *bp);

#define D_SIGN(a,b) ( b >= 0 ? (a >= 0 ? a : - a) : -(a >= 0 ? a : -a))
#define D_INT(x) ( (x>0) ? floor(x) : -floor(- x) )

extern double rand_igngeom(double p);
extern double rand_genbet (double *aa, double *bb);
extern double rand_genchi (double *df);
extern double rand_genexp (double av);
extern double rand_genf (double dfn, double dfd);
extern double rand_gengam (double a, double r);
extern double rand_gennch (double *df, double *xnonc);
extern double rand_gennf (double *dfn, double *dfd, double *xnonc);
extern double rand_gennor (double *av, double *sd);
extern double rand_genunf (double *low, double *high);
extern double rand_ignuin (double a, double b);
extern double rand_ranf (void);
extern unsigned long int rand_lgi(void);
extern double rand_sdot (int *n, double *sx, int *incx, double *sy, int *incy);
extern double rand_sexpo (void);
extern double rand_exp(double tau);
extern double rand_nor(double mu, double sigma);
extern double rand_sgamma (double a);
extern double rand_snorm (void);
extern int rand_genmn (double *parm, double *x, double *work);
extern int rand_genmul (int *n, double *p, int *ncat, int *ix);
extern void rand_genprm (double *array, int larray);
extern int rand_ignbin (int *, double *);
extern int rand_ignbin (int *n, double *pp);
extern int rand_ignnbn (int *n, double *p);
extern int rand_ignpoi (double *);
extern int rand_ignpoi (double *mu);
extern int rand_lennob (char *string, long int string_len);
extern int rand_phrtsd(char *phrase,int *seed1, int *seed2) ;
extern int rand_setgmn (double *meanv, double *covm, int *ldcovm, int *p,double *parm, int *ierr);
extern int rand_spofa (double *, int *, int *, int *);
extern int rand_spofa (double *a, int *lda, int *n, int *info);

#endif /*  RANDLIB_H */
