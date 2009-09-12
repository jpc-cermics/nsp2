#ifndef FROMQUADPACK_H
#define FROMQUADPACK_H


typedef int (*intg_f)(const double *t, double *y, int *n);

extern int nsp_quadpack_dqagse (intg_f f, double *a, double *b, double *epsabs,
				double *epsrel, int *limit, double *result,
				double *abserr, int *neval, int *ier,
				double *alist__, double *blist, double *rlist,
				double *elist, int *iord, int *last,
				int *vectflag, int *stat);
extern int nsp_quadpack_dqagie (intg_f f, double *bound, int *inf, double *epsabs,
				double *epsrel, int *limit, double *result,
				double *abserr, int *neval, int *ier,
				double *alist__, double *blist, double *rlist,
				double *elist, int *iord, int *last,
				int *vectflag, int *stat);


#endif /*  FROMQUADPACK_H */
