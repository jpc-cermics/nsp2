#ifndef FROMQUADPACK_H
#define FROMQUADPACK_H


typedef int (*intg_f)(const double *t, double *y, int *n);

extern int nsp_quadpack_dqagse (intg_f f, double *a, double *b, double *epsabs,
				double *epsrel, int *limit, double *result,
				double *abserr, int *neval, int *ier,
				double *alist__, double *blist, double *rlist,
				double *elist, int *iord, int *last,
				int *vectflag, int *stat);

/* The routine nsp_quadpack_dqagpe is nearly the same than nsp_quadpack_dqagse 
   but additionnal points where the integrand function is singular/discontinuous
   can be specified. The additionnal parameters are :
       npts2 : nb of singular points + 2 
       points : array (of size (npts2 - 2) with the npts2 - 2 singular points
       pts : (output) array of size npts2 with {a, b, points} sorted
       level : int array of size limit
       ndin : int array of size  npts2 
 */
extern int nsp_quadpack_dqagpe (intg_f f, double *a, double *b, int *npts2, double *points,
				double *epsabs, double *epsrel, int *limit, double *result,
				double *abserr, int *neval, int *ier, double *alist__,
				double *blist, double *rlist, double *elist, double *pts,
				int *iord, int *level, int *ndin, int *last, 
				int *vectflag, int *stat);

extern int nsp_quadpack_dqagie (intg_f f, double *bound, int *inf, double *epsabs,
				double *epsrel, int *limit, double *result,
				double *abserr, int *neval, int *ier,
				double *alist__, double *blist, double *rlist,
				double *elist, int *iord, int *last,
				int *vectflag, int *stat);


#endif /*  FROMQUADPACK_H */
