#ifndef RANDLIB_H
#define RANDLIB_H

extern struct {
  int iero;
} C2F(ierode);

typedef int (*ode_f)(int *neq,const double *t,const double y[],double ydot[]);
typedef int (*ode_jac)(int *neq,const double *t,const double y[],int *ml,
		       int *mu,double pd[],int *nrpd);

typedef int (*rk_rkqc_f) (double *y, double *dydx, int *n, double *x, double *htry,
			  double *eps, double *yscal, double *hdid, double *hnext,ode_f derivs);

/*
 *    subroutine de Num Recipies modifiee pour avoir le meme test 
 *    d'arret que lsode 
 */

extern int rk_lsrgk (ode_f f, int *neq, double *y, double *t, double *tout,
			 int *itol, double *rtol, double *atol, int *itask,
			 int *istate, int *iopt, double *rwork, int *lrw,
			 int *iwork, int *liw, ode_jac jac, int *mf);


extern int rk_rkf45 (ode_f fydot, int *neqn, double *y, double *t,
			 double *tout, int *itol, double *rerr,
			 double *aerr__, int *itask, int *iflag, int *iopt,
			 double *work, int *lrw, int *iwork, int *liw,
			 double *bjac, int *mf);
extern int rk_rksimp (ode_f fydot2, int *neqn, double *y, double *t,
			  double *tout, int *itol, double *rerr,
			  double *aerr__, int *itask, int *iflag, int *iopt,
			  double *work, int *lrw, int *iwork, int *liw,
			  double *bjac, int *mf);

#endif /*  RANDLIB_H */
