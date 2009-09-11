#ifndef SCI_ODE_SOLVERS
#define SCI_ODE_SOLVERS

/* return flag values */
#define COMPLETED 0
#define MORE_THAN_MAXSTEP_NEEDED 1
#define EXCESS_ACCURACY_REQUESTED 2
#define ILLEGAL_INPUT_DETECTED 3    /* this one should leads to an error and so should not be used */
#define REPEATED_ERROR_TEST_FAILURE 4 /* same remark ? */
#define REPEATED_CONVERGENCE_FAILURE 5
#define ERROR_WEIGHT_BECAME_ZERO 6
#define STIFFNESS_DETECTED 7
#define TIME_STEP_BECOME_TOO_SMALL 8
#define FCT_EVAL_FAIL 9
#define MALLOC_FAIL 10


#include "nsp/object.h"   /* for Boolean Type */

/* ode rhs and jacobian definition types */
typedef int (*ode_f)(int *neq,const double *t,const double y[],double ydot[], void *param);
typedef int (*ode_jac)(int *neq,const double *t,const double y[],int *ml,
		       int *mu,double pd[],int *nrpd, void *param);

/* prototypes for ode solvers routines */
extern int nsp_dopri5(int n, ode_f fcn, void *param, double x, double *y, double xend, double hmax,
		      double *h0, double rtol, const double *atol, int itol, int maxstep, 
		      int nstiff, double safe, double beta, double fac1, double fac2,  
		      int task, Boolean warning, double *yout, double *xout, int nout, int *noutrel);


extern int C2F(lsoda)(ode_f f,int *neq, double *y, double *t, double *tout, int *itol, 
		      double *rtol, const double *atol, int *itask, int *istate, int *iopt, 
		      double *rwork, int *lrw, int *iwork, int *liw, ode_jac jac, int *jt, void *param);

extern int C2F(lsode)(ode_f f,int *neq, double *y, double *t, double *tout, int *itol, 
		      double *rtol, const double *atol, int *itask, int *istate, int *iopt, 
		      double *rwork, int *lrw, int *iwork, int *liw, ode_jac jac, int *mf, void *param);

#endif
