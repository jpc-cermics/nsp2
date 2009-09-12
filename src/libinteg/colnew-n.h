#ifndef __COLNEWN_H
#define __COLNEWN_H

#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/interf.h"
#include "nsp/blas.h"
#include "integ.h"

typedef int (*Fsub)(double *x ,double * z ,double * f, void *args);
typedef int (*DFsub)(double *x ,double * z ,double *df, void *args);
typedef int (*Gsub)(int *i ,double * z ,double *g, void *args);
typedef int (*DGsub)(int *i ,double * z ,double *dg, void *args);
typedef int (*Guess)(double *x ,double * z ,double *dmval, void *args);

extern int
nsp_colnew_colnew (int *ncomp, int *m, double *aleft, double *aright,
		   double *zeta, int *ipar, int *ltol, double *tol,
		   double *fixpnt, int *ispace, double *fspace, int *iflag,
		   Fsub fsub, DFsub dfsub, Gsub gsub, DGsub dgsub, Guess guess,
		   void *args, int *colnew_err);


#endif 
