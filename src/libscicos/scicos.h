#ifndef __SCICOS_H 
#define __SCICOS_H 

#include "simul.h"

/* maximum value for sum of number of inputs 
 * and outputs ports of a given 
 * block of type 2 
 */

#define SZ_SIZE 60

/* maximum value for sum of number of inputs 
 * and outputs of a given block 
 * of type 0 */

#define TB_SIZE 500

typedef void (*voidf)();

/* Blocks prototypes */
/*         block     flag*/

#define scicos_args_base  int *flag__, int *nevprt, double *t, double *xd, double *x, \
	       int *nx, double *z__, int *nz, double *tvec, int *ntvec,\
	       double *rpar, int *nrpar, int *ipar, int *nipar 

/* flag  nclock t    xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar  intabl  ni  outabl no */
#define scicos_args_F0  scicos_args_base, double *u, int *nu, double *y, int *ny 

/* flag  nclock t    xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar  u1 n1 u2 n2 .... */
#define scicos_args_F  scicos_args_base, double *uy1, int *nuy1, double *uy2, int *nuy2, double *uy3, int *nuy3, \
	    double *uy4, int *nuy4, double *uy5, int *nuy5, double *uy6,\
	    int *nuy6, double *uy7, int *nuy7, double *uy8, int *nuy8, \
	    double *uy9, int *nuy9, double *uy10, int *nuy10, double *uy11, int *nuy11,double *uy12, int *nuy12, \
 	    double *uy13, int *nuy13, double *uy14, int *nuy14, double *uy15, int *nuy15,double *uy16, int *nuy16, \
            double *uy17, int *nuy17, double *uy18, int *nuy18

/* flag  nclockf t  residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar    args_in sz_in, n_in  args_out sz_out, n_out  */
#define scicos_args_F2 scicos_args_base, double **inptr, int *insz, int *nin, double **outptr, int *outsz, int *nout

/* flag  nclockf t  residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar   args_in sz_in, n_in  args_out sz_out, n_out g ng */
#define scicos_args_F2z scicos_args_F2 , double *g , int *ng

/* flag  nclock t residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar */
#define scicos_args_base_i  int *flag__, int *nevprt, double *t,double *res, double *xd, double *x, \
	       int *nx, double *z__, int *nz, double *tvec, int *ntvec,\
	       double *rpar, int *nrpar, int *ipar, int *nipar 

/* flag  nclock t residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar u1 nu1 u2 nu2 .... */
#define scicos_args_Fi scicos_args_base_i,  double *uy1, int *nuy1, double *uy2, int *nuy2, double *uy3, int *nuy3, \
	    double *uy4, int *nuy4, double *uy5, int *nuy5, double *uy6,\
	    int *nuy6, double *uy7, int *nuy7, double *uy8, int *nuy8, \
	    double *uy9, int *nuy9, double *uy10, int *nuy10, double *uy11, int *nuy11,double *uy12, int *nuy12, \
 	    double *uy13, int *nuy13, double *uy14, int *nuy14, double *uy15, int *nuy15,double *uy16, int *nuy16, \
            double *uy17, int *nuy17, double *uy18, int *nuy18 

/* flag  nclockf t  residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar    args_in sz_in, n_in  args_out sz_out, n_out  */
#define scicos_args_Fi2 scicos_args_base_i, double **inptr, int *insz, int *nin, double **outptr, int *outsz, int *nout

/* flag  nclockf t  residual xd   x    nx   z   nz   tvec   ntvec  rpar  nrpar ipar  nipar   args_in sz_in, n_in  args_out sz_out, n_out g ng */
#define scicos_args_Fi2z scicos_args_Fi2, double *g , int *ng

#define scicos_args_Fm1 int *flag__, int *nevprt, int *ntvec, double *rpar, int *nrpar, int *ipar, int *nipar, double *u, int *nu 

typedef void (*ScicosF0) (scicos_args_F0);
typedef void (*ScicosF) (scicos_args_F); 
typedef void (*ScicosF2) (scicos_args_F2);
typedef void (*ScicosF2z) (scicos_args_F2z);
typedef void (*ScicosFi) (scicos_args_Fi);
typedef void (*ScicosFi2) (scicos_args_Fi2);
typedef void (*ScicosFi2z) (scicos_args_Fi2z);
typedef void (*ScicosFm1) (scicos_args_Fm1);
typedef void (*ScicosF4) (scicos_block *,int );

typedef struct _scicos_block_table scicos_block_table ;

struct _scicos_block_table  {
  char *name;
  ScicosF fonc;
};



#include "scicos-proto.h"

#endif 
