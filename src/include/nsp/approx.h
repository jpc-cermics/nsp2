#ifndef NSP_INC_LIBAPPROX 
#define NSP_INC_LIBAPPROX 

enum {NOT_A_KNOT, NATURAL, CLAMPED, PERIODIC, FAST, FAST_PERIODIC, 
      MONOTONE, BY_ZERO, C0, LINEAR, BY_NAN, UNDEFINED};


extern void nsp_eval_piecewise_hermite(double *t, double *st, double *dst, double *d2st, 
				       double *d3st, int m, double *x, double *y, double *d, 
				       int n, int outmode);

extern void nsp_dpchim(double *x, double *u, double *d, int n, int inc);


extern void nsp_cubic_spline(double *x, double *y, double *d, int n, int type, 
			     double *A_d, double *A_sd, double *qdy, double *lll);


extern void nsp_derivd(double *x, double *u, double *du, int n, int inc, int type);


extern void nsp_nlinear_interp(double **x , double val[], int dim[], int n,
			       double **xp, double yp[], int np, int outmode, 
			       double u[], double v[], int ad[], int k[]);

extern int nsp_bicubic_subspline(double *x, double *y, double *u, int nx, int ny, double *C, int type);

extern int nsp_bicubic_spline(double *x, double *y, double *u, int nx, int ny, double *C, int type);

extern void nsp_eval_bicubic(double *x, double *y, double *C, int nx, int ny, double *x_eval, double *y_eval, 
			     double *z_eval, double *dzdx_eval, double *dzdy_eval, int m, int outmode);

#endif 

