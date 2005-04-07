#ifndef NSP_LIBAPPROX 
#define NSP_LIBAPPROX 

enum {NOT_A_KNOT, NATURAL, CLAMPED, PERIODIC, FAST, FAST_PERIODIC, 
      MONOTONE, BY_ZERO, C0, LINEAR, BY_NAN, UNDEFINED};


void eval_piecewise_hermite(double *t, double *st, double *dst, double *d2st, 
			    double *d3st, int m, double *x, double *y, double *d, 
			    int n, int outmode);

void dpchim(double *x, double *u, double *d, int n, int inc);


void cubic_spline(double *x, double *y, double *d, int n, int type, 
		  double *A_d, double *A_sd, double *qdy, double *lll);


void derivd(double *x, double *u, double *du, int n, int inc, int type);


void nlinear_interp(double **x , double val[], int dim[], int n,
		    double **xp, double yp[], int np, int outmode, 
		    double u[], double v[], int ad[], int k[]);

#endif 

