#ifndef RANDLIB_H
#define RANDLIB_H
extern int scicos_affich (int *flag__, int *nevprt, double *t, double *xd,
			  double *x, int *nx, double *z__, int *nz,
			  double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar, double *u, int *nu,
			  double *y, int *ny);
extern int scicos_setblockwin (int *win, int *cur);
extern int scicos_recterase (double *r__);
extern int scicos_affdraw (int *fontd, int *form, double *val, double *r__);
extern int scicos_fscope (int *flag__, int *nevprt, double *t, double *xd,
			  double *x, int *nx, double *z__, int *nz,
			  double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar);
/*:ref: dr1_ 14 14 13 13 4 4 4 4 4 4 7 7 7 7 124 124 */
/*:ref: getouttb_ 14 3 4 4 7 */
/*:ref: dr_ 14 14 13 13 4 4 4 4 4 4 7 7 7 7 124 124 */
/*:ref: plot2d_ 14 13 7 7 4 4 4 13 13 7 4 4 4 124 124 */
/*:ref: sciwin_ 14 0 */
/*:ref: setscale2d_ 14 4 7 7 13 124 */
/*:ref: dset_ 14 4 4 7 7 4 */
extern int ftree2scicos_ (int *vec, int *nb, int *deput, int *outoin,
			  int *outoinptr, int *ord, int *nord, int *ok);
/*:ref: isort_ 14 3 4 4 4 */
extern int ftree3scicos_ (int *vec, int *nb, int *deput, int *typl, int *bexe,
			  int *boptr, int *blnk, int *blptr, int *kk,
			  int *ord, int *nord, int *ok);
/*:ref: isort_ 14 3 4 4 4 */
extern int ftree4scicos_ (int *vec, int *nb, int *nd, int *nnd, int *typr,
			  int *outoin, int *outoinptr, int *r1, int *r2,
			  int *nr);
extern int scicos_ifthel (int *flag__, int *nevprt, int *ntvec, double *rpar,
			  int *nrpar, int *ipar, int *nipar, double *u,
			  int *nu);
/* comlen dbcos_ 4 */
extern int scicos_iocopy (int *flag__, int *nevprt, double *t, double *xd,
			  double *x, int *nx, double *z__, int *nz,
			  double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar, double *u, int *nu,
			  double *y, int *ny);
/* comlen dbcos_ 4 */
extern int scicos_mscope (int *flag__, int *nevprt, double *t, double *xd,
			  double *x, int *nx, double *z__, int *nz,
			  double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar, double *u, int *nu,
			  double *y, int *ny);
/* comlen curblk_ 4 */
/*:ref: dr1_ 14 14 13 13 4 4 4 4 4 4 7 7 7 7 124 124 */
/*:ref: setscale2d_ 14 4 7 7 13 124 */
/*:ref: scicosclip_ 14 1 4 */
/*:ref: dr_ 14 14 13 13 4 4 4 4 4 4 7 7 7 7 124 124 */
/*:ref: plot2d_ 14 11 7 7 4 4 4 13 13 7 4 124 124 */
/*:ref: sciwin_ 14 0 */
/*:ref: getlabel_ 14 4 4 13 4 124 */
/*:ref: dset_ 14 4 4 7 7 4 */
extern int scicos_scope (int *flag__, int *nevprt, double *t, double *xd,
			 double *x, int *nx, double *z__, int *nz,
			 double *tvec, int *ntvec, double *rpar, int *nrpar,
			 int *ipar, int *nipar, double *u, int *nu);
/* comlen curblk_ 4 */
/*:ref: dr1_ 14 14 13 13 4 4 4 4 4 4 7 7 7 7 124 124 */
/*:ref: scicosclip_ 14 1 4 */
/*:ref: dr_ 14 14 13 13 4 4 4 4 4 4 7 7 7 7 124 124 */
/*:ref: plot2d_ 14 13 7 7 4 4 4 13 13 7 4 4 4 124 124 */
/*:ref: sciwin_ 14 0 */
/*:ref: setscale2d_ 14 4 7 7 13 124 */
/*:ref: getlabel_ 14 4 4 13 4 124 */
/*:ref: dset_ 14 4 4 7 7 4 */
extern int scicos_scopxy (int *flag__, int *nevprt, double *t, double *xd,
			  double *x, int *nx, double *z__, int *nz,
			  double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar, double *u, int *nu,
			  double *y, int *ny);
/* comlen curblk_ 4 */
/*:ref: dr1_ 14 14 13 13 4 4 4 4 4 4 7 7 7 7 124 124 */
/*:ref: unsfdcopy_ 14 5 4 7 4 7 4 */
/*:ref: dr_ 14 14 13 13 4 4 4 4 4 4 7 7 7 7 124 124 */
/*:ref: setscale2d_ 14 4 7 7 13 124 */
/*:ref: sciwin_ 14 0 */
/*:ref: plot2d_ 14 11 7 7 4 4 4 13 13 7 4 124 124 */
/*:ref: getlabel_ 14 4 4 13 4 124 */
/*:ref: sxevents_ 14 0 */
extern int scicos_scoxy (int *flag__, int *nevprt, double *t, double *xd,
			 double *x, int *nx, double *z__, int *nz,
			 double *tvec, int *ntvec, double *rpar, int *nrpar,
			 int *ipar, int *nipar, double *u, int *nu, double *y,
			 int *ny);
/* comlen curblk_ 4 */
/*:ref: dr1_ 14 14 13 13 4 4 4 4 4 4 7 7 7 7 124 124 */
/*:ref: sciwin_ 14 0 */
/*:ref: setscale2d_ 14 4 7 7 13 124 */
/*:ref: dr_ 14 14 13 13 4 4 4 4 4 4 7 7 7 7 124 124 */
/*:ref: getlabel_ 14 4 4 13 4 124 */
/*:ref: plot2d_ 14 11 7 7 4 4 4 13 13 7 4 124 124 */
/*:ref: sxevents_ 14 0 */
extern int scicos_sctree (int *nb, int *vec, int *in, int *depu, int *outptr,
			  int *cmat, int *ord, int *nord, int *ok, int *kk);
/*:ref: isort_ 14 3 4 4 4 */
extern /* Subroutine */ int scicos_dset (int *, double *, double *, int *);
extern /* Subroutine */ int scicos_getouttb (int *, int *, double *);
extern /* Subroutine */ int plot2scicos_d (double *, double *, int *, int *,
					   int *, char *, char *, double *,
					   int *, int *, int *, long int,
					   long int),
setscale2scicos_d (double *, double *, char *, long int);
extern /* Subroutine */ int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern /* Subroutine */ int scicos_sciwin (void), dr1scicos_ (char *, char *,
							      int *, int *,
							      int *, int *,
							      int *, int *,
							      double *,
							      double *,
							      double *,
							      double *,
							      long int,
							      long int);
extern /* Subroutine */ int scicos_isort (int *, int *, int *);
extern /* Subroutine */ int scicos_isort (int *, int *, int *);
extern /* Subroutine */ int scicos_getlabel (int *, char *, int *, long int);
extern /* Subroutine */ int scicos_dset (int *, double *, double *, int *);
extern /* Subroutine */ int plot2scicos_d (double *, double *, int *, int *,
					   int *, char *, char *, double *,
					   int *, long int, long int),
setscale2scicos_d (double *, double *, char *, long int);
extern /* Subroutine */ int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern /* Subroutine */ int scicos_sciwin (void), dr1scicos_ (char *, char *,
							      int *, int *,
							      int *, int *,
							      int *, int *,
							      double *,
							      double *,
							      double *,
							      double *,
							      long int,
							      long int),
scicos_scicosclip (int *);
extern /* Subroutine */ int scicos_getlabel (int *, char *, int *, long int),
scicos_dset (int *, double *, double *, int *);
extern /* Subroutine */ int plot2scicos_d (double *, double *, int *, int *,
					   int *, char *, char *, double *,
					   int *, int *, int *, long int,
					   long int),
setscale2scicos_d (double *, double *, char *, long int);
extern /* Subroutine */ int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern /* Subroutine */ int scicos_sciwin (void), dr1scicos_ (char *, char *,
							      int *, int *,
							      int *, int *,
							      int *, int *,
							      double *,
							      double *,
							      double *,
							      double *,
							      long int,
							      long int),
scicos_scicosclip (int *);
extern /* Subroutine */ int scicos_getlabel (int *, char *, int *, long int);
extern /* Subroutine */ int scicos_sxevents (void);
extern /* Subroutine */ int scicos_unsfdcopy (int *, double *, int *,
					      double *, int *),
plot2scicos_d (double *, double *, int *, int *, int *, char *, char *,
	       double *, int *, long int, long int),
setscale2scicos_d (double *, double *, char *, long int);
extern /* Subroutine */ int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern /* Subroutine */ int scicos_sciwin (void), dr1scicos_ (char *, char *,
							      int *, int *,
							      int *, int *,
							      int *, int *,
							      double *,
							      double *,
							      double *,
							      double *,
							      long int,
							      long int);
extern /* Subroutine */ int scicos_getlabel (int *, char *, int *, long int);
extern /* Subroutine */ int scicos_sxevents (void);
extern /* Subroutine */ int plot2scicos_d (double *, double *, int *, int *,
					   int *, char *, char *, double *,
					   int *, long int, long int),
setscale2scicos_d (double *, double *, char *, long int);
extern /* Subroutine */ int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern /* Subroutine */ int scicos_sciwin (void), dr1scicos_ (char *, char *,
							      int *, int *,
							      int *, int *,
							      int *, int *,
							      double *,
							      double *,
							      double *,
							      double *,
							      long int,
							      long int);
extern /* Subroutine */ int scicos_isort (int *, int *, int *);
#endif /*  RANDLIB_H */
