#ifndef NSP_SCICOS_PROTO_H
#define NSP_SCICOS_PROTO_H

extern int scicos_affich (int *flag__, int *nevprt, double *t, double *xd,
			  double *x, int *nx, double *z__, int *nz,
			  double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar, double *u, int *nu,
			  double *y, int *ny);

extern int scicos_setblockwin (BCG *Xgc,int *win, int *cur);

extern int scicos_recterase (double *r__);

extern int scicos_affdraw (BCG *Xgc,const int *fontd,const int *form,const double *val,const double *r);

extern int scicos_fscope (int *flag__, int *nevprt, double *t, double *xd,
			  double *x, int *nx, double *z__, int *nz,
			  double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar);

extern int scicos_ftree2 (int *vec, int *nb, int *deput, int *outoin,
			  int *outoinptr, int *ord, int *nord, int *ok);

extern int scicos_ftree3(int *vec, int *nb, int *deput, int *typl, int *bexe,
			  int *boptr, int *blnk, int *blptr, int *kk,
			  int *ord, int *nord, int *ok);

extern int scicos_ftree4(int *vec, int *nb, int *nd, int *nnd, int *typr,
			  int *outoin, int *outoinptr, int *r1, int *r2,
			  int *nr);
extern int scicos_ifthel(int *flag__, int *nevprt, int *ntvec, double *rpar,
			  int *nrpar, int *ipar, int *nipar, double *u,
			  int *nu);

extern int scicos_iocopy(int *flag__, int *nevprt, double *t, double *xd,
			  double *x, int *nx, double *z__, int *nz,
			  double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar, double *u, int *nu,
			  double *y, int *ny);

extern int scicos_mscope(int *flag__, int *nevprt, double *t, double *xd,
			  double *x, int *nx, double *z__, int *nz,
			  double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar, double *u, int *nu,
			  double *y, int *ny);

extern int scicos_scope(int *flag__, int *nevprt, double *t, double *xd,
			 double *x, int *nx, double *z__, int *nz,
			 double *tvec, int *ntvec, double *rpar, int *nrpar,
			 int *ipar, int *nipar, double *u, int *nu);

extern int scicos_scopxy (int *flag__, int *nevprt, double *t, double *xd,
			  double *x, int *nx, double *z__, int *nz,
			  double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar, double *u, int *nu,
			  double *y, int *ny);
extern int scicos_scoxy (int *flag__, int *nevprt, double *t, double *xd,
			 double *x, int *nx, double *z__, int *nz,
			 double *tvec, int *ntvec, double *rpar, int *nrpar,
			 int *ipar, int *nipar, double *u, int *nu, double *y,
			 int *ny);
extern int scicos_sctree (int *nb, int *vec, int *in, int *depu, int *outptr,
			  int *cmat, int *ord, int *nord, int *ok, int *kk);
extern  int scicos_dset (int *, double *, double *, int *);
extern  int scicos_getouttb (int *, int *, double *);
extern  int setscale2scicos_d (double *, double *, char *, long int);
extern  int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern  int scicos_sciwin (void), dr1scicos_ (char *, char *,
							      int *, int *,
							      int *, int *,
							      int *, int *,
							      double *,
							      double *,
							      double *,
							      double *,
							      long int,
							      long int);
extern  int scicos_isort (int *, int *, int *);
extern  int scicos_isort (int *, int *, int *);
extern  int scicos_getlabel (int *, char *, int *, long int);
extern  int scicos_dset (int *, double *, double *, int *);
extern  int plot2scicos_d (double *, double *, int *, int *,
			   int *, char *, char *, double *,
			   int *, long int, long int),
setscale2scicos_d (double *, double *, char *, long int);
extern  int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern  int scicos_sciwin (void), dr1scicos_ (char *, char *,
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
extern  int scicos_getlabel (int *, char *, int *, long int),
scicos_dset (int *, double *, double *, int *);
extern int setscale2scicos_d (double *, double *, char *, long int);
extern  int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern  int scicos_sciwin (void), dr1scicos_ (char *, char *,
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
extern  int scicos_getlabel (int *, char *, int *, long int);
extern  int scicos_sxevents (void);
extern  int scicos_unsfdcopy (int *, double *, int *,
					      double *, int *),
setscale2scicos_d (double *, double *, char *, long int);
extern  int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern  int scicos_sciwin (void), dr1scicos_ (char *, char *,
							      int *, int *,
							      int *, int *,
							      int *, int *,
							      double *,
							      double *,
							      double *,
							      double *,
							      long int,
							      long int);
extern  int scicos_getlabel (int *, char *, int *, long int);
extern  int scicos_sxevents (void);
extern  int setscale2scicos_d (double *, double *, char *, long int);
extern  int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern  int scicos_sciwin (void), dr1scicos_ (char *, char *,
							      int *, int *,
							      int *, int *,
							      int *, int *,
							      double *,
							      double *,
							      double *,
							      double *,
							      long int,
							      long int);
extern  int scicos_isort (int *, int *, int *);

extern int  dmmul_scicos(double *a, int *na, double *b, int *nb, double *c__, int *nc, int *l, int *m, int *n);


extern int dmmul1_scicos(double *a, int *na, double *b, int *nb, double *c__, int *nc, int *l, int *m, int *n);


#endif 
