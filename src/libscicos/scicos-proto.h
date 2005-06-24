#ifndef NSP_SCICOS_PROTO_H
#define NSP_SCICOS_PROTO_H


extern int scicos_setblockwin (BCG *Xgc,int *win, int *cur);

extern int scicos_recterase (BCG *Xgc,const double r[]);

extern int scicos_affdraw (BCG *Xgc,const int *fontd,const int *form,const double *val,const double *r);


extern int scicos_ftree2 (int *vec, int *nb, int *deput, int *outoin,
			  int *outoinptr, int *ord, int *nord, int *ok);

extern int scicos_ftree3(int *vec, int *nb, int *deput, int *typl, int *bexe,
			  int *boptr, int *blnk, int *blptr, int *kk,
			  int *ord, int *nord, int *ok);

extern int scicos_ftree4(int *vec, int *nb, int *nd, int *nnd, int *typr,
			  int *outoin, int *outoinptr, int *r1, int *r2,
			  int *nr);

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
extern  char * scicos_getlabel (int);
extern  int scicos_dset (int *, double *, double *, int *);
extern  int plot2scicos_d (double *, double *, int *, int *,
			   int *, char *, char *, double *,
			   int *, long int, long int),
setscale2scicos_d (double *, double *, char *, long int);
extern  int scicos_dr (char *, char *, int *, int *, int *,
				       int *, int *, int *, double *,
				       double *, double *, double *, long int,
				       long int);
extern  int scicos_sciwin (void);
extern int dr1scicos_ (char *, char *,
		       int *, int *,
		       int *, int *,
		       int *, int *,
		       double *,
		       double *,
		       double *,
		       double *,
		       long int,
		       long int);
extern int scicos_scicosclip (int *);
extern int scicos_dset (int *, double *, double *, int *);
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
					      long int);
extern int scicos_scicosclip (int *);
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

extern int  scicos_getscicosvars(int what, double **v, int *nv, int *type);

extern int scicos_getscilabel(int kfun,char **label);
extern void scicos_clip(BCG *Xgc,int n) ;

extern void  scicos_makescicosimport(double *x, int *xptr, int *zcptr, double *z, int *zptr, int *mod, int *modptr, char **names, int *inpptr, int *inplnk, int *outptr, int *outlnk, int *lnkptr, int *nlnkptr, double *rpar, int *rpptr, int *ipar, int *ipptr, int *nblk, double *outtb, int *nout, int *subs, int *nsubs, double *tevts, int *evtspt, int *nevts, int *pointi, int *oord, int *zord, int *funptr, int *funtyp, int *ztyp, int *cord, int *ordclk, int *clkptr, int *ordptr, int *critev, int *iwa);

extern int  scicos_getscicosvars(int what, double **v, int *nv, int *type);

extern void scicos_clearscicosimport(void);

int scicos_main
(double *x_in, int *xptr_in, double *z__, double *work, int *zptr, int *modptr_in,char **names, double *t0_in, double *tf_in, double *tevts_in, int *evtspt_in, int *nevts, int *pointi_in, double *outtb_in, int *nout1,int *funflag,void **funptr, int *funtyp_in, int *inpptr_in, int *outptr_in, int *inplnk_in, int *outlnk_in, int *lnkptr_in, int *nlnkptr, double *rpar, int *rpptr, int *ipar, int *ipptr, int *clkptr_in, int *ordptr_in, int *nordptr1, int *ordclk_in, int *cord_in, int *ncord1, int *iord_in, int *niord1, int *oord_in, int *noord1, int *zord_in, int *nzord1, int *critev_in, int *nblk1, int *ztyp, int *zcptr_in, int *subscr, int *nsubs, double *simpar, int *flag__, int *ierr_out);

extern BCG *scicos_set_win(int wid,int *oldwid);

/* from Fortran library */
extern double pow_di(double *ap, int *bp);
extern double pow_dd(double *ap, double *bp);

extern void *get_function(char * fname);


#endif 
