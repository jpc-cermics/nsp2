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

extern int scicos_sctree (int *nb, int *vec, int *in, int *depu, int *outptr, int *cmat,
			  int *ord, int *nord, int *ok, int *kk);

extern  int scicos_dset (int *, double *, double *, int *);
extern void scicos_getouttb(int nsize,int *nvec, double *outtc);
extern  int setscale2scicos_d (double *, double *, char *, long int);
extern  int scicos_sciwin (void);
extern  int scicos_isort (int *, int *, int *);
extern  char *scicos_getlabel(int kf);
extern  int scicos_dset (int *, double *, double *, int *);
extern  int plot2scicos_d (double *, double *, int *, int *,
			   int *, char *, char *, double *,
			   int *, long int, long int);
extern int scicos_scicosclip (int *);
extern  int scicos_sxevents (void);
extern  int scicos_unsfdcopy (int *, double *, int *, double *, int *);
extern  int scicos_isort (int *, int *, int *);
extern int  dmmul_scicos(double *a, int *na, double *b, int *nb, double *c__, int *nc, int *l, int *m, int *n);
extern int dmmul1_scicos(double *a, int *na, double *b, int *nb, double *c__, int *nc, int *l, int *m, int *n);
extern int  scicos_getscicosvars(int what, double **v, int *nv, int *type);
extern int scicos_getscilabel(int kfun,char **label);
extern void scicos_clip(BCG *Xgc,int n) ;
extern BCG *scicos_set_win(int wid,int *oldwid);
extern void *get_function(char * fname);
extern void do_cold_restart(void);
extern int get_phase_simulation(void);
extern double get_scicos_time(void);
extern int get_block_number(void);
extern void set_block_error(int);
extern void set_pointer_xproperty(int* pointer);
extern void * scicos_malloc(size_t );
extern void scicos_free(void *p);

/* FIXME should be defined elsewhere */
extern int nsp_check_events_activated(void);
extern int nsp_check_gtk_events(void);

#endif 
