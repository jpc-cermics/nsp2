/* Nsp
 * Copyright (C) 2005-2010 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * scicos objects used for simulation 
 *--------------------------------------------------------------------------*/

#ifndef NSP_SCICOS_SIMUL4_H
#define NSP_SCICOS_SIMUL4_H

#include "nsp/object.h"

/* used when a list of objects is stored in arrays */

typedef struct _outtb_el outtb_el;

struct  _outtb_el {
  int lnk;
  int pos;
};

typedef struct _scicos_list_flat scicos_list_flat;

struct _scicos_list_flat {
  int n;
  void **ptr;
  int *sz;
  int *type;
  int use_elems;
  int nelem; 
  outtb_el *elems;
};

typedef enum { fun_macros, fun_macro_name, fun_pointer} scicos_funflag;

typedef struct  _scicos_block scicos_block ;

struct  _scicos_block {
  int nevprt;
  void *funpt ; /* hard coded function */
  int type;
  scicos_funflag scsptr_flag ;  /* to decide if next field is a name or a macro */
  void *scsptr;  /* macros (in fact a NspObject *) or name  */
  int nz;
  double *z;
  int nx;
  double *x;
  double *xd;
  double *res;
  int nin;
  int *insz;
  double **inptr;
  int nout;
  int *outsz;
  double **outptr;
  int nevout;
  double *evout;
  int nrpar;
  double *rpar;
  int nipar;
  int *ipar;
  int ng;
  double *g;
  int ztyp;
  int *jroot;
  int *jroot_init; /* keep track of initial jroot */
  char *label;
  void **work;
  int nmode;
  int *mode;
  int noz;
  int *ozsz;
  int *oztyp;
  void **ozptr;
  int *xprop;
  int nopar;
  int *oparsz;
  int *opartyp;
  void **oparptr;
  double *alpha;
  double *beta;
};

typedef struct _scicos_sim scicos_sim ;

struct _scicos_sim {
  /* WARNING arguments following here must be in the same order as state names 
   * in scicos_fill_state 
   */
  /* --- start of state list followin state names */
  double *x;      /* continuous state */
  double *z;      /* 5  - discrete state */
  void *ozl;      /* list of nsp objects */
  double *iz;     /* 18 - vectors of labels */
  double *tevts;
  int *evtspt;
  int *pointi;
  void *outtbl;   /* list of nsp objects */
  /* -- end of state list */
  /* extra arguments for outtb */
  int nlnk;        /* length of outtbl */
  void **outtbptr; /* array of pointers to outtb data */
  int *outtbsz;    /* array giving sizes of outtb elements */
  int *outtbtyp;   /* array giving the type of outtb elemnts */
  int nelem; 
  outtb_el *elems;
  /* extra arguments for oz */
  int noz;         /* length of ozl */
  void **ozptr;    /* pointer to data */
  int *ozsz;       /* sizes */
  int *oztyp;      /* types */
  /* extra arguments */
  void * State; /* original hash table */
  void * State_elts[8]; /* keep track of original data */
  int nevts; 
  int *iwa;
  /* WARNING arguments following here must be in the same order as sim names 
   * in scicos_fill_sim
   */
  /* --- start of sim list */
  void *funs;
  int *xptr;
  int *zptr;
  int *zcptr;
  int *inpptr;
  int *outptr;
  int *inplnk;
  int *outlnk;
  int *oziptr; /* used for keeping tracks of oz: it is called ozptr in nsp csim */
  double *rpar;
  int *rpptr;
  int *ipar;
  int *ipptr;
  int *clkptr;
  int *ordptr;
  double *execlk;
  int *ordclk;
  int *cord;
  int *oord;
  int *zord;
  int *critev;
  int *nb;
  int *ztyp;
  int *nblkptr;
  int *ndcblkptr;
  int *subscr;
  int *funtyp;
  int *iord;
  nsp_string *labels;
  int *modptr;
  void *opar; /* list of objects */
  int *opptr; /* keep track of opar list */
  /* --- end of sim list */
  /* extra elements for opar */
  int nopar;
  void **oparptr;
  int *oparsz;
  int *opartyp;
  /* extra elements */
  void *Sim; /* original hash table */
  void *Sim_elts[32]; /* pointers to NspObjects of Sim  */
  /* extra arguments allocated  */
  int *funflag;
  void **funptr; 
  int *mod ; 
  int *xprop;
  double *alpha;
  double *beta;
  double *g;
  double *xd;
  /* constants */
  int nlnkptr ;/* lnkptr */
  int nordptr;/* ordptr  */
  int ncord ;/* cord */
  int niord ;/* iord */
  int noord ;/* oord */
  int nzord ;/* zord */
  int nblk ;
  int ndcblk;
  int nsubs; /* subscr */
  int nmod;
  int nordclk;
  int ng;   /*     computes number of zero crossing surfaces */
  int nz;   /*     number of  discrete real states */
  int nx;   /*     number of continuous states */
  int debug_block ; /* debug block id */
};

/*
 * scicos parameters 
 */

typedef struct _scicos_params scicos_params;

struct _scicos_params {
  int curblk; /* current activated block  */
  double scale ; /* real time  scale factor */
  int halt; /* halt event activated */
  int solver; /* solver used */
  int debug; /* debug level */
  int debug_counter; /* counting entries in debug block */
  scicos_funflag scsptr_flag ;  /* to decide if next field is a name or a macro */
  void *scsptr; /* external function to be used */
  int nclock;
  int *neq;
  double Atol, rtol, ttol, deltat,hmax;
  int hot;
  int phase; 
};


typedef struct _scicos_run scicos_run;
typedef enum { run_on , run_off } scicos_run_status;

struct _scicos_run {
  scicos_run_status  status ;
  scicos_sim sim;
  scicos_block *Blocks;
  scicos_params params;
};


extern int scicos_fill_run(scicos_run *sr,NspHash *Sim,NspHash *State);
extern void scicos_clear_run(scicos_run *sr);
extern int scicos_main( scicos_run *sr, double *t0_in, double *tf_in, double *simpar, int *flag__, int *ierr_out);


extern void  scicos_sciblk2(int *flag, int *nevprt, double *t, double *xd, double *x, int *nx, double *z,
		     int *nz, double *tvec, int *ntvec, double *rpar, int *nrpar, int *ipar, 
		     int *nipar, double **inptr, int *insz, int *nin, double **outptr, 
		     int *outsz, int *nout);
extern void scicos_sciblk4(scicos_block *Blocks, int flag);
extern void scicos_sciblk(int *flag, int *nevprt, double *t, double *xd, double *x, int *nx,
			  double *z, int *nz, double *tvec, int *ntvec, double *rpar, int *nrpar,
			  int *ipar, int *nipar, double *u, int *nu, double *y, int *ny);




#endif 

