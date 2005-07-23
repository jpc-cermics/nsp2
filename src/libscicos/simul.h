/* Nsp
 * Copyright (C) 2005 Jean-Philippe Chancelier Enpc/Cermics
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

#ifndef NSP_SCICOS_SIMUL_H
#define NSP_SCICOS_SIMUL_H

#include "nsp/object.h"

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
};

typedef struct _scicos_state scicos_state ;

/* Warning: take care to follow the same order as in 
 * the state variable of the interface 
 */

struct _scicos_state {
  /* arguments in the same order as in state */
  double *x;
  double *z;
  double *iz;
  double *tevts;
  int *evtspt;
  int *pointi;
  double *outtb;
  /* extra arguments */
  void * State; /* original hash table */
  void * State_elts[7]; /* keep track of original data */
  int nevts; 
  int nout;
  int *iwa;
};

typedef struct _scicos_sim scicos_sim ;

/* Warning: take care to follow the same order as in 
 * the sim variable of the interface 
 */

struct _scicos_sim {
  /* arguments in the same order as in sim */
  void *funs;
  int *xptr;
  int *zptr;
  int *zcptr;
  int *inpptr;
  int *outptr;
  int *inplnk;
  int *outlnk;
  int *lnkptr;
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
  /* extra elements */
  void * Sim; /* original hash table */
  void *Sim_elts[30]; /* keep track of original data */
  /* extra arguments allocated  */
  int *funflag;
  void **funptr; 
  int *mod ; 
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
  void *scsptr; /* external function to be used */
  int nclock;
  int *neq;
  double Atol, rtol, ttol, deltat,hmax;
  int hot;
};


typedef struct _scicos_run scicos_run;
typedef enum { run_on , run_off } scicos_run_status;

struct _scicos_run {
  scicos_run_status  status ;
  scicos_sim sim;
  scicos_state state;
  scicos_block *Blocks;
  scicos_params params;
};



extern int scicos_fill_run(scicos_run *sr,NspHash *Sim,NspHash *State);
extern void scicos_clear_run(scicos_run *sr);
extern int scicos_main( scicos_run *sr, double *t0_in, double *tf_in, double *simpar, int *flag__, int *ierr_out);

#endif 

