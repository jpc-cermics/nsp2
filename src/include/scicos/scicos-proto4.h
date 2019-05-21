/* Nsp
 * Copyright (C) 2011-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 * prototypes of scicos functions 
 *--------------------------------------------------------------------------*/

#ifndef NSP_SCICOS_PROTO4_H
#define NSP_SCICOS_PROTO4_H

#include <nsp/nsp.h>

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

extern int scicos_dset (int *, double *, double *, int *);
extern void scicos_getouttb(int nsize,int *nvec, double *outtc);
extern int setscale2scicos_d (double *, double *, char *, long int);
extern int scicos_sciwin (void);
extern int scicos_isort (int *, int *, int *);
extern char *scicos_getlabel(int kf);
extern int scicos_dset (int *, double *, double *, int *);
extern int plot2scicos_d (double *, double *, int *, int *,
			  int *, char *, char *, double *,
			  int *, long int, long int);
extern int scicos_scicosclip (int *);
extern int scicos_sxevents (void);
extern int scicos_unsfdcopy (int *, double *, int *, double *, int *);
extern int scicos_isort (int *, int *, int *);
extern int  scicos_getscicosvars(int what, double **v, int *nv, int *type);
extern int scicos_getscilabel(int kfun,char **label);
extern void *scicos_get_function(char * fname);
extern void scicos_do_cold_restart(void);
extern int scicos_get_phase_simulation(void);
extern double scicos_get_scicos_time(void);
extern int scicos_get_block_number(void);
extern void scicos_set_block_error(int);
extern void scicos_set_pointer_xproperty(int* pointer);
extern char *scicos_get_label(int kf);
extern int scicos_get_block_by_label(const char *label);
extern int scicos_get_fcaller_id(void);
extern void scicos_end_scicos_sim(void);
extern void scicos_set_block_number(int kfun);
extern int scicos_get_block_error (void);

/* nsp events */

extern int nsp_check_events_activated(void);
extern int nsp_check_gtk_events(void);

/* should be a define */
extern void Coserror (char *fmt, ...);

/* utility functions used in blocks */

extern void *scicos_malloc(size_t size);
extern void scicos_free(void *p);
extern int scicos_mtran(double *a, int na, double *b, int nb, int m, int n);
extern void Set_Jacobian_flag (int flag);

/* extern used by some modelica blocks */ 

extern double exp_(double x);
extern double log_(double x);
extern double pow_(double x, double y);

/* utilities used in intcos */

extern int scicos_getblock(NspObject *obj,double *pt,int *k);
extern int scicos_getblocklink(NspObject *obj,double *pt,int *k, int *wh);
extern void scicos_getobjs_in_rect(NspList *objs,double ox,double oy,double w,double h,
				   int *nin,double *in,int *nout,double *out);
extern int scicos_getobj(NspObject *obj,const double *pt,int *k, int *wh);

#endif 
