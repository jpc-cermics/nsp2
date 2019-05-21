/* -*- Mode: C -*- */
#ifndef NSP_INC_NspStochdec
#define NSP_INC_NspStochdec

/*
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 */

#line 4 "codegen/stochdec.override"

#line 27 "./stochdec.h"
/* NspStochdec */

#include <nsp/object.h>

/*
 * NspStochdec inherits from Object
 */

typedef struct _NspStochdec NspStochdec ;
typedef struct _NspTypeStochdec NspTypeStochdec ;

struct _NspTypeStochdec {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspStochdec {
  /*< private >*/
  NspObject father;
  NspTypeStochdec*type;
  /*< public >*/
    int xdim;
};

extern int nsp_type_stochdec_id;
extern NspTypeStochdec *nsp_type_stochdec;

/* type instances for object */

NspTypeStochdec *new_type_stochdec(type_mode mode);

/* instance for NspStochdec */

NspStochdec *new_stochdec();

/*
 * Object methods redefined for stochdec 
 */


#define NULLSTOCHDEC (NspStochdec*) 0

extern NspStochdec *nsp_stochdec_create(const char *name,int xdim,NspTypeBase *type);
extern NspStochdec *nsp_stochdec_create_default(const char *name);

/* from NspStochdecObj.c */

extern NspStochdec *nsp_stochdec_copy(NspStochdec *H);
extern void nsp_stochdec_destroy(NspStochdec *H);
extern int nsp_stochdec_info(NspStochdec *H, int indent,const char *name, int rec_level);
extern int nsp_stochdec_print(NspStochdec *H, int indent,const char *name, int rec_level);
extern int nsp_stochdec_latex(NspStochdec *H, int indent,const char *name, int rec_level);
extern NspStochdec *nsp_stochdec_object (NspObject *O);
extern int IsStochdecObj (Stack stack, int i);
extern int IsStochdec(NspObject *O);
extern NspStochdec *GetStochdecCopy (Stack stack, int i);
extern NspStochdec *GetStochdec (Stack stack, int i);
extern int nsp_stochdec_create_partial(NspStochdec *H);
extern void nsp_stochdec_destroy_partial(NspStochdec *H);
extern NspStochdec * nsp_stochdec_copy_partial(NspStochdec *H,NspStochdec *self);
extern NspStochdec * nsp_stochdec_full_copy_partial(NspStochdec *H,NspStochdec *self);
extern NspStochdec * nsp_stochdec_full_copy(NspStochdec *self);
extern int nsp_stochdec_check_values(NspStochdec *H);
extern int int_stochdec_create(Stack stack, int rhs, int opt, int lhs);
extern NspStochdec *nsp_stochdec_xdr_load_partial(XDR *xdrs, NspStochdec *M);
extern int nsp_stochdec_xdr_save(XDR  *xdrs, NspStochdec *M);

#line 7 "codegen/stochdec.override"
/* inserted at the end of public part of include file
 */
#include <nsp/valuefn.h>
#include <nsp/gridvaluefn.h>
#include <nsp/cutsvaluefn.h>

extern int nsp_gvf_check_nx(NspMatrix *nx);
extern NspMatrix *nsp_gvf_create_steps(NspMatrix *nx,NspMatrix *xmin,NspMatrix *xmax);
extern NspMatrix *nsp_gvf_create_values(NspMatrix *nx);
extern int nsp_gvf_ind_to_point(NspGridValueFn *Gvf,double pt[], int i);
extern int nsp_gvf_point_to_ind(NspGridValueFn *Gvf,const double pt[]) ;
extern double nsp_gvf_get_i_value(NspGridValueFn *Gvf, int i);
extern void nsp_gvf_set_i_value(NspGridValueFn *Gvf, int i,const double val);
extern double nsp_gvf_get_pt_value(NspGridValueFn *Gvf,const double pt[]) ;
extern void nsp_gvf_set_pt_value(NspGridValueFn *Gvf,const double pt[],const double val) ;
extern NspMatrix *nsp_gvf_get_nx(NspGridValueFn *Gvf);
extern NspMatrix *nsp_gvf_get_imax(NspGridValueFn *Gvf);
extern NspGridValueFn *nsp_gvf_create(const char *name, NspMatrix *nx,NspMatrix *xmin,NspMatrix *xmax, int use_values);


extern int nsp_cvf_add_slopes(NspCutsValueFn *Cvf,NspMatrix *height,NspMatrix *slopes);
extern double nsp_cvf_get_value(NspCutsValueFn *Cvf,const double pt[]) ;
extern int nsp_cvf_add_slope(NspCutsValueFn *Cvf,double height,double slope[]);
extern int nsp_ind_to_point(NspMatrix *pt, int i, NspMatrix *min,NspMatrix *nx, NspMatrix *step, int t);
extern NspMatrix *nsp_cvf_get_slopes(NspCutsValueFn *self);
extern NspMatrix *nsp_cvf_get_heights(NspCutsValueFn *self);

#line 124 "./stochdec.h"
#endif /* NSP_INC_NspStochdec */ 

#ifdef NspStochdec_Private 
static int init_stochdec(NspStochdec *o,NspTypeStochdec *type);
static int nsp_stochdec_size(NspStochdec *Mat, int flag);
static char *nsp_stochdec_type_as_string(void);
static char *nsp_stochdec_type_short_string(NspObject *v);
static int nsp_stochdec_eq(NspStochdec *A, NspObject *B);
static int nsp_stochdec_neq(NspStochdec *A, NspObject *B);
static NspStochdec *nsp_stochdec_xdr_load(XDR *xdrs);
static AttrTab stochdec_attrs[];
static NspMethods *stochdec_get_methods(void);
/* static int int_stochdec_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspStochdec *nsp_stochdec_create_void(const char *name,NspTypeBase *type);
#line 36 "codegen/stochdec.override"
/* inserted in the private part of include file
 * of ldfaure.h
 */

#line 144 "./stochdec.h"
#endif /* NspStochdec_Private */

