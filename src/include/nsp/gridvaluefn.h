/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGridValueFn
#define NSP_INC_NspGridValueFn

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

/* NspGridValueFn */

#include <nsp/valuefn.h>

/*
 * NspGridValueFn inherits from ValueFn
 */

typedef struct _NspGridValueFn NspGridValueFn ;
typedef struct _NspTypeGridValueFn NspTypeGridValueFn ;

struct _NspTypeGridValueFn {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspGridValueFn {
  /*< private >*/
  NspValueFn father;
  NspTypeGridValueFn*type;
  /*< public >*/
    NspMatrix* n;
  NspMatrix* step;
  NspMatrix* values;
};

extern int nsp_type_gridvaluefn_id;
extern NspTypeGridValueFn *nsp_type_gridvaluefn;

/* type instances for valuefn */

NspTypeGridValueFn *new_type_gridvaluefn(type_mode mode);

/* instance for NspGridValueFn */

NspGridValueFn *new_gridvaluefn();

/*
 * Object methods redefined for gridvaluefn 
 */


#define NULLGRIDVALUEFN (NspGridValueFn*) 0

extern NspGridValueFn *nsp_gridvaluefn_create(const char *name,NspMatrix* n,NspMatrix* step,NspMatrix* values,NspTypeBase *type);
extern NspGridValueFn *nsp_gridvaluefn_create_default(const char *name);

/* from NspGridValueFnObj.c */

extern NspGridValueFn *nsp_gridvaluefn_copy(NspGridValueFn *H);
extern void nsp_gridvaluefn_destroy(NspGridValueFn *H);
extern int nsp_gridvaluefn_info(NspGridValueFn *H, int indent,const char *name, int rec_level);
extern int nsp_gridvaluefn_print(NspGridValueFn *H, int indent,const char *name, int rec_level);
extern int nsp_gridvaluefn_latex(NspGridValueFn *H, int indent,const char *name, int rec_level);
extern NspGridValueFn *nsp_gridvaluefn_object (NspObject *O);
extern int IsGridValueFnObj (Stack stack, int i);
extern int IsGridValueFn(NspObject *O);
extern NspGridValueFn *GetGridValueFnCopy (Stack stack, int i);
extern NspGridValueFn *GetGridValueFn (Stack stack, int i);
extern int nsp_gridvaluefn_create_partial(NspGridValueFn *H);
extern void nsp_gridvaluefn_destroy_partial(NspGridValueFn *H);
extern NspGridValueFn * nsp_gridvaluefn_copy_partial(NspGridValueFn *H,NspGridValueFn *self);
extern NspGridValueFn * nsp_gridvaluefn_full_copy_partial(NspGridValueFn *H,NspGridValueFn *self);
extern NspGridValueFn * nsp_gridvaluefn_full_copy(NspGridValueFn *self);
extern int nsp_gridvaluefn_check_values(NspGridValueFn *H);
extern int int_gridvaluefn_create(Stack stack, int rhs, int opt, int lhs);
extern NspGridValueFn *nsp_gridvaluefn_xdr_load_partial(XDR *xdrs, NspGridValueFn *M);
extern int nsp_gridvaluefn_xdr_save(XDR  *xdrs, NspGridValueFn *M);

#endif /* NSP_INC_NspGridValueFn */ 

#ifdef NspGridValueFn_Private 
static int init_gridvaluefn(NspGridValueFn *o,NspTypeGridValueFn *type);
static int nsp_gridvaluefn_size(NspGridValueFn *Mat, int flag);
static char *nsp_gridvaluefn_type_as_string(void);
static char *nsp_gridvaluefn_type_short_string(NspObject *v);
static int nsp_gridvaluefn_eq(NspGridValueFn *A, NspObject *B);
static int nsp_gridvaluefn_neq(NspGridValueFn *A, NspObject *B);
static NspGridValueFn *nsp_gridvaluefn_xdr_load(XDR *xdrs);
static AttrTab gridvaluefn_attrs[];
static NspMethods *gridvaluefn_get_methods(void);
/* static int int_gridvaluefn_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGridValueFn *nsp_gridvaluefn_create_void(const char *name,NspTypeBase *type);
#endif /* NspGridValueFn_Private */

