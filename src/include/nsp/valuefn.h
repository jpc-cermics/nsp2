/* -*- Mode: C -*- */
#ifndef NSP_INC_NspValueFn
#define NSP_INC_NspValueFn

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

/* NspValueFn */

#include <nsp/object.h>

/*
 * NspValueFn inherits from Object
 */

typedef struct _NspValueFn NspValueFn ;
typedef struct _NspTypeValueFn NspTypeValueFn ;

struct _NspTypeValueFn {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspValueFn {
  /*< private >*/
  NspObject father;
  NspTypeValueFn*type;
  /*< public >*/
    int xdim;
  NspMatrix* xmin;
  NspMatrix* xmax;
};

extern int nsp_type_valuefn_id;
extern NspTypeValueFn *nsp_type_valuefn;

/* type instances for object */

NspTypeValueFn *new_type_valuefn(type_mode mode);

/* instance for NspValueFn */

NspValueFn *new_valuefn();

/*
 * Object methods redefined for valuefn 
 */


#define NULLVALUEFN (NspValueFn*) 0

extern NspValueFn *nsp_valuefn_create(const char *name,int xdim,NspMatrix* xmin,NspMatrix* xmax,NspTypeBase *type);
extern NspValueFn *nsp_valuefn_create_default(const char *name);

/* from NspValueFnObj.c */

extern NspValueFn *nsp_valuefn_copy(NspValueFn *H);
extern void nsp_valuefn_destroy(NspValueFn *H);
extern int nsp_valuefn_info(NspValueFn *H, int indent,const char *name, int rec_level);
extern int nsp_valuefn_print(NspValueFn *H, int indent,const char *name, int rec_level);
extern int nsp_valuefn_latex(NspValueFn *H, int indent,const char *name, int rec_level);
extern NspValueFn *nsp_valuefn_object (NspObject *O);
extern int IsValueFnObj (Stack stack, int i);
extern int IsValueFn(NspObject *O);
extern NspValueFn *GetValueFnCopy (Stack stack, int i);
extern NspValueFn *GetValueFn (Stack stack, int i);
extern int nsp_valuefn_create_partial(NspValueFn *H);
extern void nsp_valuefn_destroy_partial(NspValueFn *H);
extern NspValueFn * nsp_valuefn_copy_partial(NspValueFn *H,NspValueFn *self);
extern NspValueFn * nsp_valuefn_full_copy_partial(NspValueFn *H,NspValueFn *self);
extern NspValueFn * nsp_valuefn_full_copy(NspValueFn *self);
extern int nsp_valuefn_check_values(NspValueFn *H);
extern int int_valuefn_create(Stack stack, int rhs, int opt, int lhs);
extern NspValueFn *nsp_valuefn_xdr_load_partial(XDR *xdrs, NspValueFn *M);
extern int nsp_valuefn_xdr_save(XDR  *xdrs, NspValueFn *M);

#endif /* NSP_INC_NspValueFn */ 

#ifdef NspValueFn_Private 
static int init_valuefn(NspValueFn *o,NspTypeValueFn *type);
static int nsp_valuefn_size(NspValueFn *Mat, int flag);
static char *nsp_valuefn_type_as_string(void);
static char *nsp_valuefn_type_short_string(NspObject *v);
static int nsp_valuefn_eq(NspValueFn *A, NspObject *B);
static int nsp_valuefn_neq(NspValueFn *A, NspObject *B);
static NspValueFn *nsp_valuefn_xdr_load(XDR *xdrs);
static AttrTab valuefn_attrs[];
static NspMethods *valuefn_get_methods(void);
/* static int int_valuefn_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspValueFn *nsp_valuefn_create_void(const char *name,NspTypeBase *type);
#endif /* NspValueFn_Private */

