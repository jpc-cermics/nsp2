/* -*- Mode: C -*- */
#ifndef NSP_INC_NspCutsValueFn
#define NSP_INC_NspCutsValueFn

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

/* NspCutsValueFn */

#include <nsp/valuefn.h>

/*
 * NspCutsValueFn inherits from ValueFn
 */

typedef struct _NspCutsValueFn NspCutsValueFn ;
typedef struct _NspTypeCutsValueFn NspTypeCutsValueFn ;

struct _NspTypeCutsValueFn {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspCutsValueFn {
  /*< private >*/
  NspValueFn father;
  NspTypeCutsValueFn*type;
  /*< public >*/
    NspMatrix* heights;
  NspMatrix* slopes;
};

extern int nsp_type_cutsvaluefn_id;
extern NspTypeCutsValueFn *nsp_type_cutsvaluefn;

/* type instances for valuefn */

NspTypeCutsValueFn *new_type_cutsvaluefn(type_mode mode);

/* instance for NspCutsValueFn */

NspCutsValueFn *new_cutsvaluefn();

/*
 * Object methods redefined for cutsvaluefn 
 */


#define NULLCUTSVALUEFN (NspCutsValueFn*) 0

extern NspCutsValueFn *nsp_cutsvaluefn_create(const char *name,NspMatrix* heights,NspMatrix* slopes,NspTypeBase *type);
extern NspCutsValueFn *nsp_cutsvaluefn_create_default(const char *name);

/* from NspCutsValueFnObj.c */

extern NspCutsValueFn *nsp_cutsvaluefn_copy(NspCutsValueFn *H);
extern void nsp_cutsvaluefn_destroy(NspCutsValueFn *H);
extern int nsp_cutsvaluefn_info(NspCutsValueFn *H, int indent,const char *name, int rec_level);
extern int nsp_cutsvaluefn_print(NspCutsValueFn *H, int indent,const char *name, int rec_level);
extern int nsp_cutsvaluefn_latex(NspCutsValueFn *H, int indent,const char *name, int rec_level);
extern NspCutsValueFn *nsp_cutsvaluefn_object (NspObject *O);
extern int IsCutsValueFnObj (Stack stack, int i);
extern int IsCutsValueFn(NspObject *O);
extern NspCutsValueFn *GetCutsValueFnCopy (Stack stack, int i);
extern NspCutsValueFn *GetCutsValueFn (Stack stack, int i);
extern int nsp_cutsvaluefn_create_partial(NspCutsValueFn *H);
extern void nsp_cutsvaluefn_destroy_partial(NspCutsValueFn *H);
extern NspCutsValueFn * nsp_cutsvaluefn_copy_partial(NspCutsValueFn *H,NspCutsValueFn *self);
extern NspCutsValueFn * nsp_cutsvaluefn_full_copy_partial(NspCutsValueFn *H,NspCutsValueFn *self);
extern NspCutsValueFn * nsp_cutsvaluefn_full_copy(NspCutsValueFn *self);
extern int nsp_cutsvaluefn_check_values(NspCutsValueFn *H);
extern int int_cutsvaluefn_create(Stack stack, int rhs, int opt, int lhs);
extern NspCutsValueFn *nsp_cutsvaluefn_xdr_load_partial(XDR *xdrs, NspCutsValueFn *M);
extern int nsp_cutsvaluefn_xdr_save(XDR  *xdrs, NspCutsValueFn *M);

#endif /* NSP_INC_NspCutsValueFn */ 

#ifdef NspCutsValueFn_Private 
static int init_cutsvaluefn(NspCutsValueFn *o,NspTypeCutsValueFn *type);
static int nsp_cutsvaluefn_size(NspCutsValueFn *Mat, int flag);
static char *nsp_cutsvaluefn_type_as_string(void);
static char *nsp_cutsvaluefn_type_short_string(NspObject *v);
static int nsp_cutsvaluefn_eq(NspCutsValueFn *A, NspObject *B);
static int nsp_cutsvaluefn_neq(NspCutsValueFn *A, NspObject *B);
static NspCutsValueFn *nsp_cutsvaluefn_xdr_load(XDR *xdrs);
static AttrTab cutsvaluefn_attrs[];
static NspMethods *cutsvaluefn_get_methods(void);
/* static int int_cutsvaluefn_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspCutsValueFn *nsp_cutsvaluefn_create_void(const char *name,NspTypeBase *type);
#endif /* NspCutsValueFn_Private */

