/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGVariantType
#define NSP_INC_NspGVariantType

/*
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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

/* NspGVariantType */

#include <nsp/object.h>

/*
 * NspGVariantType inherits from Object
 */

typedef struct _NspGVariantType NspGVariantType ;
typedef struct _NspTypeGVariantType NspTypeGVariantType ;

struct _NspTypeGVariantType {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_gvarianttype nsp_gvarianttype;
struct _nsp_gvarianttype {
  GVariantType* value;
  int ref_count;
};

struct _NspGVariantType {
  /*< private >*/
  NspObject father;
  NspTypeGVariantType*type;
  /*< public >*/
  nsp_gvarianttype *obj;
};

extern int nsp_type_gvarianttype_id;
extern NspTypeGVariantType *nsp_type_gvarianttype;

/* type instances for object */

NspTypeGVariantType *new_type_gvarianttype(type_mode mode);

/* instance for NspGVariantType */

NspGVariantType *new_gvarianttype();

/*
 * Object methods redefined for gvarianttype 
 */


#define NULLGVARIANTTYPE (NspGVariantType*) 0

extern NspGVariantType *nsp_gvarianttype_create(const char *name,GVariantType* value,NspTypeBase *type);
extern NspGVariantType *nsp_gvarianttype_create_default(const char *name);

/* from NspGVariantTypeObj.c */

extern NspGVariantType *nsp_gvarianttype_copy(NspGVariantType *H);
extern void nsp_gvarianttype_destroy(NspGVariantType *H);
extern int nsp_gvarianttype_info(NspGVariantType *H, int indent,const char *name, int rec_level);
extern int nsp_gvarianttype_print(NspGVariantType *H, int indent,const char *name, int rec_level);
extern int nsp_gvarianttype_latex(NspGVariantType *H, int indent,const char *name, int rec_level);
extern NspGVariantType *nsp_gvarianttype_object (NspObject *O);
extern int IsGVariantTypeObj (Stack stack, int i);
extern int IsGVariantType(NspObject *O);
extern NspGVariantType *GetGVariantTypeCopy (Stack stack, int i);
extern NspGVariantType *GetGVariantType (Stack stack, int i);
extern int nsp_gvarianttype_create_partial(NspGVariantType *H);
extern void nsp_gvarianttype_destroy_partial(NspGVariantType *H);
extern NspGVariantType * nsp_gvarianttype_copy_partial(NspGVariantType *H,NspGVariantType *self);
extern NspGVariantType * nsp_gvarianttype_full_copy_partial(NspGVariantType *H,NspGVariantType *self);
extern NspGVariantType * nsp_gvarianttype_full_copy(NspGVariantType *self);
extern int nsp_gvarianttype_check_values(NspGVariantType *H);
extern int int_gvarianttype_create(Stack stack, int rhs, int opt, int lhs);
extern NspGVariantType *nsp_gvarianttype_xdr_load_partial(XDR *xdrs, NspGVariantType *M);
extern int nsp_gvarianttype_xdr_save(XDR  *xdrs, NspGVariantType *M);

#line 50 "codegen/glib.override"
GVariantType *nsp_copy_GVariantType(const GVariantType *gv);

#line 102 "./gvarianttype.h"
#endif /* NSP_INC_NspGVariantType */ 

#ifdef NspGVariantType_Private 
static int init_gvarianttype(NspGVariantType *o,NspTypeGVariantType *type);
static int nsp_gvarianttype_size(NspGVariantType *Mat, int flag);
static char *nsp_gvarianttype_type_as_string(void);
static char *nsp_gvarianttype_type_short_string(NspObject *v);
static int nsp_gvarianttype_eq(NspGVariantType *A, NspObject *B);
static int nsp_gvarianttype_neq(NspGVariantType *A, NspObject *B);
static NspGVariantType *nsp_gvarianttype_xdr_load(XDR *xdrs);
static AttrTab gvarianttype_attrs[];
static NspMethods *gvarianttype_get_methods(void);
/* static int int_gvarianttype_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGVariantType *nsp_gvarianttype_create_void(const char *name,NspTypeBase *type);
#line 38 "codegen/glib.override"

static int nsp_destroy_GVariantType(GVariantType *v,NspGVariantType *H);
static int nsp_print_GVariantType(int indent,GVariantType *v,NspGVariantType *M);
static int nsp_check_GVariantType(GVariantType *v,NspGVariantType *H);
static int nsp_GVariantType_full_copy(NspGVariantType *H,GVariantType *v,NspGVariantType *self);

#line 124 "./gvarianttype.h"
#endif /* NspGVariantType_Private */

