/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGVariant
#define NSP_INC_NspGVariant

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

/* NspGVariant */

#include <nsp/object.h>

/*
 * NspGVariant inherits from Object
 */

typedef struct _NspGVariant NspGVariant ;
typedef struct _NspTypeGVariant NspTypeGVariant ;

struct _NspTypeGVariant {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_gvariant nsp_gvariant;
struct _nsp_gvariant {
  GVariant* value;
  int ref_count;
};

struct _NspGVariant {
  /*< private >*/
  NspObject father;
  NspTypeGVariant*type;
  /*< public >*/
  nsp_gvariant *obj;
};

extern int nsp_type_gvariant_id;
extern NspTypeGVariant *nsp_type_gvariant;

/* type instances for object */

NspTypeGVariant *new_type_gvariant(type_mode mode);

/* instance for NspGVariant */

NspGVariant *new_gvariant();

/*
 * Object methods redefined for gvariant 
 */


#define NULLGVARIANT (NspGVariant*) 0

extern NspGVariant *nsp_gvariant_create(const char *name,GVariant* value,NspTypeBase *type);
extern NspGVariant *nsp_gvariant_create_default(const char *name);

/* from NspGVariantObj.c */

extern NspGVariant *nsp_gvariant_copy(NspGVariant *H);
extern void nsp_gvariant_destroy(NspGVariant *H);
extern int nsp_gvariant_info(NspGVariant *H, int indent,const char *name, int rec_level);
extern int nsp_gvariant_print(NspGVariant *H, int indent,const char *name, int rec_level);
extern int nsp_gvariant_latex(NspGVariant *H, int indent,const char *name, int rec_level);
extern NspGVariant *nsp_gvariant_object (NspObject *O);
extern int IsGVariantObj (Stack stack, int i);
extern int IsGVariant(NspObject *O);
extern NspGVariant *GetGVariantCopy (Stack stack, int i);
extern NspGVariant *GetGVariant (Stack stack, int i);
extern int nsp_gvariant_create_partial(NspGVariant *H);
extern void nsp_gvariant_destroy_partial(NspGVariant *H);
extern NspGVariant * nsp_gvariant_copy_partial(NspGVariant *H,NspGVariant *self);
extern NspGVariant * nsp_gvariant_full_copy_partial(NspGVariant *H,NspGVariant *self);
extern NspGVariant * nsp_gvariant_full_copy(NspGVariant *self);
extern int nsp_gvariant_check_values(NspGVariant *H);
extern int int_gvariant_create(Stack stack, int rhs, int opt, int lhs);
extern NspGVariant *nsp_gvariant_xdr_load_partial(XDR *xdrs, NspGVariant *M);
extern int nsp_gvariant_xdr_save(XDR  *xdrs, NspGVariant *M);

#line 46 "codegen/glib.override"
GVariant *nsp_copy_GVariant(GVariant *gv);

#line 102 "./gvariant.h"
#endif /* NSP_INC_NspGVariant */ 

#ifdef NspGVariant_Private 
static int init_gvariant(NspGVariant *o,NspTypeGVariant *type);
static int nsp_gvariant_size(NspGVariant *Mat, int flag);
static char *nsp_gvariant_type_as_string(void);
static char *nsp_gvariant_type_short_string(NspObject *v);
static int nsp_gvariant_eq(NspGVariant *A, NspObject *B);
static int nsp_gvariant_neq(NspGVariant *A, NspObject *B);
static NspGVariant *nsp_gvariant_xdr_load(XDR *xdrs);
static AttrTab gvariant_attrs[];
static NspMethods *gvariant_get_methods(void);
/* static int int_gvariant_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGVariant *nsp_gvariant_create_void(const char *name,NspTypeBase *type);
#line 30 "codegen/glib.override"

static int nsp_destroy_GVariant(GVariant *v,NspGVariant *H);
static int nsp_print_GVariant(int indent,GVariant *v,NspGVariant *M);
static int nsp_check_GVariant(GVariant *v,NspGVariant *H);
static int nsp_GVariant_full_copy(NspGVariant *H,GVariant *v,NspGVariant *self);

#line 124 "./gvariant.h"
#endif /* NspGVariant_Private */

