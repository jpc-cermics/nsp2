/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGValue
#define NSP_INC_NspGValue

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

#line 14 "codegen/gvalue.override"

/* start: on windows GetGValue exists  in wingdi.h ! */
#include <nsp/objects.h>
#ifdef GetGValue
#undef GetGValue
#endif

#line 33 "./gvalue.h"
/* NspGValue */

#include <nsp/object.h>

/*
 * NspGValue inherits from Object
 */

typedef struct _NspGValue NspGValue ;
typedef struct _NspTypeGValue NspTypeGValue ;

struct _NspTypeGValue {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspGValue {
  /*< private >*/
  NspObject father;
  NspTypeGValue*type;
  /*< public >*/
    GValue value;
};

extern int nsp_type_gvalue_id;
extern NspTypeGValue *nsp_type_gvalue;

/* type instances for object */

NspTypeGValue *new_type_gvalue(type_mode mode);

/* instance for NspGValue */

NspGValue *new_gvalue();

/*
 * Object methods redefined for gvalue 
 */


#define NULLGVALUE (NspGValue*) 0

extern NspGValue *nsp_gvalue_create(const char *name,GValue value,NspTypeBase *type);
extern NspGValue *nsp_gvalue_create_default(const char *name);

/* from NspGValueObj.c */

extern NspGValue *nsp_gvalue_copy(NspGValue *H);
extern void nsp_gvalue_destroy(NspGValue *H);
extern int nsp_gvalue_info(NspGValue *H, int indent,const char *name, int rec_level);
extern int nsp_gvalue_print(NspGValue *H, int indent,const char *name, int rec_level);
extern int nsp_gvalue_latex(NspGValue *H, int indent,const char *name, int rec_level);
extern NspGValue *nsp_gvalue_object (NspObject *O);
extern int IsGValueObj (Stack stack, int i);
extern int IsGValue(NspObject *O);
extern NspGValue *GetGValueCopy (Stack stack, int i);
extern NspGValue *GetGValue (Stack stack, int i);
extern int nsp_gvalue_create_partial(NspGValue *H);
extern void nsp_gvalue_destroy_partial(NspGValue *H);
extern NspGValue * nsp_gvalue_copy_partial(NspGValue *H,NspGValue *self);
extern NspGValue * nsp_gvalue_full_copy_partial(NspGValue *H,NspGValue *self);
extern NspGValue * nsp_gvalue_full_copy(NspGValue *self);
extern int nsp_gvalue_check_values(NspGValue *H);
extern int int_gvalue_create(Stack stack, int rhs, int opt, int lhs);
extern NspGValue *nsp_gvalue_xdr_load_partial(XDR *xdrs, NspGValue *M);
extern int nsp_gvalue_xdr_save(XDR  *xdrs, NspGValue *M);

#line 23 "codegen/gvalue.override"

/* public: on windows GetGValue exists  in wingdi.h ! */
#include <nsp/objects.h>
#ifdef GetGValue
#undef GetGValue
#endif

#line 110 "./gvalue.h"
#endif /* NSP_INC_NspGValue */ 

#ifdef NspGValue_Private 
static int init_gvalue(NspGValue *o,NspTypeGValue *type);
static int nsp_gvalue_size(NspGValue *Mat, int flag);
static char *nsp_gvalue_type_as_string(void);
static char *nsp_gvalue_type_short_string(NspObject *v);
static int nsp_gvalue_eq(NspGValue *A, NspObject *B);
static int nsp_gvalue_neq(NspGValue *A, NspObject *B);
static NspGValue *nsp_gvalue_xdr_load(XDR *xdrs);
static AttrTab gvalue_attrs[];
static NspMethods *gvalue_get_methods(void);
/* static int int_gvalue_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGValue *nsp_gvalue_create_void(const char *name,NspTypeBase *type);
#line 32 "codegen/gvalue.override"

/* private: */
static void nsp_init_GValue(GValue *value);
static int nsp_eq_GValue(GValue *v1, GValue *v2);
static int nsp_GValue_full_copy(NspGValue *H,GValue *v,NspGValue *self);
static int nsp_print_GValue(int indent,GValue *v,NspGValue *M);
static int nsp_destroy_GValue(GValue *v,NspGValue *H);
static int nsp_check_GValue(GValue *v,NspGValue *H);
static int nsp_fill_g_value_from_nspobject(GValue *value, NspObject *obj);

#line 136 "./gvalue.h"
#endif /* NspGValue_Private */

