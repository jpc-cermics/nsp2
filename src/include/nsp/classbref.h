/* -*- Mode: C -*- */
#ifndef NSP_INC_NspClassBRef
#define NSP_INC_NspClassBRef

/*
 * Copyright (C) 1998-2010 Jean-Philippe Chancelier Enpc/Cermics
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

/* NspClassBRef */

#include <nsp/classaref.h>

/*
 * NspClassBRef inherits from ClassARef
 */

typedef struct _NspClassBRef NspClassBRef ;
typedef struct _NspTypeClassBRef NspTypeClassBRef ;

#line 36 "./classbref.h"

struct _NspTypeClassBRef {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 43 "./classbref.h"
};

typedef struct _nsp_classbref nsp_classbref;
struct _nsp_classbref {
  int clb_color;
  int clb_thickness;
  NspMatrix* clb_val;
  int ref_count;
};

struct _NspClassBRef {
  /*< private >*/
  NspClassARef father;
  NspTypeClassBRef*type;
  /*< public >*/
  nsp_classbref *obj;
};

extern int nsp_type_classbref_id;
extern NspTypeClassBRef *nsp_type_classbref;

/* type instances for classaref */

NspTypeClassBRef *new_type_classbref(type_mode mode);

/* instance for NspClassBRef */

NspClassBRef *new_classbref();

/*
 * Object methods redefined for classbref 
 */


#define NULLCLASSBREF (NspClassBRef*) 0

extern NspClassBRef *nsp_classbref_create(char *name,int clb_color,int clb_thickness,NspMatrix* clb_val,NspTypeBase *type);
extern NspClassBRef *nsp_classbref_create_default(char *name);

/* from NspClassBRefObj.c */

extern NspClassBRef *nsp_classbref_copy(NspClassBRef *H);
extern void nsp_classbref_destroy(NspClassBRef *H);
extern int nsp_classbref_info(NspClassBRef *H, int indent,const char *name, int rec_level);
extern int nsp_classbref_print(NspClassBRef *H, int indent,const char *name, int rec_level);
extern int nsp_classbref_latex(NspClassBRef *H, int indent,const char *name, int rec_level);
extern NspClassBRef *nsp_classbref_object (NspObject *O);
extern int IsClassBRefObj (Stack stack, int i);
extern int IsClassBRef(NspObject *O);
extern NspClassBRef *GetClassBRefCopy (Stack stack, int i);
extern NspClassBRef *GetClassBRef (Stack stack, int i);
extern int nsp_classbref_create_partial(NspClassBRef *H);
extern void nsp_classbref_destroy_partial(NspClassBRef *H);
extern NspClassBRef * nsp_classbref_copy_partial(NspClassBRef *H,NspClassBRef *self);
extern NspClassBRef * nsp_classbref_full_copy_partial(NspClassBRef *H,NspClassBRef *self);
extern NspClassBRef * nsp_classbref_full_copy(NspClassBRef *self);
extern int nsp_classbref_check_values(NspClassBRef *H);
extern int int_classbref_create(Stack stack, int rhs, int opt, int lhs);
extern NspClassBRef *nsp_classbref_xdr_load_partial(XDR *xdrs, NspClassBRef *M);
extern int nsp_classbref_xdr_save(XDR  *xdrs, NspClassBRef *M);

#endif /* NSP_INC_NspClassBRef */ 

#ifdef NspClassBRef_Private 
static int init_classbref(NspClassBRef *o,NspTypeClassBRef *type);
static int nsp_classbref_size(NspClassBRef *Mat, int flag);
static char *nsp_classbref_type_as_string(void);
static char *nsp_classbref_type_short_string(NspObject *v);
static int nsp_classbref_eq(NspClassBRef *A, NspObject *B);
static int nsp_classbref_neq(NspClassBRef *A, NspObject *B);
static NspClassBRef *nsp_classbref_xdr_load(XDR *xdrs);
static AttrTab classbref_attrs[];
static NspMethods *classbref_get_methods(void);
/* static int int_classbref_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspClassBRef *nsp_classbref_create_void(char *name,NspTypeBase *type);
#endif /* NspClassBRef_Private */

