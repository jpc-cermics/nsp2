/* -*- Mode: C -*- */
#ifndef NSP_INC_NspClassARef
#define NSP_INC_NspClassARef

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

/* NspClassARef */

#include <nsp/object.h>

/*
 * NspClassARef inherits from Object
 */

typedef struct _NspClassARef NspClassARef ;
typedef struct _NspTypeClassARef NspTypeClassARef ;

#line 36 "./classaref.h"

struct _NspTypeClassARef {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 43 "./classaref.h"
};

typedef struct _nsp_classaref nsp_classaref;
struct _nsp_classaref {
  int cla_color;
  int cla_thickness;
  NspMatrix* cla_val;
  NspBMatrix* cla_bval;
  NspList* cla_lval;
  int ref_count;
};

struct _NspClassARef {
  /*< private >*/
  NspObject father;
  NspTypeClassARef*type;
  /*< public >*/
  nsp_classaref *obj;
};

extern int nsp_type_classaref_id;
extern NspTypeClassARef *nsp_type_classaref;

/* type instances for object */

NspTypeClassARef *new_type_classaref(type_mode mode);

/* instance for NspClassARef */

NspClassARef *new_classaref();

/*
 * Object methods redefined for classaref 
 */


#define NULLCLASSAREF (NspClassARef*) 0

extern NspClassARef *nsp_classaref_create(char *name,int cla_color,int cla_thickness,NspMatrix* cla_val,NspBMatrix* cla_bval,NspList* cla_lval,NspTypeBase *type);
extern NspClassARef *nsp_classaref_create_default(char *name);

/* from NspClassARefObj.c */

extern NspClassARef *nsp_classaref_copy(NspClassARef *H);
extern void nsp_classaref_destroy(NspClassARef *H);
extern int nsp_classaref_info(NspClassARef *H, int indent,const char *name, int rec_level);
extern int nsp_classaref_print(NspClassARef *H, int indent,const char *name, int rec_level);
extern int nsp_classaref_latex(NspClassARef *H, int indent,const char *name, int rec_level);
extern NspClassARef *nsp_classaref_object (NspObject *O);
extern int IsClassARefObj (Stack stack, int i);
extern int IsClassARef(NspObject *O);
extern NspClassARef *GetClassARefCopy (Stack stack, int i);
extern NspClassARef *GetClassARef (Stack stack, int i);
extern int nsp_classaref_create_partial(NspClassARef *H);
extern void nsp_classaref_destroy_partial(NspClassARef *H);
extern NspClassARef * nsp_classaref_copy_partial(NspClassARef *H,NspClassARef *self);
extern NspClassARef * nsp_classaref_full_copy_partial(NspClassARef *H,NspClassARef *self);
extern NspClassARef * nsp_classaref_full_copy(NspClassARef *self);
extern int nsp_classaref_check_values(NspClassARef *H);
extern int int_classaref_create(Stack stack, int rhs, int opt, int lhs);
extern NspClassARef *nsp_classaref_xdr_load_partial(XDR *xdrs, NspClassARef *M);
extern int nsp_classaref_xdr_save(XDR  *xdrs, NspClassARef *M);

#endif /* NSP_INC_NspClassARef */ 

#ifdef NspClassARef_Private 
static int init_classaref(NspClassARef *o,NspTypeClassARef *type);
static int nsp_classaref_size(NspClassARef *Mat, int flag);
static char *nsp_classaref_type_as_string(void);
static char *nsp_classaref_type_short_string(NspObject *v);
static int nsp_classaref_eq(NspClassARef *A, NspObject *B);
static int nsp_classaref_neq(NspClassARef *A, NspObject *B);
static NspClassARef *nsp_classaref_xdr_load(XDR *xdrs);
static AttrTab classaref_attrs[];
static NspMethods *classaref_get_methods(void);
/* static int int_classaref_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspClassARef *nsp_classaref_create_void(char *name,NspTypeBase *type);
#endif /* NspClassARef_Private */

