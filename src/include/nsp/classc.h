/* -*- Mode: C -*- */
#ifndef NSP_INC_NspClassC
#define NSP_INC_NspClassC

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

#line 7 "codegen/classc.override"

/* */
typedef struct _ClassC ClassC;

struct _ClassC {
  int *s;
};

#line 34 "./classc.h"
/* NspClassC */

#include <nsp/object.h>

/*
 * NspClassC inherits from Object
 */

typedef struct _NspClassC NspClassC ;
typedef struct _NspTypeClassC NspTypeClassC ;

struct _NspTypeClassC {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_classc nsp_classc;
struct _nsp_classc {
  ClassC* value;
  int ref_count;
};

struct _NspClassC {
  /*< private >*/
  NspObject father;
  NspTypeClassC*type;
  /*< public >*/
  nsp_classc *obj;
};

extern int nsp_type_classc_id;
extern NspTypeClassC *nsp_type_classc;

/* type instances for object */

NspTypeClassC *new_type_classc(type_mode mode);

/* instance for NspClassC */

NspClassC *new_classc();

/*
 * Object methods redefined for classc 
 */


#define NULLCLASSC (NspClassC*) 0

extern NspClassC *nsp_classc_create(const char *name,ClassC* value,NspTypeBase *type);
extern NspClassC *nsp_classc_create_default(const char *name);

/* from NspClassCObj.c */

extern NspClassC *nsp_classc_copy(NspClassC *H);
extern void nsp_classc_destroy(NspClassC *H);
extern int nsp_classc_info(NspClassC *H, int indent,const char *name, int rec_level);
extern int nsp_classc_print(NspClassC *H, int indent,const char *name, int rec_level);
extern int nsp_classc_latex(NspClassC *H, int indent,const char *name, int rec_level);
extern NspClassC *nsp_classc_object (NspObject *O);
extern int IsClassCObj (Stack stack, int i);
extern int IsClassC(NspObject *O);
extern NspClassC *GetClassCCopy (Stack stack, int i);
extern NspClassC *GetClassC (Stack stack, int i);
extern int nsp_classc_create_partial(NspClassC *H);
extern void nsp_classc_destroy_partial(NspClassC *H);
extern NspClassC * nsp_classc_copy_partial(NspClassC *H,NspClassC *self);
extern NspClassC * nsp_classc_full_copy_partial(NspClassC *H,NspClassC *self);
extern NspClassC * nsp_classc_full_copy(NspClassC *self);
extern int nsp_classc_check_values(NspClassC *H);
extern int int_classc_create(Stack stack, int rhs, int opt, int lhs);
extern NspClassC *nsp_classc_xdr_load_partial(XDR *xdrs, NspClassC *M);
extern int nsp_classc_xdr_save(XDR  *xdrs, NspClassC *M);

#line 26 "codegen/classc.override"

ClassC *nsp_copy_ClassC(ClassC *gv);

#line 113 "./classc.h"
#endif /* NSP_INC_NspClassC */ 

#ifdef NspClassC_Private 
static int init_classc(NspClassC *o,NspTypeClassC *type);
static int nsp_classc_size(NspClassC *Mat, int flag);
static char *nsp_classc_type_as_string(void);
static char *nsp_classc_type_short_string(NspObject *v);
static int nsp_classc_eq(NspClassC *A, NspObject *B);
static int nsp_classc_neq(NspClassC *A, NspObject *B);
static NspClassC *nsp_classc_xdr_load(XDR *xdrs);
static AttrTab classc_attrs[];
static NspMethods *classc_get_methods(void);
/* static int int_classc_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspClassC *nsp_classc_create_void(const char *name,NspTypeBase *type);
#line 17 "codegen/classc.override"

static int nsp_destroy_ClassC(ClassC *v,NspClassC *H);
static int nsp_print_ClassC(int indent,ClassC *v,NspClassC *M);
static int nsp_check_ClassC(ClassC *v,NspClassC *H);
static int nsp_ClassC_full_copy(NspClassC *H,ClassC *v,NspClassC *self);
static int nsp_save_ClassC(XDR *xdrs,ClassC *value,NspClassC *self);
static int nsp_load_ClassC(XDR *xdrs,ClassC *value,NspClassC *self);
#line 136 "./classc.h"
#endif /* NspClassC_Private */

