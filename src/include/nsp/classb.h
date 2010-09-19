/* -*- Mode: C -*- */
#ifndef NSP_INC_NspClassB
#define NSP_INC_NspClassB

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

/* NspClassB */

#include <nsp/classa.h>

/*
 * NspClassB inherits from ClassA
 */

typedef struct _NspClassB NspClassB ;
typedef struct _NspTypeClassB NspTypeClassB ;

#line 36 "./classb.h"

struct _NspTypeClassB {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 43 "./classb.h"
};

struct _NspClassB {
  /*< private >*/
  NspClassA father;
  NspTypeClassB*type;
  /*< public >*/
    int clb_color;
  int clb_thickness;
  NspMatrix* clb_val;
};

extern int nsp_type_classb_id;
extern NspTypeClassB *nsp_type_classb;

/* type instances for classa */

NspTypeClassB *new_type_classb(type_mode mode);

/* instance for NspClassB */

NspClassB *new_classb();

/*
 * Object methods redefined for classb 
 */


#define NULLCLASSB (NspClassB*) 0

extern NspClassB *nsp_classb_create(const char *name,int clb_color,int clb_thickness,NspMatrix* clb_val,NspTypeBase *type);
extern NspClassB *nsp_classb_create_default(const char *name);

/* from NspClassBObj.c */

extern NspClassB *nsp_classb_copy(NspClassB *H);
extern void nsp_classb_destroy(NspClassB *H);
extern int nsp_classb_info(NspClassB *H, int indent,const char *name, int rec_level);
extern int nsp_classb_print(NspClassB *H, int indent,const char *name, int rec_level);
extern int nsp_classb_latex(NspClassB *H, int indent,const char *name, int rec_level);
extern NspClassB *nsp_classb_object (NspObject *O);
extern int IsClassBObj (Stack stack, int i);
extern int IsClassB(NspObject *O);
extern NspClassB *GetClassBCopy (Stack stack, int i);
extern NspClassB *GetClassB (Stack stack, int i);
extern int nsp_classb_create_partial(NspClassB *H);
extern void nsp_classb_destroy_partial(NspClassB *H);
extern NspClassB * nsp_classb_copy_partial(NspClassB *H,NspClassB *self);
extern NspClassB * nsp_classb_full_copy_partial(NspClassB *H,NspClassB *self);
extern NspClassB * nsp_classb_full_copy(NspClassB *self);
extern int nsp_classb_check_values(NspClassB *H);
extern int int_classb_create(Stack stack, int rhs, int opt, int lhs);
extern NspClassB *nsp_classb_xdr_load_partial(XDR *xdrs, NspClassB *M);
extern int nsp_classb_xdr_save(XDR  *xdrs, NspClassB *M);

#endif /* NSP_INC_NspClassB */ 

#ifdef NspClassB_Private 
static int init_classb(NspClassB *o,NspTypeClassB *type);
static int nsp_classb_size(NspClassB *Mat, int flag);
static char *nsp_classb_type_as_string(void);
static char *nsp_classb_type_short_string(NspObject *v);
static int nsp_classb_eq(NspClassB *A, NspObject *B);
static int nsp_classb_neq(NspClassB *A, NspObject *B);
static NspClassB *nsp_classb_xdr_load(XDR *xdrs);
static AttrTab classb_attrs[];
static NspMethods *classb_get_methods(void);
/* static int int_classb_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspClassB *nsp_classb_create_void(const char *name,NspTypeBase *type);
#endif /* NspClassB_Private */

