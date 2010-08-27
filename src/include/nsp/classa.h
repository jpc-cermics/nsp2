/* -*- Mode: C -*- */
#ifndef NSP_INC_NspClassA
#define NSP_INC_NspClassA

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

#line 4 "codegen/classa.override"

typedef struct _hfoo hfoo;
struct _hfoo {
  int a;
  double *val;
};

#line 33 "./classa.h"
/* NspClassA */

#include <nsp/object.h>

/*
 * NspClassA inherits from Object
 */

typedef struct _NspClassA NspClassA ;
typedef struct _NspTypeClassA NspTypeClassA ;

#line 45 "./classa.h"

struct _NspTypeClassA {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 52 "./classa.h"
};

struct _NspClassA {
  /*< private >*/
  NspObject father;
  NspTypeClassA*type;
  /*< public >*/
    int cla_color;
  int cla_thickness;
  NspMatrix* cla_val;
  NspBMatrix* cla_bval;
  NspList* cla_lval;
};

extern int nsp_type_classa_id;
extern NspTypeClassA *nsp_type_classa;

/* type instances for object */

NspTypeClassA *new_type_classa(type_mode mode);

/* instance for NspClassA */

NspClassA *new_classa();

/*
 * Object methods redefined for classa 
 */


#define NULLCLASSA (NspClassA*) 0

extern NspClassA *nsp_classa_create(char *name,int cla_color,int cla_thickness,NspMatrix* cla_val,NspBMatrix* cla_bval,NspList* cla_lval,NspTypeBase *type);
extern NspClassA *nsp_classa_create_default(char *name);

/* from NspClassAObj.c */

extern NspClassA *nsp_classa_copy(NspClassA *H);
extern void nsp_classa_destroy(NspClassA *H);
extern int nsp_classa_info(NspClassA *H, int indent,const char *name, int rec_level);
extern int nsp_classa_print(NspClassA *H, int indent,const char *name, int rec_level);
extern int nsp_classa_latex(NspClassA *H, int indent,const char *name, int rec_level);
extern NspClassA *nsp_classa_object (NspObject *O);
extern int IsClassAObj (Stack stack, int i);
extern int IsClassA(NspObject *O);
extern NspClassA *GetClassACopy (Stack stack, int i);
extern NspClassA *GetClassA (Stack stack, int i);
extern int nsp_classa_create_partial(NspClassA *H);
extern void nsp_classa_destroy_partial(NspClassA *H);
extern NspClassA * nsp_classa_copy_partial(NspClassA *H,NspClassA *self);
extern NspClassA * nsp_classa_full_copy_partial(NspClassA *H,NspClassA *self);
extern NspClassA * nsp_classa_full_copy(NspClassA *self);
extern int nsp_classa_check_values(NspClassA *H);
extern int int_classa_create(Stack stack, int rhs, int opt, int lhs);
extern NspClassA *nsp_classa_xdr_load_partial(XDR *xdrs, NspClassA *M);
extern int nsp_classa_xdr_save(XDR  *xdrs, NspClassA *M);

#line 13 "codegen/classa.override"

/* inserted at the end of public part of include file
 * of classa.h
 */

#line 116 "./classa.h"
#endif /* NSP_INC_NspClassA */ 

#ifdef NspClassA_Private 
static int init_classa(NspClassA *o,NspTypeClassA *type);
static int nsp_classa_size(NspClassA *Mat, int flag);
static char *nsp_classa_type_as_string(void);
static char *nsp_classa_type_short_string(NspObject *v);
static int nsp_classa_eq(NspClassA *A, NspObject *B);
static int nsp_classa_neq(NspClassA *A, NspObject *B);
static NspClassA *nsp_classa_xdr_load(XDR *xdrs);
static AttrTab classa_attrs[];
static NspMethods *classa_get_methods(void);
/* static int int_classa_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspClassA *nsp_classa_create_void(char *name,NspTypeBase *type);
#line 20 "codegen/classa.override"

/* inserted in the private part of include file
 * of classa.h
 */

#line 137 "./classa.h"
#endif /* NspClassA_Private */

