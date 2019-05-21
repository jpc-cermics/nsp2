/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAstv
#define NSP_INC_NspAstv

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

#line 4 "codegen/astv.override"

#line 27 "./astv.h"
/* NspAstv */

#include <nsp/object.h>

/*
 * NspAstv inherits from Object
 */

typedef struct _NspAstv NspAstv ;
typedef struct _NspTypeAstv NspTypeAstv ;

struct _NspTypeAstv {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspAstv {
  /*< private >*/
  NspObject father;
  NspTypeAstv*type;
  /*< public >*/
    gboolean hv;
  int rows;
  int columns;
  NspObject* value;
  NspObject* ast_rows;
  NspObject* ast_columns;
  NspObject* ast_value;
  NspObject* args;
  char* stype;
  char* ssubtype;
};

extern int nsp_type_astv_id;
extern NspTypeAstv *nsp_type_astv;

/* type instances for object */

NspTypeAstv *new_type_astv(type_mode mode);

/* instance for NspAstv */

NspAstv *new_astv();

/*
 * Object methods redefined for astv 
 */


#define NULLASTV (NspAstv*) 0

extern NspAstv *nsp_astv_create(const char *name,gboolean hv,int rows,int columns,NspObject* value,NspObject* ast_rows,NspObject* ast_columns,NspObject* ast_value,NspObject* args,char* stype,char* ssubtype,NspTypeBase *type);
extern NspAstv *nsp_astv_create_default(const char *name);

/* from NspAstvObj.c */

extern NspAstv *nsp_astv_copy(NspAstv *H);
extern void nsp_astv_destroy(NspAstv *H);
extern int nsp_astv_info(NspAstv *H, int indent,const char *name, int rec_level);
extern int nsp_astv_print(NspAstv *H, int indent,const char *name, int rec_level);
extern int nsp_astv_latex(NspAstv *H, int indent,const char *name, int rec_level);
extern NspAstv *nsp_astv_object (NspObject *O);
extern int IsAstvObj (Stack stack, int i);
extern int IsAstv(NspObject *O);
extern NspAstv *GetAstvCopy (Stack stack, int i);
extern NspAstv *GetAstv (Stack stack, int i);
extern int nsp_astv_create_partial(NspAstv *H);
extern void nsp_astv_destroy_partial(NspAstv *H);
extern NspAstv * nsp_astv_copy_partial(NspAstv *H,NspAstv *self);
extern NspAstv * nsp_astv_full_copy_partial(NspAstv *H,NspAstv *self);
extern NspAstv * nsp_astv_full_copy(NspAstv *self);
extern int nsp_astv_check_values(NspAstv *H);
extern int int_astv_create(Stack stack, int rhs, int opt, int lhs);
extern NspAstv *nsp_astv_xdr_load_partial(XDR *xdrs, NspAstv *M);
extern int nsp_astv_xdr_save(XDR  *xdrs, NspAstv *M);

#line 7 "codegen/astv.override"

/* inserted at the end of public part of class include file */
extern NspAstv *nsp_astv(NspObject *Obj,int flag);

#line 110 "./astv.h"
#endif /* NSP_INC_NspAstv */ 

#ifdef NspAstv_Private 
static int init_astv(NspAstv *o,NspTypeAstv *type);
static int nsp_astv_size(NspAstv *Mat, int flag);
static char *nsp_astv_type_as_string(void);
static char *nsp_astv_type_short_string(NspObject *v);
static int nsp_astv_eq(NspAstv *A, NspObject *B);
static int nsp_astv_neq(NspAstv *A, NspObject *B);
static NspAstv *nsp_astv_xdr_load(XDR *xdrs);
static AttrTab astv_attrs[];
static NspMethods *astv_get_methods(void);
/* static int int_astv_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspAstv *nsp_astv_create_void(const char *name,NspTypeBase *type);
#line 13 "codegen/astv.override"

/* inserted in the private part of include file
 * of classa.h
 */

#line 131 "./astv.h"
#endif /* NspAstv_Private */

