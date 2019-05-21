/* -*- Mode: C -*- */
#ifndef NSP_INC_NspBvar
#define NSP_INC_NspBvar

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

#line 4 "codegen/bvar.override"
/* inserted at the start of include file of bvar class */

#line 28 "./bvar.h"
/* NspBvar */

#include <nsp/object.h>

/*
 * NspBvar inherits from Object
 */

typedef struct _NspBvar NspBvar ;
typedef struct _NspTypeBvar NspTypeBvar ;

struct _NspTypeBvar {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspBvar {
  /*< private >*/
  NspObject father;
  NspTypeBvar*type;
  /*< public >*/
    gboolean sym;
  NspObject* value;
  char* varname;
};

extern int nsp_type_bvar_id;
extern NspTypeBvar *nsp_type_bvar;

/* type instances for object */

NspTypeBvar *new_type_bvar(type_mode mode);

/* instance for NspBvar */

NspBvar *new_bvar();

/*
 * Object methods redefined for bvar 
 */


#define NULLBVAR (NspBvar*) 0

extern NspBvar *nsp_bvar_create(const char *name,gboolean sym,NspObject* value,char* varname,NspTypeBase *type);
extern NspBvar *nsp_bvar_create_default(const char *name);

/* from NspBvarObj.c */

extern NspBvar *nsp_bvar_copy(NspBvar *H);
extern void nsp_bvar_destroy(NspBvar *H);
extern int nsp_bvar_info(NspBvar *H, int indent,const char *name, int rec_level);
extern int nsp_bvar_print(NspBvar *H, int indent,const char *name, int rec_level);
extern int nsp_bvar_latex(NspBvar *H, int indent,const char *name, int rec_level);
extern NspBvar *nsp_bvar_object (NspObject *O);
extern int IsBvarObj (Stack stack, int i);
extern int IsBvar(NspObject *O);
extern NspBvar *GetBvarCopy (Stack stack, int i);
extern NspBvar *GetBvar (Stack stack, int i);
extern int nsp_bvar_create_partial(NspBvar *H);
extern void nsp_bvar_destroy_partial(NspBvar *H);
extern NspBvar * nsp_bvar_copy_partial(NspBvar *H,NspBvar *self);
extern NspBvar * nsp_bvar_full_copy_partial(NspBvar *H,NspBvar *self);
extern NspBvar * nsp_bvar_full_copy(NspBvar *self);
extern int nsp_bvar_check_values(NspBvar *H);
extern int int_bvar_create(Stack stack, int rhs, int opt, int lhs);
extern NspBvar *nsp_bvar_xdr_load_partial(XDR *xdrs, NspBvar *M);
extern int nsp_bvar_xdr_save(XDR  *xdrs, NspBvar *M);

#line 8 "codegen/bvar.override"
/* inserted at the end of public part of class include file */
extern NspBvar *nsp_bvar(NspObject *Obj,int flag);

#line 103 "./bvar.h"
#endif /* NSP_INC_NspBvar */ 

#ifdef NspBvar_Private 
static int init_bvar(NspBvar *o,NspTypeBvar *type);
static int nsp_bvar_size(NspBvar *Mat, int flag);
static char *nsp_bvar_type_as_string(void);
static char *nsp_bvar_type_short_string(NspObject *v);
static int nsp_bvar_eq(NspBvar *A, NspObject *B);
static int nsp_bvar_neq(NspBvar *A, NspObject *B);
static NspBvar *nsp_bvar_xdr_load(XDR *xdrs);
static AttrTab bvar_attrs[];
static NspMethods *bvar_get_methods(void);
/* static int int_bvar_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspBvar *nsp_bvar_create_void(const char *name,NspTypeBase *type);
#line 13 "codegen/bvar.override"
/* inserted in the private part of include file of bvar class */
static NspObject *nsp_bvar_loop(char *str, NspObject *O, NspObject *O1, int i, int *rep);
static int nsp_bvar_is_true(void *Obj);
static int bvar_code_getvarname(NspObject *Obj, NspHash *H);
static int bvar_code_countvarname(NspObject *Obj,const char *name, int *count);
static int bvar_code_isvarname(NspObject *Obj,const char *name,int *res);
static int bvar_code_vars(NspList *L, NspHash *H);
static int bvar_code_vars_used(NspList *L, NspHash *H);
static NspObject *bvar_code_replacevarname(NspObject *Obj,const char *name,NspObject *expr,int *changed);
static int bvar_code_replacevar(NspList *L,NspMatrix *Inds, const char *vname,NspObject *expr, int *callf) ;
static int bvar_code_varstatus(NspList *L,NspMatrix **Idx_used,NspMatrix **Idx_modified, const char *vname);

#line 131 "./bvar.h"
#endif /* NspBvar_Private */

