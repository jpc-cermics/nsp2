/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAgsym
#define NSP_INC_NspAgsym

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

/* NspAgsym */

#include <nsp/object.h>

/*
 * NspAgsym inherits from Object
 */

typedef struct _NspAgsym NspAgsym ;
typedef struct _NspTypeAgsym NspTypeAgsym ;

struct _NspTypeAgsym {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_agsym nsp_agsym;
struct _nsp_agsym {
  NspMatrix* Mcoord;
  int ref_count;
};

struct _NspAgsym {
  /*< private >*/
  NspObject father;
  NspTypeAgsym*type;
  /*< public >*/
  nsp_agsym *obj;
};

extern int nsp_type_agsym_id;
extern NspTypeAgsym *nsp_type_agsym;

/* type instances for object */

NspTypeAgsym *new_type_agsym(type_mode mode);

/* instance for NspAgsym */

NspAgsym *new_agsym();

/*
 * Object methods redefined for agsym 
 */


#define NULLAGSYM (NspAgsym*) 0

extern NspAgsym *nsp_agsym_create(const char *name,NspMatrix* Mcoord,NspTypeBase *type);
extern NspAgsym *nsp_agsym_create_default(const char *name);

/* from NspAgsymObj.c */

extern NspAgsym *nsp_agsym_copy(NspAgsym *H);
extern void nsp_agsym_destroy(NspAgsym *H);
extern int nsp_agsym_info(NspAgsym *H, int indent,const char *name, int rec_level);
extern int nsp_agsym_print(NspAgsym *H, int indent,const char *name, int rec_level);
extern int nsp_agsym_latex(NspAgsym *H, int indent,const char *name, int rec_level);
extern NspAgsym *nsp_agsym_object (NspObject *O);
extern int IsAgsymObj (Stack stack, int i);
extern int IsAgsym(NspObject *O);
extern NspAgsym *GetAgsymCopy (Stack stack, int i);
extern NspAgsym *GetAgsym (Stack stack, int i);
extern int nsp_agsym_create_partial(NspAgsym *H);
extern void nsp_agsym_destroy_partial(NspAgsym *H);
extern NspAgsym * nsp_agsym_copy_partial(NspAgsym *H,NspAgsym *self);
extern NspAgsym * nsp_agsym_full_copy_partial(NspAgsym *H,NspAgsym *self);
extern NspAgsym * nsp_agsym_full_copy(NspAgsym *self);
extern int nsp_agsym_check_values(NspAgsym *H);
extern int int_agsym_create(Stack stack, int rhs, int opt, int lhs);
extern NspAgsym *nsp_agsym_xdr_load_partial(XDR *xdrs, NspAgsym *M);
extern int nsp_agsym_xdr_save(XDR  *xdrs, NspAgsym *M);

#endif /* NSP_INC_NspAgsym */ 

#ifdef NspAgsym_Private 
static int init_agsym(NspAgsym *o,NspTypeAgsym *type);
static int nsp_agsym_size(NspAgsym *Mat, int flag);
static char *nsp_agsym_type_as_string(void);
static char *nsp_agsym_type_short_string(NspObject *v);
static int nsp_agsym_eq(NspAgsym *A, NspObject *B);
static int nsp_agsym_neq(NspAgsym *A, NspObject *B);
static NspAgsym *nsp_agsym_xdr_load(XDR *xdrs);
static AttrTab agsym_attrs[];
static NspMethods *agsym_get_methods(void);
/* static int int_agsym_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspAgsym *nsp_agsym_create_void(const char *name,NspTypeBase *type);
#endif /* NspAgsym_Private */

