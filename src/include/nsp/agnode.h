/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAgnode
#define NSP_INC_NspAgnode

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

/* NspAgnode */

#include <nsp/object.h>

/*
 * NspAgnode inherits from Object
 */

typedef struct _NspAgnode NspAgnode ;
typedef struct _NspTypeAgnode NspTypeAgnode ;

struct _NspTypeAgnode {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_agnode nsp_agnode;
struct _nsp_agnode {
  void* node;
  int ref_count;
};

struct _NspAgnode {
  /*< private >*/
  NspObject father;
  NspTypeAgnode*type;
  /*< public >*/
  nsp_agnode *obj;
};

extern int nsp_type_agnode_id;
extern NspTypeAgnode *nsp_type_agnode;

/* type instances for object */

NspTypeAgnode *new_type_agnode(type_mode mode);

/* instance for NspAgnode */

NspAgnode *new_agnode();

/*
 * Object methods redefined for agnode 
 */


#define NULLAGNODE (NspAgnode*) 0

extern NspAgnode *nsp_agnode_create(const char *name,void* node,NspTypeBase *type);
extern NspAgnode *nsp_agnode_create_default(const char *name);

/* from NspAgnodeObj.c */

extern NspAgnode *nsp_agnode_copy(NspAgnode *H);
extern void nsp_agnode_destroy(NspAgnode *H);
extern int nsp_agnode_info(NspAgnode *H, int indent,const char *name, int rec_level);
extern int nsp_agnode_print(NspAgnode *H, int indent,const char *name, int rec_level);
extern int nsp_agnode_latex(NspAgnode *H, int indent,const char *name, int rec_level);
extern NspAgnode *nsp_agnode_object (NspObject *O);
extern int IsAgnodeObj (Stack stack, int i);
extern int IsAgnode(NspObject *O);
extern NspAgnode *GetAgnodeCopy (Stack stack, int i);
extern NspAgnode *GetAgnode (Stack stack, int i);
extern int nsp_agnode_create_partial(NspAgnode *H);
extern void nsp_agnode_destroy_partial(NspAgnode *H);
extern NspAgnode * nsp_agnode_copy_partial(NspAgnode *H,NspAgnode *self);
extern NspAgnode * nsp_agnode_full_copy_partial(NspAgnode *H,NspAgnode *self);
extern NspAgnode * nsp_agnode_full_copy(NspAgnode *self);
extern int nsp_agnode_check_values(NspAgnode *H);
extern int int_agnode_create(Stack stack, int rhs, int opt, int lhs);
extern NspAgnode *nsp_agnode_xdr_load_partial(XDR *xdrs, NspAgnode *M);
extern int nsp_agnode_xdr_save(XDR  *xdrs, NspAgnode *M);

#endif /* NSP_INC_NspAgnode */ 

#ifdef NspAgnode_Private 
static int init_agnode(NspAgnode *o,NspTypeAgnode *type);
static int nsp_agnode_size(NspAgnode *Mat, int flag);
static char *nsp_agnode_type_as_string(void);
static char *nsp_agnode_type_short_string(NspObject *v);
static int nsp_agnode_eq(NspAgnode *A, NspObject *B);
static int nsp_agnode_neq(NspAgnode *A, NspObject *B);
static NspAgnode *nsp_agnode_xdr_load(XDR *xdrs);
static AttrTab agnode_attrs[];
static NspMethods *agnode_get_methods(void);
/* static int int_agnode_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspAgnode *nsp_agnode_create_void(const char *name,NspTypeBase *type);
#endif /* NspAgnode_Private */

