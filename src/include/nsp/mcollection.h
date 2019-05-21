/* -*- Mode: C -*- */
#ifndef NSP_INC_NspMcollection
#define NSP_INC_NspMcollection

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

/* NspMcollection */

#include <nsp/object.h>

/*
 * NspMcollection inherits from Object
 */

typedef struct _NspMcollection NspMcollection ;
typedef struct _NspTypeMcollection NspTypeMcollection ;

struct _NspTypeMcollection {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_mcollection nsp_mcollection;
struct _nsp_mcollection {
  void* co;
  int ref_count;
};

struct _NspMcollection {
  /*< private >*/
  NspObject father;
  NspTypeMcollection*type;
  /*< public >*/
  nsp_mcollection *obj;
};

extern int nsp_type_mcollection_id;
extern NspTypeMcollection *nsp_type_mcollection;

/* type instances for object */

NspTypeMcollection *new_type_mcollection(type_mode mode);

/* instance for NspMcollection */

NspMcollection *new_mcollection();

/*
 * Object methods redefined for mcollection 
 */


#define NULLMCOLLECTION (NspMcollection*) 0

extern NspMcollection *nsp_mcollection_create(const char *name,void* co,NspTypeBase *type);
extern NspMcollection *nsp_mcollection_create_default(const char *name);

/* from NspMcollectionObj.c */

extern NspMcollection *nsp_mcollection_copy(NspMcollection *H);
extern void nsp_mcollection_destroy(NspMcollection *H);
extern int nsp_mcollection_info(NspMcollection *H, int indent,const char *name, int rec_level);
extern int nsp_mcollection_print(NspMcollection *H, int indent,const char *name, int rec_level);
extern int nsp_mcollection_latex(NspMcollection *H, int indent,const char *name, int rec_level);
extern NspMcollection *nsp_mcollection_object (NspObject *O);
extern int IsMcollectionObj (Stack stack, int i);
extern int IsMcollection(NspObject *O);
extern NspMcollection *GetMcollectionCopy (Stack stack, int i);
extern NspMcollection *GetMcollection (Stack stack, int i);
extern int nsp_mcollection_create_partial(NspMcollection *H);
extern void nsp_mcollection_destroy_partial(NspMcollection *H);
extern NspMcollection * nsp_mcollection_copy_partial(NspMcollection *H,NspMcollection *self);
extern NspMcollection * nsp_mcollection_full_copy_partial(NspMcollection *H,NspMcollection *self);
extern NspMcollection * nsp_mcollection_full_copy(NspMcollection *self);
extern int nsp_mcollection_check_values(NspMcollection *H);
extern int int_mcollection_create(Stack stack, int rhs, int opt, int lhs);
extern NspMcollection *nsp_mcollection_xdr_load_partial(XDR *xdrs, NspMcollection *M);
extern int nsp_mcollection_xdr_save(XDR  *xdrs, NspMcollection *M);

#endif /* NSP_INC_NspMcollection */ 

#ifdef NspMcollection_Private 
static int init_mcollection(NspMcollection *o,NspTypeMcollection *type);
static int nsp_mcollection_size(NspMcollection *Mat, int flag);
static char *nsp_mcollection_type_as_string(void);
static char *nsp_mcollection_type_short_string(NspObject *v);
static int nsp_mcollection_eq(NspMcollection *A, NspObject *B);
static int nsp_mcollection_neq(NspMcollection *A, NspObject *B);
static NspMcollection *nsp_mcollection_xdr_load(XDR *xdrs);
static AttrTab mcollection_attrs[];
static NspMethods *mcollection_get_methods(void);
/* static int int_mcollection_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspMcollection *nsp_mcollection_create_void(const char *name,NspTypeBase *type);
#endif /* NspMcollection_Private */

