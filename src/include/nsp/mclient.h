/* -*- Mode: C -*- */
#ifndef NSP_INC_NspMclient
#define NSP_INC_NspMclient

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

/* NspMclient */

#include <nsp/object.h>

/*
 * NspMclient inherits from Object
 */

typedef struct _NspMclient NspMclient ;
typedef struct _NspTypeMclient NspTypeMclient ;

struct _NspTypeMclient {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_mclient nsp_mclient;
struct _nsp_mclient {
  void* cl;
  int ref_count;
};

struct _NspMclient {
  /*< private >*/
  NspObject father;
  NspTypeMclient*type;
  /*< public >*/
  nsp_mclient *obj;
};

extern int nsp_type_mclient_id;
extern NspTypeMclient *nsp_type_mclient;

/* type instances for object */

NspTypeMclient *new_type_mclient(type_mode mode);

/* instance for NspMclient */

NspMclient *new_mclient();

/*
 * Object methods redefined for mclient 
 */


#define NULLMCLIENT (NspMclient*) 0

extern NspMclient *nsp_mclient_create(const char *name,void* cl,NspTypeBase *type);
extern NspMclient *nsp_mclient_create_default(const char *name);

/* from NspMclientObj.c */

extern NspMclient *nsp_mclient_copy(NspMclient *H);
extern void nsp_mclient_destroy(NspMclient *H);
extern int nsp_mclient_info(NspMclient *H, int indent,const char *name, int rec_level);
extern int nsp_mclient_print(NspMclient *H, int indent,const char *name, int rec_level);
extern int nsp_mclient_latex(NspMclient *H, int indent,const char *name, int rec_level);
extern NspMclient *nsp_mclient_object (NspObject *O);
extern int IsMclientObj (Stack stack, int i);
extern int IsMclient(NspObject *O);
extern NspMclient *GetMclientCopy (Stack stack, int i);
extern NspMclient *GetMclient (Stack stack, int i);
extern int nsp_mclient_create_partial(NspMclient *H);
extern void nsp_mclient_destroy_partial(NspMclient *H);
extern NspMclient * nsp_mclient_copy_partial(NspMclient *H,NspMclient *self);
extern NspMclient * nsp_mclient_full_copy_partial(NspMclient *H,NspMclient *self);
extern NspMclient * nsp_mclient_full_copy(NspMclient *self);
extern int nsp_mclient_check_values(NspMclient *H);
extern int int_mclient_create(Stack stack, int rhs, int opt, int lhs);
extern NspMclient *nsp_mclient_xdr_load_partial(XDR *xdrs, NspMclient *M);
extern int nsp_mclient_xdr_save(XDR  *xdrs, NspMclient *M);

#endif /* NSP_INC_NspMclient */ 

#ifdef NspMclient_Private 
static int init_mclient(NspMclient *o,NspTypeMclient *type);
static int nsp_mclient_size(NspMclient *Mat, int flag);
static char *nsp_mclient_type_as_string(void);
static char *nsp_mclient_type_short_string(NspObject *v);
static int nsp_mclient_eq(NspMclient *A, NspObject *B);
static int nsp_mclient_neq(NspMclient *A, NspObject *B);
static NspMclient *nsp_mclient_xdr_load(XDR *xdrs);
static AttrTab mclient_attrs[];
static NspMethods *mclient_get_methods(void);
/* static int int_mclient_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspMclient *nsp_mclient_create_void(const char *name,NspTypeBase *type);
#endif /* NspMclient_Private */

