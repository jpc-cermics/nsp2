/* -*- Mode: C -*- */
#ifndef NSP_INC_NspEpoints
#define NSP_INC_NspEpoints

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

#line 4 "codegen/epoints.override"
/* inserted at the start of include file */

#line 28 "./epoints.h"
/* NspEpoints */

#include <nsp/object.h>

/*
 * NspEpoints inherits from Object
 */

typedef struct _NspEpoints NspEpoints ;
typedef struct _NspTypeEpoints NspTypeEpoints ;

#line 40 "./epoints.h"

struct _NspTypeEpoints {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 47 "./epoints.h"
};

typedef struct _nsp_epoints nsp_epoints;
struct _nsp_epoints {
  void* func;
  int shid;
  int ref_count;
};

struct _NspEpoints {
  /*< private >*/
  NspObject father;
  NspTypeEpoints*type;
  /*< public >*/
  nsp_epoints *obj;
};

extern int nsp_type_epoints_id;
extern NspTypeEpoints *nsp_type_epoints;

/* type instances for object */

NspTypeEpoints *new_type_epoints(type_mode mode);

/* instance for NspEpoints */

NspEpoints *new_epoints();

/*
 * Object methods redefined for epoints 
 */


#define NULLEPOINTS (NspEpoints*) 0

extern NspEpoints *nsp_epoints_create(const char *name,void* func,int shid,NspTypeBase *type);
extern NspEpoints *nsp_epoints_create_default(const char *name);

/* from NspEpointsObj.c */

extern NspEpoints *nsp_epoints_copy(NspEpoints *H);
extern void nsp_epoints_destroy(NspEpoints *H);
extern int nsp_epoints_info(NspEpoints *H, int indent,const char *name, int rec_level);
extern int nsp_epoints_print(NspEpoints *H, int indent,const char *name, int rec_level);
extern int nsp_epoints_latex(NspEpoints *H, int indent,const char *name, int rec_level);
extern NspEpoints *nsp_epoints_object (NspObject *O);
extern int IsEpointsObj (Stack stack, int i);
extern int IsEpoints(NspObject *O);
extern NspEpoints *GetEpointsCopy (Stack stack, int i);
extern NspEpoints *GetEpoints (Stack stack, int i);
extern int nsp_epoints_create_partial(NspEpoints *H);
extern void nsp_epoints_destroy_partial(NspEpoints *H);
extern NspEpoints * nsp_epoints_copy_partial(NspEpoints *H,NspEpoints *self);
extern NspEpoints * nsp_epoints_full_copy_partial(NspEpoints *H,NspEpoints *self);
extern NspEpoints * nsp_epoints_full_copy(NspEpoints *self);
extern int nsp_epoints_check_values(NspEpoints *H);
extern int int_epoints_create(Stack stack, int rhs, int opt, int lhs);
extern NspEpoints *nsp_epoints_xdr_load_partial(XDR *xdrs, NspEpoints *M);
extern int nsp_epoints_xdr_save(XDR  *xdrs, NspEpoints *M);

#line 8 "codegen/epoints.override"
/* inserted at the end of public part of include file */
extern int  nsp_insert_epoint(const char *name, void *func, int sharedid);
extern NspEpoints *nsp_find_epoint(const char *name) ;
extern void nsp_show_epoints();
extern void nsp_remove_sharedlib_epoints(int shid);

#line 115 "./epoints.h"
#endif /* NSP_INC_NspEpoints */ 

#ifdef NspEpoints_Private 
static int init_epoints(NspEpoints *o,NspTypeEpoints *type);
static int nsp_epoints_size(NspEpoints *Mat, int flag);
static char *nsp_epoints_type_as_string(void);
static char *nsp_epoints_type_short_string(NspObject *v);
static int nsp_epoints_eq(NspEpoints *A, NspObject *B);
static int nsp_epoints_neq(NspEpoints *A, NspObject *B);
static NspEpoints *nsp_epoints_xdr_load(XDR *xdrs);
static AttrTab epoints_attrs[];
static NspMethods *epoints_get_methods(void);
/* static int int_epoints_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspEpoints *nsp_epoints_create_void(const char *name,NspTypeBase *type);
#line 16 "codegen/epoints.override"
/* inserted in the private part of include file */

#line 133 "./epoints.h"
#endif /* NspEpoints_Private */

