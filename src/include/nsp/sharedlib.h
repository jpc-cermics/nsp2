/* -*- Mode: C -*- */
#ifndef NSP_INC_NspSharedlib
#define NSP_INC_NspSharedlib

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

#line 4 "codegen/sharedlib.override"
/* inserted at the start of include file */

#line 28 "./sharedlib.h"
/* NspSharedlib */

#include <nsp/object.h>

/*
 * NspSharedlib inherits from Object
 */

typedef struct _NspSharedlib NspSharedlib ;
typedef struct _NspTypeSharedlib NspTypeSharedlib ;

#line 40 "./sharedlib.h"

struct _NspTypeSharedlib {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 47 "./sharedlib.h"
};

typedef struct _nsp_sharedlib nsp_sharedlib;
struct _nsp_sharedlib {
  void* shd;
  int id;
  char* path;
  int ref_count;
};

struct _NspSharedlib {
  /*< private >*/
  NspObject father;
  NspTypeSharedlib*type;
  /*< public >*/
  nsp_sharedlib *obj;
};

extern int nsp_type_sharedlib_id;
extern NspTypeSharedlib *nsp_type_sharedlib;

/* type instances for object */

NspTypeSharedlib *new_type_sharedlib(type_mode mode);

/* instance for NspSharedlib */

NspSharedlib *new_sharedlib();

/*
 * Object methods redefined for sharedlib 
 */


#define NULLSHAREDLIB (NspSharedlib*) 0

extern NspSharedlib *nsp_sharedlib_create(const char *name,void* shd,int id,char* path,NspTypeBase *type);
extern NspSharedlib *nsp_sharedlib_create_default(const char *name);

/* from NspSharedlibObj.c */

extern NspSharedlib *nsp_sharedlib_copy(NspSharedlib *H);
extern void nsp_sharedlib_destroy(NspSharedlib *H);
extern int nsp_sharedlib_info(NspSharedlib *H, int indent,const char *name, int rec_level);
extern int nsp_sharedlib_print(NspSharedlib *H, int indent,const char *name, int rec_level);
extern int nsp_sharedlib_latex(NspSharedlib *H, int indent,const char *name, int rec_level);
extern NspSharedlib *nsp_sharedlib_object (NspObject *O);
extern int IsSharedlibObj (Stack stack, int i);
extern int IsSharedlib(NspObject *O);
extern NspSharedlib *GetSharedlibCopy (Stack stack, int i);
extern NspSharedlib *GetSharedlib (Stack stack, int i);
extern int nsp_sharedlib_create_partial(NspSharedlib *H);
extern void nsp_sharedlib_destroy_partial(NspSharedlib *H);
extern NspSharedlib * nsp_sharedlib_copy_partial(NspSharedlib *H,NspSharedlib *self);
extern NspSharedlib * nsp_sharedlib_full_copy_partial(NspSharedlib *H,NspSharedlib *self);
extern NspSharedlib * nsp_sharedlib_full_copy(NspSharedlib *self);
extern int nsp_sharedlib_check_values(NspSharedlib *H);
extern int int_sharedlib_create(Stack stack, int rhs, int opt, int lhs);
extern NspSharedlib *nsp_sharedlib_xdr_load_partial(XDR *xdrs, NspSharedlib *M);
extern int nsp_sharedlib_xdr_save(XDR  *xdrs, NspSharedlib *M);

#line 8 "codegen/sharedlib.override"
/* inserted at the end of public part of include file */
extern int  nsp_insert_shared_library( void *shd,unsigned int id, const  char *path);
extern NspSharedlib * nsp_find_shared_library( int id);

#line 114 "./sharedlib.h"
#endif /* NSP_INC_NspSharedlib */ 

#ifdef NspSharedlib_Private 
static int init_sharedlib(NspSharedlib *o,NspTypeSharedlib *type);
static int nsp_sharedlib_size(NspSharedlib *Mat, int flag);
static char *nsp_sharedlib_type_as_string(void);
static char *nsp_sharedlib_type_short_string(NspObject *v);
static int nsp_sharedlib_eq(NspSharedlib *A, NspObject *B);
static int nsp_sharedlib_neq(NspSharedlib *A, NspObject *B);
static NspSharedlib *nsp_sharedlib_xdr_load(XDR *xdrs);
static AttrTab sharedlib_attrs[];
static NspMethods *sharedlib_get_methods(void);
/* static int int_sharedlib_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspSharedlib *nsp_sharedlib_create_void(const char *name,NspTypeBase *type);
#line 14 "codegen/sharedlib.override"
/* inserted in the private part of include file */

#line 132 "./sharedlib.h"
#endif /* NspSharedlib_Private */

