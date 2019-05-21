/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGIBaseInfo
#define NSP_INC_NspGIBaseInfo

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

/* NspGIBaseInfo */

#include <nsp/object.h>

/*
 * NspGIBaseInfo inherits from Object
 */

typedef struct _NspGIBaseInfo NspGIBaseInfo ;
typedef struct _NspTypeGIBaseInfo NspTypeGIBaseInfo ;

struct _NspTypeGIBaseInfo {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_gibaseinfo nsp_gibaseinfo;
struct _nsp_gibaseinfo {
  void* info;
  int ref_count;
};

struct _NspGIBaseInfo {
  /*< private >*/
  NspObject father;
  NspTypeGIBaseInfo*type;
  /*< public >*/
  nsp_gibaseinfo *obj;
};

extern int nsp_type_gibaseinfo_id;
extern NspTypeGIBaseInfo *nsp_type_gibaseinfo;

/* type instances for object */

NspTypeGIBaseInfo *new_type_gibaseinfo(type_mode mode);

/* instance for NspGIBaseInfo */

NspGIBaseInfo *new_gibaseinfo();

/*
 * Object methods redefined for gibaseinfo 
 */


#define NULLGIBASEINFO (NspGIBaseInfo*) 0

extern NspGIBaseInfo *nsp_gibaseinfo_create(const char *name,void* info,NspTypeBase *type);
extern NspGIBaseInfo *nsp_gibaseinfo_create_default(const char *name);

/* from NspGIBaseInfoObj.c */

extern NspGIBaseInfo *nsp_gibaseinfo_copy(NspGIBaseInfo *H);
extern void nsp_gibaseinfo_destroy(NspGIBaseInfo *H);
extern int nsp_gibaseinfo_info(NspGIBaseInfo *H, int indent,const char *name, int rec_level);
extern int nsp_gibaseinfo_print(NspGIBaseInfo *H, int indent,const char *name, int rec_level);
extern int nsp_gibaseinfo_latex(NspGIBaseInfo *H, int indent,const char *name, int rec_level);
extern NspGIBaseInfo *nsp_gibaseinfo_object (NspObject *O);
extern int IsGIBaseInfoObj (Stack stack, int i);
extern int IsGIBaseInfo(NspObject *O);
extern NspGIBaseInfo *GetGIBaseInfoCopy (Stack stack, int i);
extern NspGIBaseInfo *GetGIBaseInfo (Stack stack, int i);
extern int nsp_gibaseinfo_create_partial(NspGIBaseInfo *H);
extern void nsp_gibaseinfo_destroy_partial(NspGIBaseInfo *H);
extern NspGIBaseInfo * nsp_gibaseinfo_copy_partial(NspGIBaseInfo *H,NspGIBaseInfo *self);
extern NspGIBaseInfo * nsp_gibaseinfo_full_copy_partial(NspGIBaseInfo *H,NspGIBaseInfo *self);
extern NspGIBaseInfo * nsp_gibaseinfo_full_copy(NspGIBaseInfo *self);
extern int nsp_gibaseinfo_check_values(NspGIBaseInfo *H);
extern int int_gibaseinfo_create(Stack stack, int rhs, int opt, int lhs);
extern NspGIBaseInfo *nsp_gibaseinfo_xdr_load_partial(XDR *xdrs, NspGIBaseInfo *M);
extern int nsp_gibaseinfo_xdr_save(XDR  *xdrs, NspGIBaseInfo *M);

#endif /* NSP_INC_NspGIBaseInfo */ 

#ifdef NspGIBaseInfo_Private 
static int init_gibaseinfo(NspGIBaseInfo *o,NspTypeGIBaseInfo *type);
static int nsp_gibaseinfo_size(NspGIBaseInfo *Mat, int flag);
static char *nsp_gibaseinfo_type_as_string(void);
static char *nsp_gibaseinfo_type_short_string(NspObject *v);
static int nsp_gibaseinfo_eq(NspGIBaseInfo *A, NspObject *B);
static int nsp_gibaseinfo_neq(NspGIBaseInfo *A, NspObject *B);
static NspGIBaseInfo *nsp_gibaseinfo_xdr_load(XDR *xdrs);
static AttrTab gibaseinfo_attrs[];
static NspMethods *gibaseinfo_get_methods(void);
/* static int int_gibaseinfo_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGIBaseInfo *nsp_gibaseinfo_create_void(const char *name,NspTypeBase *type);
#endif /* NspGIBaseInfo_Private */

