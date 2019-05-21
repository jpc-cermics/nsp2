/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAgedge
#define NSP_INC_NspAgedge

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

/* NspAgedge */

#include <nsp/object.h>

/*
 * NspAgedge inherits from Object
 */

typedef struct _NspAgedge NspAgedge ;
typedef struct _NspTypeAgedge NspTypeAgedge ;

struct _NspTypeAgedge {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_agedge nsp_agedge;
struct _nsp_agedge {
  void* edge;
  int ref_count;
};

struct _NspAgedge {
  /*< private >*/
  NspObject father;
  NspTypeAgedge*type;
  /*< public >*/
  nsp_agedge *obj;
};

extern int nsp_type_agedge_id;
extern NspTypeAgedge *nsp_type_agedge;

/* type instances for object */

NspTypeAgedge *new_type_agedge(type_mode mode);

/* instance for NspAgedge */

NspAgedge *new_agedge();

/*
 * Object methods redefined for agedge 
 */


#define NULLAGEDGE (NspAgedge*) 0

extern NspAgedge *nsp_agedge_create(const char *name,void* edge,NspTypeBase *type);
extern NspAgedge *nsp_agedge_create_default(const char *name);

/* from NspAgedgeObj.c */

extern NspAgedge *nsp_agedge_copy(NspAgedge *H);
extern void nsp_agedge_destroy(NspAgedge *H);
extern int nsp_agedge_info(NspAgedge *H, int indent,const char *name, int rec_level);
extern int nsp_agedge_print(NspAgedge *H, int indent,const char *name, int rec_level);
extern int nsp_agedge_latex(NspAgedge *H, int indent,const char *name, int rec_level);
extern NspAgedge *nsp_agedge_object (NspObject *O);
extern int IsAgedgeObj (Stack stack, int i);
extern int IsAgedge(NspObject *O);
extern NspAgedge *GetAgedgeCopy (Stack stack, int i);
extern NspAgedge *GetAgedge (Stack stack, int i);
extern int nsp_agedge_create_partial(NspAgedge *H);
extern void nsp_agedge_destroy_partial(NspAgedge *H);
extern NspAgedge * nsp_agedge_copy_partial(NspAgedge *H,NspAgedge *self);
extern NspAgedge * nsp_agedge_full_copy_partial(NspAgedge *H,NspAgedge *self);
extern NspAgedge * nsp_agedge_full_copy(NspAgedge *self);
extern int nsp_agedge_check_values(NspAgedge *H);
extern int int_agedge_create(Stack stack, int rhs, int opt, int lhs);
extern NspAgedge *nsp_agedge_xdr_load_partial(XDR *xdrs, NspAgedge *M);
extern int nsp_agedge_xdr_save(XDR  *xdrs, NspAgedge *M);

#endif /* NSP_INC_NspAgedge */ 

#ifdef NspAgedge_Private 
static int init_agedge(NspAgedge *o,NspTypeAgedge *type);
static int nsp_agedge_size(NspAgedge *Mat, int flag);
static char *nsp_agedge_type_as_string(void);
static char *nsp_agedge_type_short_string(NspObject *v);
static int nsp_agedge_eq(NspAgedge *A, NspObject *B);
static int nsp_agedge_neq(NspAgedge *A, NspObject *B);
static NspAgedge *nsp_agedge_xdr_load(XDR *xdrs);
static AttrTab agedge_attrs[];
static NspMethods *agedge_get_methods(void);
/* static int int_agedge_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspAgedge *nsp_agedge_create_void(const char *name,NspTypeBase *type);
#endif /* NspAgedge_Private */

