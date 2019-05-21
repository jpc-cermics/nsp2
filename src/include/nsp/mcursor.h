/* -*- Mode: C -*- */
#ifndef NSP_INC_NspMcursor
#define NSP_INC_NspMcursor

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

/* NspMcursor */

#include <nsp/object.h>

/*
 * NspMcursor inherits from Object
 */

typedef struct _NspMcursor NspMcursor ;
typedef struct _NspTypeMcursor NspTypeMcursor ;

struct _NspTypeMcursor {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_mcursor nsp_mcursor;
struct _nsp_mcursor {
  void* cu;
  void* doc;
  int ref_count;
};

struct _NspMcursor {
  /*< private >*/
  NspObject father;
  NspTypeMcursor*type;
  /*< public >*/
  nsp_mcursor *obj;
};

extern int nsp_type_mcursor_id;
extern NspTypeMcursor *nsp_type_mcursor;

/* type instances for object */

NspTypeMcursor *new_type_mcursor(type_mode mode);

/* instance for NspMcursor */

NspMcursor *new_mcursor();

/*
 * Object methods redefined for mcursor 
 */


#define NULLMCURSOR (NspMcursor*) 0

extern NspMcursor *nsp_mcursor_create(const char *name,void* cu,void* doc,NspTypeBase *type);
extern NspMcursor *nsp_mcursor_create_default(const char *name);

/* from NspMcursorObj.c */

extern NspMcursor *nsp_mcursor_copy(NspMcursor *H);
extern void nsp_mcursor_destroy(NspMcursor *H);
extern int nsp_mcursor_info(NspMcursor *H, int indent,const char *name, int rec_level);
extern int nsp_mcursor_print(NspMcursor *H, int indent,const char *name, int rec_level);
extern int nsp_mcursor_latex(NspMcursor *H, int indent,const char *name, int rec_level);
extern NspMcursor *nsp_mcursor_object (NspObject *O);
extern int IsMcursorObj (Stack stack, int i);
extern int IsMcursor(NspObject *O);
extern NspMcursor *GetMcursorCopy (Stack stack, int i);
extern NspMcursor *GetMcursor (Stack stack, int i);
extern int nsp_mcursor_create_partial(NspMcursor *H);
extern void nsp_mcursor_destroy_partial(NspMcursor *H);
extern NspMcursor * nsp_mcursor_copy_partial(NspMcursor *H,NspMcursor *self);
extern NspMcursor * nsp_mcursor_full_copy_partial(NspMcursor *H,NspMcursor *self);
extern NspMcursor * nsp_mcursor_full_copy(NspMcursor *self);
extern int nsp_mcursor_check_values(NspMcursor *H);
extern int int_mcursor_create(Stack stack, int rhs, int opt, int lhs);
extern NspMcursor *nsp_mcursor_xdr_load_partial(XDR *xdrs, NspMcursor *M);
extern int nsp_mcursor_xdr_save(XDR  *xdrs, NspMcursor *M);

#endif /* NSP_INC_NspMcursor */ 

#ifdef NspMcursor_Private 
static int init_mcursor(NspMcursor *o,NspTypeMcursor *type);
static int nsp_mcursor_size(NspMcursor *Mat, int flag);
static char *nsp_mcursor_type_as_string(void);
static char *nsp_mcursor_type_short_string(NspObject *v);
static int nsp_mcursor_eq(NspMcursor *A, NspObject *B);
static int nsp_mcursor_neq(NspMcursor *A, NspObject *B);
static NspMcursor *nsp_mcursor_xdr_load(XDR *xdrs);
static AttrTab mcursor_attrs[];
static NspMethods *mcursor_get_methods(void);
/* static int int_mcursor_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspMcursor *nsp_mcursor_create_void(const char *name,NspTypeBase *type);
#endif /* NspMcursor_Private */

