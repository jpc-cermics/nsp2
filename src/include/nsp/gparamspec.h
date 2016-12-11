/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGParamSpec
#define NSP_INC_NspGParamSpec

/*
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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

#line 14 "codegen/gparamspec.override"

/* start: on windows GetGParamSpec exists  in wingdi.h ! */
#include <nsp/objects.h>
#ifdef GetGParamSpec
#undef GetGParamSpec
#endif

#line 33 "./gparamspec.h"
/* NspGParamSpec */

#include <nsp/object.h>

/*
 * NspGParamSpec inherits from Object
 */

typedef struct _NspGParamSpec NspGParamSpec ;
typedef struct _NspTypeGParamSpec NspTypeGParamSpec ;

struct _NspTypeGParamSpec {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_gparamspec nsp_gparamspec;
struct _nsp_gparamspec {
  GParamSpec* value;
  int ref_count;
};

struct _NspGParamSpec {
  /*< private >*/
  NspObject father;
  NspTypeGParamSpec*type;
  /*< public >*/
  nsp_gparamspec *obj;
};

extern int nsp_type_gparamspec_id;
extern NspTypeGParamSpec *nsp_type_gparamspec;

/* type instances for object */

NspTypeGParamSpec *new_type_gparamspec(type_mode mode);

/* instance for NspGParamSpec */

NspGParamSpec *new_gparamspec();

/*
 * Object methods redefined for gparamspec 
 */


#define NULLGPARAMSPEC (NspGParamSpec*) 0

extern NspGParamSpec *nsp_gparamspec_create(const char *name,GParamSpec* value,NspTypeBase *type);
extern NspGParamSpec *nsp_gparamspec_create_default(const char *name);

/* from NspGParamSpecObj.c */

extern NspGParamSpec *nsp_gparamspec_copy(NspGParamSpec *H);
extern void nsp_gparamspec_destroy(NspGParamSpec *H);
extern int nsp_gparamspec_info(NspGParamSpec *H, int indent,const char *name, int rec_level);
extern int nsp_gparamspec_print(NspGParamSpec *H, int indent,const char *name, int rec_level);
extern int nsp_gparamspec_latex(NspGParamSpec *H, int indent,const char *name, int rec_level);
extern NspGParamSpec *nsp_gparamspec_object (NspObject *O);
extern int IsGParamSpecObj (Stack stack, int i);
extern int IsGParamSpec(NspObject *O);
extern NspGParamSpec *GetGParamSpecCopy (Stack stack, int i);
extern NspGParamSpec *GetGParamSpec (Stack stack, int i);
extern int nsp_gparamspec_create_partial(NspGParamSpec *H);
extern void nsp_gparamspec_destroy_partial(NspGParamSpec *H);
extern NspGParamSpec * nsp_gparamspec_copy_partial(NspGParamSpec *H,NspGParamSpec *self);
extern NspGParamSpec * nsp_gparamspec_full_copy_partial(NspGParamSpec *H,NspGParamSpec *self);
extern NspGParamSpec * nsp_gparamspec_full_copy(NspGParamSpec *self);
extern int nsp_gparamspec_check_values(NspGParamSpec *H);
extern int int_gparamspec_create(Stack stack, int rhs, int opt, int lhs);
extern NspGParamSpec *nsp_gparamspec_xdr_load_partial(XDR *xdrs, NspGParamSpec *M);
extern int nsp_gparamspec_xdr_save(XDR  *xdrs, NspGParamSpec *M);

#line 23 "codegen/gparamspec.override"

/* public: on windows GetGParamSpec exists  in wingdi.h ! */
#include <nsp/objects.h>
#ifdef GetGParamSpec
#undef GetGParamSpec
#endif

#line 116 "./gparamspec.h"
#endif /* NSP_INC_NspGParamSpec */ 

#ifdef NspGParamSpec_Private 
static int init_gparamspec(NspGParamSpec *o,NspTypeGParamSpec *type);
static int nsp_gparamspec_size(NspGParamSpec *Mat, int flag);
static char *nsp_gparamspec_type_as_string(void);
static char *nsp_gparamspec_type_short_string(NspObject *v);
static int nsp_gparamspec_eq(NspGParamSpec *A, NspObject *B);
static int nsp_gparamspec_neq(NspGParamSpec *A, NspObject *B);
static NspGParamSpec *nsp_gparamspec_xdr_load(XDR *xdrs);
static AttrTab gparamspec_attrs[];
static NspMethods *gparamspec_get_methods(void);
/* static int int_gparamspec_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGParamSpec *nsp_gparamspec_create_void(const char *name,NspTypeBase *type);
#line 32 "codegen/gparamspec.override"

/* private: */
static int nsp_GParamSpec_full_copy(NspGParamSpec *H,GParamSpec *v,NspGParamSpec *self);
static int nsp_print_GParamSpec(int indent,GParamSpec *v,NspGParamSpec *M);
static int nsp_destroy_GParamSpec(GParamSpec *v,NspGParamSpec *H);
static int nsp_check_GParamSpec(GParamSpec *v,NspGParamSpec *H);

#line 139 "./gparamspec.h"
#endif /* NspGParamSpec_Private */

