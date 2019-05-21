/* -*- Mode: C -*- */
#ifndef NSP_INC_NspLinearSys
#define NSP_INC_NspLinearSys

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

#line 4 "codegen/linearsys.override"

#line 27 "./linearsys.h"
/* NspLinearSys */

#include <nsp/object.h>

/*
 * NspLinearSys inherits from Object
 */

typedef struct _NspLinearSys NspLinearSys ;
typedef struct _NspTypeLinearSys NspTypeLinearSys ;

struct _NspTypeLinearSys {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspLinearSys {
  /*< private >*/
  NspObject father;
  NspTypeLinearSys*type;
  /*< public >*/
    NspMatrix* A;
  NspMatrix* B;
  NspMatrix* C;
  NspPMatrix* D;
  NspMatrix* X0;
  char* dom;
  double dt;
};

extern int nsp_type_linearsys_id;
extern NspTypeLinearSys *nsp_type_linearsys;

/* type instances for object */

NspTypeLinearSys *new_type_linearsys(type_mode mode);

/* instance for NspLinearSys */

NspLinearSys *new_linearsys();

/*
 * Object methods redefined for linearsys 
 */


#define NULLLINEARSYS (NspLinearSys*) 0

extern NspLinearSys *nsp_linearsys_create(const char *name,NspMatrix* A,NspMatrix* B,NspMatrix* C,NspPMatrix* D,NspMatrix* X0,char* dom,double dt,NspTypeBase *type);
extern NspLinearSys *nsp_linearsys_create_default(const char *name);

/* from NspLinearSysObj.c */

extern NspLinearSys *nsp_linearsys_copy(NspLinearSys *H);
extern void nsp_linearsys_destroy(NspLinearSys *H);
extern int nsp_linearsys_info(NspLinearSys *H, int indent,const char *name, int rec_level);
extern int nsp_linearsys_print(NspLinearSys *H, int indent,const char *name, int rec_level);
extern int nsp_linearsys_latex(NspLinearSys *H, int indent,const char *name, int rec_level);
extern NspLinearSys *nsp_linearsys_object (NspObject *O);
extern int IsLinearSysObj (Stack stack, int i);
extern int IsLinearSys(NspObject *O);
extern NspLinearSys *GetLinearSysCopy (Stack stack, int i);
extern NspLinearSys *GetLinearSys (Stack stack, int i);
extern int nsp_linearsys_create_partial(NspLinearSys *H);
extern void nsp_linearsys_destroy_partial(NspLinearSys *H);
extern NspLinearSys * nsp_linearsys_copy_partial(NspLinearSys *H,NspLinearSys *self);
extern NspLinearSys * nsp_linearsys_full_copy_partial(NspLinearSys *H,NspLinearSys *self);
extern NspLinearSys * nsp_linearsys_full_copy(NspLinearSys *self);
extern int nsp_linearsys_check_values(NspLinearSys *H);
extern int int_linearsys_create(Stack stack, int rhs, int opt, int lhs);
extern NspLinearSys *nsp_linearsys_xdr_load_partial(XDR *xdrs, NspLinearSys *M);
extern int nsp_linearsys_xdr_save(XDR  *xdrs, NspLinearSys *M);

#line 7 "codegen/linearsys.override"

/* inserted at the end of public part of include file
 */
extern NspObject *nsp_linearsys_get_D(  NspLinearSys *sys);

#line 108 "./linearsys.h"
#endif /* NSP_INC_NspLinearSys */ 

#ifdef NspLinearSys_Private 
static int init_linearsys(NspLinearSys *o,NspTypeLinearSys *type);
static int nsp_linearsys_size(NspLinearSys *Mat, int flag);
static char *nsp_linearsys_type_as_string(void);
static char *nsp_linearsys_type_short_string(NspObject *v);
static int nsp_linearsys_eq(NspLinearSys *A, NspObject *B);
static int nsp_linearsys_neq(NspLinearSys *A, NspObject *B);
static NspLinearSys *nsp_linearsys_xdr_load(XDR *xdrs);
static AttrTab linearsys_attrs[];
static NspMethods *linearsys_get_methods(void);
/* static int int_linearsys_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspLinearSys *nsp_linearsys_create_void(const char *name,NspTypeBase *type);
#line 14 "codegen/linearsys.override"

/* inserted in the private part of include file
 */

#line 128 "./linearsys.h"
#endif /* NspLinearSys_Private */

