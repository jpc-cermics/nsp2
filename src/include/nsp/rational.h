/* -*- Mode: C -*- */
#ifndef NSP_INC_NspRational
#define NSP_INC_NspRational

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

#line 4 "codegen/rational.override"

#include <nsp/pmatrix.h>
#include <nsp/matint.h>

#line 30 "./rational.h"
/* NspRational */

#include <nsp/object.h>

/*
 * NspRational inherits from Object
 */

typedef struct _NspRational NspRational ;
typedef struct _NspTypeRational NspTypeRational ;

struct _NspTypeRational {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspRational {
  /*< private >*/
  NspObject father;
  NspTypeRational*type;
  /*< public >*/
    NspPMatrix* n;
  NspPMatrix* d;
  char* mode;
};

extern int nsp_type_rational_id;
extern NspTypeRational *nsp_type_rational;

/* type instances for object */

NspTypeRational *new_type_rational(type_mode mode);

/* instance for NspRational */

NspRational *new_rational();

/*
 * Object methods redefined for rational 
 */


#define NULLRATIONAL (NspRational*) 0

extern NspRational *nsp_rational_create(const char *name,NspPMatrix* n,NspPMatrix* d,char* mode,NspTypeBase *type);
extern NspRational *nsp_rational_create_default(const char *name);

/* from NspRationalObj.c */

extern NspRational *nsp_rational_copy(NspRational *H);
extern void nsp_rational_destroy(NspRational *H);
extern int nsp_rational_info(NspRational *H, int indent,const char *name, int rec_level);
extern int nsp_rational_print(NspRational *H, int indent,const char *name, int rec_level);
extern int nsp_rational_latex(NspRational *H, int indent,const char *name, int rec_level);
extern NspRational *nsp_rational_object (NspObject *O);
extern int IsRationalObj (Stack stack, int i);
extern int IsRational(NspObject *O);
extern NspRational *GetRationalCopy (Stack stack, int i);
extern NspRational *GetRational (Stack stack, int i);
extern int nsp_rational_create_partial(NspRational *H);
extern void nsp_rational_destroy_partial(NspRational *H);
extern NspRational * nsp_rational_copy_partial(NspRational *H,NspRational *self);
extern NspRational * nsp_rational_full_copy_partial(NspRational *H,NspRational *self);
extern NspRational * nsp_rational_full_copy(NspRational *self);
extern int nsp_rational_check_values(NspRational *H);
extern int int_rational_create(Stack stack, int rhs, int opt, int lhs);
extern NspRational *nsp_rational_xdr_load_partial(XDR *xdrs, NspRational *M);
extern int nsp_rational_xdr_save(XDR  *xdrs, NspRational *M);

#line 10 "codegen/rational.override"

/* inserted at the end of public part of include file
 * of classa.h
 */

#line 107 "./rational.h"
#endif /* NSP_INC_NspRational */ 

#ifdef NspRational_Private 
static int init_rational(NspRational *o,NspTypeRational *type);
static int nsp_rational_size(NspRational *Mat, int flag);
static char *nsp_rational_type_as_string(void);
static char *nsp_rational_type_short_string(NspObject *v);
static int nsp_rational_eq(NspRational *A, NspObject *B);
static int nsp_rational_neq(NspRational *A, NspObject *B);
static NspRational *nsp_rational_xdr_load(XDR *xdrs);
static AttrTab rational_attrs[];
static NspMethods *rational_get_methods(void);
/* static int int_rational_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspRational *nsp_rational_create_void(const char *name,NspTypeBase *type);
#line 17 "codegen/rational.override"

/* inserted in the private part of include file
 * of classa.h
 */

#line 128 "./rational.h"
#endif /* NspRational_Private */

