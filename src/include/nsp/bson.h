/* -*- Mode: C -*- */
#ifndef NSP_INC_NspBson
#define NSP_INC_NspBson

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

#line 4 "codegen/bson.override"
/* inserted at the start of include file of bson class */

#line 28 "./bson.h"
/* NspBson */

#include <nsp/object.h>

/*
 * NspBson inherits from Object
 */

typedef struct _NspBson NspBson ;
typedef struct _NspTypeBson NspTypeBson ;

struct _NspTypeBson {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_bson nsp_bson;
struct _nsp_bson {
  void* b;
  int ref_count;
};

struct _NspBson {
  /*< private >*/
  NspObject father;
  NspTypeBson*type;
  /*< public >*/
  nsp_bson *obj;
};

extern int nsp_type_bson_id;
extern NspTypeBson *nsp_type_bson;

/* type instances for object */

NspTypeBson *new_type_bson(type_mode mode);

/* instance for NspBson */

NspBson *new_bson();

/*
 * Object methods redefined for bson 
 */


#define NULLBSON (NspBson*) 0

extern NspBson *nsp_bson_create(const char *name,void* b,NspTypeBase *type);
extern NspBson *nsp_bson_create_default(const char *name);

/* from NspBsonObj.c */

extern NspBson *nsp_bson_copy(NspBson *H);
extern void nsp_bson_destroy(NspBson *H);
extern int nsp_bson_info(NspBson *H, int indent,const char *name, int rec_level);
extern int nsp_bson_print(NspBson *H, int indent,const char *name, int rec_level);
extern int nsp_bson_latex(NspBson *H, int indent,const char *name, int rec_level);
extern NspBson *nsp_bson_object (NspObject *O);
extern int IsBsonObj (Stack stack, int i);
extern int IsBson(NspObject *O);
extern NspBson *GetBsonCopy (Stack stack, int i);
extern NspBson *GetBson (Stack stack, int i);
extern int nsp_bson_create_partial(NspBson *H);
extern void nsp_bson_destroy_partial(NspBson *H);
extern NspBson * nsp_bson_copy_partial(NspBson *H,NspBson *self);
extern NspBson * nsp_bson_full_copy_partial(NspBson *H,NspBson *self);
extern NspBson * nsp_bson_full_copy(NspBson *self);
extern int nsp_bson_check_values(NspBson *H);
extern int int_bson_create(Stack stack, int rhs, int opt, int lhs);
extern NspBson *nsp_bson_xdr_load_partial(XDR *xdrs, NspBson *M);
extern int nsp_bson_xdr_save(XDR  *xdrs, NspBson *M);

#line 8 "codegen/bson.override"
/* inserted at the end of public part of class include file */
extern NspBson *nsp_bson_zz(NspObject *Obj,int flag);

#line 107 "./bson.h"
#endif /* NSP_INC_NspBson */ 

#ifdef NspBson_Private 
static int init_bson(NspBson *o,NspTypeBson *type);
static int nsp_bson_size(NspBson *Mat, int flag);
static char *nsp_bson_type_as_string(void);
static char *nsp_bson_type_short_string(NspObject *v);
static int nsp_bson_eq(NspBson *A, NspObject *B);
static int nsp_bson_neq(NspBson *A, NspObject *B);
static NspBson *nsp_bson_xdr_load(XDR *xdrs);
static AttrTab bson_attrs[];
static NspMethods *bson_get_methods(void);
/* static int int_bson_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspBson *nsp_bson_create_void(const char *name,NspTypeBase *type);
#line 13 "codegen/bson.override"
/* inserted in the private part of include file of bson class */
static NspObject *nsp_bson_loop(char *str, NspObject *O, NspObject *O1, int i, int *rep);
static int nsp_bson_is_true(void *Obj);
static int nsp_bson_insert(bson_t *b,const char *name, NspObject* Obj);
static void bson_show(void *self);
static NspHash *nsp_bson_to_hash(const char *name, bson_t *doc);
static bson_t *nsp_bson_create_from_hash(const char *name, NspHash *H);
static bson_t *nsp_bson_b_copy(const bson_t *b);

#define bson_uint8_t uint8_t 
#define bson_bool_t bool_t 
#define bson_uint32_t uint32_t

#line 136 "./bson.h"
#endif /* NspBson_Private */

