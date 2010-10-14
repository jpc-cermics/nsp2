/* -*- Mode: C -*- */
#ifndef NSP_INC_NspHm
#define NSP_INC_NspHm

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

#line 4 "codegen/hm.override"

#line 27 "./hm.h"
/* NspHm */

#include <nsp/object.h>

/*
 * NspHm inherits from Object
 */

typedef struct _NspHm NspHm ;
typedef struct _NspTypeHm NspTypeHm ;

#line 39 "./hm.h"

struct _NspTypeHm {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 46 "./hm.h"
};

struct _NspHm {
  /*< private >*/
  NspObject father;
  NspTypeHm*type;
  /*< public >*/
    void* htable;
  int hsize;
  int filled;
  int base;
  int keysize;
};

extern int nsp_type_hm_id;
extern NspTypeHm *nsp_type_hm;

/* type instances for object */

NspTypeHm *new_type_hm(type_mode mode);

/* instance for NspHm */

NspHm *new_hm();

/*
 * Object methods redefined for hm 
 */


#define NULLHM (NspHm*) 0

extern NspHm *nsp_hm_create(const char *name,void* htable,int hsize,int filled,int base,int keysize,NspTypeBase *type);
extern NspHm *nsp_hm_create_default(const char *name);

/* from NspHmObj.c */

extern NspHm *nsp_hm_copy(NspHm *H);
extern void nsp_hm_destroy(NspHm *H);
extern int nsp_hm_info(NspHm *H, int indent,const char *name, int rec_level);
extern int nsp_hm_print(NspHm *H, int indent,const char *name, int rec_level);
extern int nsp_hm_latex(NspHm *H, int indent,const char *name, int rec_level);
extern NspHm *nsp_hm_object (NspObject *O);
extern int IsHmObj (Stack stack, int i);
extern int IsHm(NspObject *O);
extern NspHm *GetHmCopy (Stack stack, int i);
extern NspHm *GetHm (Stack stack, int i);
extern int nsp_hm_create_partial(NspHm *H);
extern void nsp_hm_destroy_partial(NspHm *H);
extern NspHm * nsp_hm_copy_partial(NspHm *H,NspHm *self);
extern NspHm * nsp_hm_full_copy_partial(NspHm *H,NspHm *self);
extern NspHm * nsp_hm_full_copy(NspHm *self);
extern int nsp_hm_check_values(NspHm *H);
extern int int_hm_create(Stack stack, int rhs, int opt, int lhs);
extern NspHm *nsp_hm_xdr_load_partial(XDR *xdrs, NspHm *M);
extern int nsp_hm_xdr_save(XDR  *xdrs, NspHm *M);

#line 7 "codegen/hm.override"
/* inserted at the end of public part of include file
 * of classa.h
 */

#line 109 "./hm.h"
#endif /* NSP_INC_NspHm */ 

#ifdef NspHm_Private 
static int init_hm(NspHm *o,NspTypeHm *type);
static int nsp_hm_size(NspHm *Mat, int flag);
static char *nsp_hm_type_as_string(void);
static char *nsp_hm_type_short_string(NspObject *v);
static int nsp_hm_eq(NspHm *A, NspObject *B);
static int nsp_hm_neq(NspHm *A, NspObject *B);
static NspHm *nsp_hm_xdr_load(XDR *xdrs);
static AttrTab hm_attrs[];
static NspMethods *hm_get_methods(void);
/* static int int_hm_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspHm *nsp_hm_create_void(const char *name,NspTypeBase *type);
#line 13 "codegen/hm.override"
/* inserted in the private part of include file
 * of classa.h
 */

/* Element stored in hm */

typedef struct _HM_Entry  HM_Entry; 

struct _HM_Entry { 
  unsigned int used; /* used to detect if data is present */
  gint64 key;
  double val;
};

typedef enum {
  HM_FIND,
  HM_ENTER,
  HM_REMOVE 
} HMOperation;

static NspHm *nsp_bhcreate(const char *name, unsigned int nel);
static int nsp_bhsearch(NspHm *H, gint64 key,double *val, HMOperation action);
static int nsp_bhfind(NspHm *H, gint64 key,double *val);
static NspMatrix *nsp_hm_key2m(NspHm *Hm, gint64 key) ;
static int nsp_hm_compute_key(NspHm *Hm,NspMatrix *M, gint64 *key);
static void nsp_hm_remove(NspHm *H, gint64 key );
static int nsp_hm_enter(NspHm *H, gint64 key,double val);
static int nsp_hm_find(NspHm *H, gint64 key, double *val);
static int nsp_hm_check_slope(NspHm *H,NspMatrix *M);
static NspIMatrix *nsp_hm_get_keys(const char *name,NspHm *Hv);
const int no_key=-1;

#line 157 "./hm.h"
#endif /* NspHm_Private */

