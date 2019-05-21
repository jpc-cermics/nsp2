/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAgdisc
#define NSP_INC_NspAgdisc

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

/* NspAgdisc */

#include <nsp/object.h>

/*
 * NspAgdisc inherits from Object
 */

typedef struct _NspAgdisc NspAgdisc ;
typedef struct _NspTypeAgdisc NspTypeAgdisc ;

struct _NspTypeAgdisc {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_agdisc nsp_agdisc;
struct _nsp_agdisc {
  NspMatrix* Mcoord;
  int ref_count;
};

struct _NspAgdisc {
  /*< private >*/
  NspObject father;
  NspTypeAgdisc*type;
  /*< public >*/
  nsp_agdisc *obj;
};

extern int nsp_type_agdisc_id;
extern NspTypeAgdisc *nsp_type_agdisc;

/* type instances for object */

NspTypeAgdisc *new_type_agdisc(type_mode mode);

/* instance for NspAgdisc */

NspAgdisc *new_agdisc();

/*
 * Object methods redefined for agdisc 
 */


#define NULLAGDISC (NspAgdisc*) 0

extern NspAgdisc *nsp_agdisc_create(const char *name,NspMatrix* Mcoord,NspTypeBase *type);
extern NspAgdisc *nsp_agdisc_create_default(const char *name);

/* from NspAgdiscObj.c */

extern NspAgdisc *nsp_agdisc_copy(NspAgdisc *H);
extern void nsp_agdisc_destroy(NspAgdisc *H);
extern int nsp_agdisc_info(NspAgdisc *H, int indent,const char *name, int rec_level);
extern int nsp_agdisc_print(NspAgdisc *H, int indent,const char *name, int rec_level);
extern int nsp_agdisc_latex(NspAgdisc *H, int indent,const char *name, int rec_level);
extern NspAgdisc *nsp_agdisc_object (NspObject *O);
extern int IsAgdiscObj (Stack stack, int i);
extern int IsAgdisc(NspObject *O);
extern NspAgdisc *GetAgdiscCopy (Stack stack, int i);
extern NspAgdisc *GetAgdisc (Stack stack, int i);
extern int nsp_agdisc_create_partial(NspAgdisc *H);
extern void nsp_agdisc_destroy_partial(NspAgdisc *H);
extern NspAgdisc * nsp_agdisc_copy_partial(NspAgdisc *H,NspAgdisc *self);
extern NspAgdisc * nsp_agdisc_full_copy_partial(NspAgdisc *H,NspAgdisc *self);
extern NspAgdisc * nsp_agdisc_full_copy(NspAgdisc *self);
extern int nsp_agdisc_check_values(NspAgdisc *H);
extern int int_agdisc_create(Stack stack, int rhs, int opt, int lhs);
extern NspAgdisc *nsp_agdisc_xdr_load_partial(XDR *xdrs, NspAgdisc *M);
extern int nsp_agdisc_xdr_save(XDR  *xdrs, NspAgdisc *M);

#endif /* NSP_INC_NspAgdisc */ 

#ifdef NspAgdisc_Private 
static int init_agdisc(NspAgdisc *o,NspTypeAgdisc *type);
static int nsp_agdisc_size(NspAgdisc *Mat, int flag);
static char *nsp_agdisc_type_as_string(void);
static char *nsp_agdisc_type_short_string(NspObject *v);
static int nsp_agdisc_eq(NspAgdisc *A, NspObject *B);
static int nsp_agdisc_neq(NspAgdisc *A, NspObject *B);
static NspAgdisc *nsp_agdisc_xdr_load(XDR *xdrs);
static AttrTab agdisc_attrs[];
static NspMethods *agdisc_get_methods(void);
/* static int int_agdisc_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspAgdisc *nsp_agdisc_create_void(const char *name,NspTypeBase *type);
#endif /* NspAgdisc_Private */

