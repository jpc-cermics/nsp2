/* -*- Mode: C -*- */
#ifndef NSP_INC_NspArrows
#define NSP_INC_NspArrows

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

#line 4 "codegen/arrows.override"
/* inserted at the start of include file */
#include <nsp/figure.h>

#line 29 "./arrows.h"
/* NspArrows */

#include <nsp/graphic.h>

/*
 * NspArrows inherits from Graphic
 */

typedef struct _NspArrows NspArrows ;
typedef struct _NspTypeArrows NspTypeArrows ;

#line 41 "./arrows.h"

struct _NspTypeArrows {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 48 "./arrows.h"
};

typedef struct _nsp_arrows nsp_arrows;
struct _nsp_arrows {
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* color;
  double arsize;
  int ref_count;
};

struct _NspArrows {
  /*< private >*/
  NspGraphic father;
  NspTypeArrows*type;
  /*< public >*/
  nsp_arrows *obj;
};

extern int nsp_type_arrows_id;
extern NspTypeArrows *nsp_type_arrows;

/* type instances for graphic */

NspTypeArrows *new_type_arrows(type_mode mode);

/* instance for NspArrows */

NspArrows *new_arrows();

/*
 * Object methods redefined for arrows 
 */


#define NULLARROWS (NspArrows*) 0

extern NspArrows *nsp_arrows_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* color,double arsize,NspTypeBase *type);
extern NspArrows *nsp_arrows_create_default(char *name);

/* from NspArrowsObj.c */

extern NspArrows *nsp_arrows_copy(NspArrows *H);
extern void nsp_arrows_destroy(NspArrows *H);
extern int nsp_arrows_info(NspArrows *H, int indent,const char *name, int rec_level);
extern int nsp_arrows_print(NspArrows *H, int indent,const char *name, int rec_level);
extern int nsp_arrows_latex(NspArrows *H, int indent,const char *name, int rec_level);
extern NspArrows *nsp_arrows_object (NspObject *O);
extern int IsArrowsObj (Stack stack, int i);
extern int IsArrows(NspObject *O);
extern NspArrows *GetArrowsCopy (Stack stack, int i);
extern NspArrows *GetArrows (Stack stack, int i);
extern int nsp_arrows_create_partial(NspArrows *H);
extern void nsp_arrows_destroy_partial(NspArrows *H);
extern NspArrows * nsp_arrows_copy_partial(NspArrows *H,NspArrows *self);
extern NspArrows * nsp_arrows_full_copy_partial(NspArrows *H,NspArrows *self);
extern NspArrows * nsp_arrows_full_copy(NspArrows *self);
extern int nsp_arrows_check_values(NspArrows *H);
extern int int_arrows_create(Stack stack, int rhs, int opt, int lhs);
extern NspArrows *nsp_arrows_xdr_load_partial(XDR *xdrs, NspArrows *M);
extern int nsp_arrows_xdr_save(XDR  *xdrs, NspArrows *M);

#line 9 "codegen/arrows.override"

/* inserted at the end of public part of include file */

#line 115 "./arrows.h"
#endif /* NSP_INC_NspArrows */ 

#ifdef NspArrows_Private 
static int init_arrows(NspArrows *o,NspTypeArrows *type);
static int nsp_arrows_size(NspArrows *Mat, int flag);
static char *nsp_arrows_type_as_string(void);
static char *nsp_arrows_type_short_string(NspObject *v);
static int nsp_arrows_eq(NspArrows *A, NspObject *B);
static int nsp_arrows_neq(NspArrows *A, NspObject *B);
static NspArrows *nsp_arrows_xdr_load(XDR *xdrs);
static AttrTab arrows_attrs[];
static NspMethods *arrows_get_methods(void);
/* static int int_arrows_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspArrows *nsp_arrows_create_void(char *name,NspTypeBase *type);
#line 14 "codegen/arrows.override"

/* inserted in the private part of include file */

static void nsp_draw_arrows(BCG *Xgc,NspGraphic *Obj,const GdkRectangle *rect, void *data);
static void nsp_translate_arrows(NspGraphic *o,const double *tr);
static void nsp_rotate_arrows(NspGraphic *o,double *R);
static void nsp_scale_arrows(NspGraphic *o,double *alpha);
static int nsp_getbounds_arrows(NspGraphic *o,double *bounds);

#line 140 "./arrows.h"
#endif /* NspArrows_Private */

