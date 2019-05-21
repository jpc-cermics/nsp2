/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGraphic
#define NSP_INC_NspGraphic

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

#line 4 "codegen/graphic.override"
/* inserted at the start of include file */
#include <gdk/gdk.h>

#line 29 "./graphic.h"
/* NspGraphic */

#include <nsp/object.h>

/*
 * NspGraphic inherits from Object
 */

typedef struct _NspGraphic NspGraphic ;
typedef struct _NspTypeGraphic NspTypeGraphic ;

#line 67 "codegen/graphic.override"

typedef void draw_func(BCG *Xgc,NspGraphic *Obj,const GdkRectangle *rect, void *data);
typedef void translate_func(NspGraphic *Obj,const double *tr);
typedef void rotate_func(NspGraphic *Obj,double *R);
typedef void scale_func(NspGraphic *Obj,double *alpha);
typedef int bounds_func(NspGraphic *Obj,double *bounds);
typedef void link_figure_func(NspGraphic *Obj,void *F, void *A);
typedef void unlink_figure_func(NspGraphic *Obj,void *F);
typedef NspList *children_func(NspGraphic *Obj);
typedef void zmean_func(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim);
typedef int n_faces_func(BCG *Xgc,NspGraphic *Obj);
typedef void invalidate_func(NspGraphic *G);

#line 55 "./graphic.h"

struct _NspTypeGraphic {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/

#line 52 "codegen/graphic.override"

  draw_func *draw;
  translate_func *translate;
  rotate_func *rotate;
  scale_func *scale;
  bounds_func *bounds;
  link_figure_func *link_figure;
  unlink_figure_func *unlink_figure;
  children_func *children;
  zmean_func *zmean;
  n_faces_func *n_faces;
  invalidate_func *invalidate;

#line 76 "./graphic.h"
};

typedef struct _nsp_graphic nsp_graphic;
struct _nsp_graphic {
  gboolean hilited;
  gboolean show;
  void* Fig;
  void* Axe;
  int ref_count;
};

struct _NspGraphic {
  /*< private >*/
  NspObject father;
  NspTypeGraphic*type;
  /*< public >*/
  nsp_graphic *obj;
};

extern int nsp_type_graphic_id;
extern NspTypeGraphic *nsp_type_graphic;

/* type instances for object */

NspTypeGraphic *new_type_graphic(type_mode mode);

/* instance for NspGraphic */

NspGraphic *new_graphic();

/*
 * Object methods redefined for graphic 
 */


#define NULLGRAPHIC (NspGraphic*) 0

extern NspGraphic *nsp_graphic_create(const char *name,gboolean hilited,gboolean show,void* Fig,void* Axe,NspTypeBase *type);
extern NspGraphic *nsp_graphic_create_default(const char *name);

/* from NspGraphicObj.c */

extern NspGraphic *nsp_graphic_copy(NspGraphic *H);
extern void nsp_graphic_destroy(NspGraphic *H);
extern int nsp_graphic_info(NspGraphic *H, int indent,const char *name, int rec_level);
extern int nsp_graphic_print(NspGraphic *H, int indent,const char *name, int rec_level);
extern int nsp_graphic_latex(NspGraphic *H, int indent,const char *name, int rec_level);
extern NspGraphic *nsp_graphic_object (NspObject *O);
extern int IsGraphicObj (Stack stack, int i);
extern int IsGraphic(NspObject *O);
extern NspGraphic *GetGraphicCopy (Stack stack, int i);
extern NspGraphic *GetGraphic (Stack stack, int i);
extern int nsp_graphic_create_partial(NspGraphic *H);
extern void nsp_graphic_destroy_partial(NspGraphic *H);
extern NspGraphic * nsp_graphic_copy_partial(NspGraphic *H,NspGraphic *self);
extern NspGraphic * nsp_graphic_full_copy_partial(NspGraphic *H,NspGraphic *self);
extern NspGraphic * nsp_graphic_full_copy(NspGraphic *self);
extern int nsp_graphic_check_values(NspGraphic *H);
extern int int_graphic_create(Stack stack, int rhs, int opt, int lhs);
extern NspGraphic *nsp_graphic_xdr_load_partial(XDR *xdrs, NspGraphic *M);
extern int nsp_graphic_xdr_save(XDR  *xdrs, NspGraphic *M);

#line 9 "codegen/graphic.override"

/* inserted at the end of public part of include file */
extern void nsp_graphic_invalidate(NspGraphic *G);
extern int nsp_graphic_intersect_rectangle(NspGraphic *G,const GdkRectangle *rect);

#line 145 "./graphic.h"
#endif /* NSP_INC_NspGraphic */ 

#ifdef NspGraphic_Private 
static int init_graphic(NspGraphic *o,NspTypeGraphic *type);
static int nsp_graphic_size(NspGraphic *Mat, int flag);
static char *nsp_graphic_type_as_string(void);
static char *nsp_graphic_type_short_string(NspObject *v);
static int nsp_graphic_eq(NspGraphic *A, NspObject *B);
static int nsp_graphic_neq(NspGraphic *A, NspObject *B);
static NspGraphic *nsp_graphic_xdr_load(XDR *xdrs);
static AttrTab graphic_attrs[];
static NspMethods *graphic_get_methods(void);
/* static int int_graphic_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGraphic *nsp_graphic_create_void(const char *name,NspTypeBase *type);
#line 16 "codegen/graphic.override"

/* inserted in the private part of include file */
static NspMatrix *graphic_get_bounds(NspGraphic *G);
static int nsp_graphic_bounds(NspGraphic *Obj,double *bounds);
static void nsp_graphic_draw(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_graphic_translate(NspGraphic *o,const double *tr);
static void nsp_graphic_rotate(NspGraphic *o,double *R);
static void nsp_graphic_scale(NspGraphic *o,double *alpha);

#line 170 "./graphic.h"
#endif /* NspGraphic_Private */

