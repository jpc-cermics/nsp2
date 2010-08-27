/* -*- Mode: C -*- */
#ifndef NSP_INC_NspFigure
#define NSP_INC_NspFigure

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

#line 4 "codegen/figure.override"
/* inserted at the start of include file */
#include <nsp/objects.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/figuredata.h>

#line 31 "./figure.h"
/* NspFigure */

#include <nsp/graphic.h>

/*
 * NspFigure inherits from Graphic
 */

typedef struct _NspFigure NspFigure ;
typedef struct _NspTypeFigure NspTypeFigure ;

#line 43 "./figure.h"

struct _NspTypeFigure {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 50 "./figure.h"
};

typedef struct _nsp_figure nsp_figure;
struct _nsp_figure {
  char* fname;
  char* driver;
  int id;
  NspMatrix* dims;
  NspMatrix* viewport_dims;
  gboolean wresize;
  NspMatrix* position;
  NspList* children;
  gboolean draw_now;
  NspFigureData* gc;
  void* Xgc;
  int ref_count;
};

struct _NspFigure {
  /*< private >*/
  NspGraphic father;
  NspTypeFigure*type;
  /*< public >*/
  nsp_figure *obj;
};

extern int nsp_type_figure_id;
extern NspTypeFigure *nsp_type_figure;

/* type instances for graphic */

NspTypeFigure *new_type_figure(type_mode mode);

/* instance for NspFigure */

NspFigure *new_figure();

/*
 * Object methods redefined for figure 
 */


#define NULLFIGURE (NspFigure*) 0

extern NspFigure *nsp_figure_create(char *name,char* fname,char* driver,int id,NspMatrix* dims,NspMatrix* viewport_dims,gboolean wresize,NspMatrix* position,NspList* children,gboolean draw_now,NspFigureData* gc,void* Xgc,NspTypeBase *type);
extern NspFigure *nsp_figure_create_default(char *name);

/* from NspFigureObj.c */

extern NspFigure *nsp_figure_copy(NspFigure *H);
extern void nsp_figure_destroy(NspFigure *H);
extern int nsp_figure_info(NspFigure *H, int indent,const char *name, int rec_level);
extern int nsp_figure_print(NspFigure *H, int indent,const char *name, int rec_level);
extern int nsp_figure_latex(NspFigure *H, int indent,const char *name, int rec_level);
extern NspFigure *nsp_figure_object (NspObject *O);
extern int IsFigureObj (Stack stack, int i);
extern int IsFigure(NspObject *O);
extern NspFigure *GetFigureCopy (Stack stack, int i);
extern NspFigure *GetFigure (Stack stack, int i);
extern int nsp_figure_create_partial(NspFigure *H);
extern void nsp_figure_destroy_partial(NspFigure *H);
extern NspFigure * nsp_figure_copy_partial(NspFigure *H,NspFigure *self);
extern NspFigure * nsp_figure_full_copy_partial(NspFigure *H,NspFigure *self);
extern NspFigure * nsp_figure_full_copy(NspFigure *self);
extern int nsp_figure_check_values(NspFigure *H);
extern int int_figure_create(Stack stack, int rhs, int opt, int lhs);
extern NspFigure *nsp_figure_xdr_load_partial(XDR *xdrs, NspFigure *M);
extern int nsp_figure_xdr_save(XDR  *xdrs, NspFigure *M);

#line 11 "codegen/figure.override"

/* inserted at the end of public part of include file
 * of figure.h
 */

extern BCG *nsp_check_graphic_context(void);
extern NspFigure *nsp_get_figure(BCG *Xgc);
extern NspFigure *nsp_check_for_figure(BCG *Xgc,int set_current);
extern NspObject * nsp_check_for_axes_or_objs3d(BCG *Xgc,const double *wrect);
extern NspObject * nsp_check_pt_axes_or_objs3d(BCG *Xgc,const int *pt);
extern void nsp_list_link_figure(NspList *L, nsp_figure  *F,void *A);
extern void nsp_list_unlink_figure(NspList *L, nsp_figure *F);
extern int nsp_list_check_figure(NspList *L, nsp_figure *F);
extern void nsp_graphic_link_figure(NspGraphic *G, void *F, void *A);
extern void nsp_graphic_unlink_figure(NspGraphic *G, void *F);
extern void nsp_figure_invalidate(NspGraphic *G);
extern NspObject *nsp_check_for_axes_or_objs3d_from_pointer(nsp_figure *F,void *obj);
extern NspGraphic *nsp_get_point_axes(BCG *Xgc,int px,int py,double *dp);
extern void nsp_figure_data_set_colormap(NspFigure *F,NspMatrix *Mc);
extern int nsp_set_current_figure(NspFigure *F);
extern NspFigure *nsp_get_current_figure(void);
extern void nsp_send_scale_2D_to_opengl(BCG *Xgc);
extern void nsp_send_scale_3D_to_opengl(BCG *Xgc);
extern int nsp_figure_remove_children(NspFigure *F);
extern NspObject *nsp_check_for_current_axes_or_objs3d(void);
extern NspFigure *nsp_check_for_current_figure(void);
extern void nsp_figure_data_reset(NspFigure *F);


#line 150 "./figure.h"
#endif /* NSP_INC_NspFigure */ 

#ifdef NspFigure_Private 
static int init_figure(NspFigure *o,NspTypeFigure *type);
static int nsp_figure_size(NspFigure *Mat, int flag);
static char *nsp_figure_type_as_string(void);
static char *nsp_figure_type_short_string(NspObject *v);
static int nsp_figure_eq(NspFigure *A, NspObject *B);
static int nsp_figure_neq(NspFigure *A, NspObject *B);
static NspFigure *nsp_figure_xdr_load(XDR *xdrs);
static AttrTab figure_attrs[];
static NspMethods *figure_get_methods(void);
/* static int int_figure_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspFigure *nsp_figure_create_void(char *name,NspTypeBase *type);
#line 42 "codegen/figure.override"

/* inserted in the private part of include file
 * of classa.h
 */
static void nsp_draw_figure(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static int nsp_figure_connect(NspFigure *);
static int nsp_figure_unconnect(NspFigure *);
static int nsp_figure_draw_latter(NspFigure *);
static int nsp_figure_draw_now(NspFigure *);
static void nsp_figure_children_unlink_figure(NspFigure *F);
static void nsp_figure_children_link_figure(NspFigure *F);
static int nsp_figure_check_children(NspFigure *F,NspList *L);
static NspList *nsp_figure_children(NspGraphic *Obj);
static NspAxes *nsp_get_current_axes(void);
static int nsp_figure_start_compound(NspFigure *F);
static NspCompound *nsp_figure_end_compound(char *name,NspFigure *F);
static int nsp_figure_remove_element(NspFigure *F,NspGraphic *Obj);
static void nsp_figure_set_gc_values(NspFigure *F);
static void nsp_figure_initialize_gc(NspFigure *F);
static void nsp_figure_process_updates(NspFigure *F);

#line 187 "./figure.h"
#endif /* NspFigure_Private */

