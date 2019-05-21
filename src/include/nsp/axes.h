/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAxes
#define NSP_INC_NspAxes

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

#line 4 "codegen/axes.override"
/* inserted at the start of include file */
#include <nsp/figure.h>

/**
 * NspAxes:
 * @obj: a #nsp_axes pointer
 *
 * inherits from #NspGraphics
 */

#line 36 "./axes.h"
/* NspAxes */

#include <nsp/graphic.h>

/*
 * NspAxes inherits from Graphic
 */

typedef struct _NspAxes NspAxes ;
typedef struct _NspTypeAxes NspTypeAxes ;

struct _NspTypeAxes {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_axes nsp_axes;
struct _nsp_axes {
  nsp_gcscale scale;
  NspMatrix* wrect;
  double rho;
  gboolean top;
  NspMatrix* bounds;
  NspMatrix* arect;
  NspMatrix* frect;
  char* title;
  char* x;
  char* y;
  NspList* children;
  gboolean fixed;
  gboolean iso;
  gboolean auto_axis;
  int grid;
  int axes;
  gboolean xlog;
  gboolean ylog;
  int lpos;
  NspMatrix* rect;
  gboolean zoom;
  NspMatrix* zrect;
  gboolean clip;
  int line_width;
  int font_size;
  int background;
  NspMatrix* nax;
  int ref_count;
};

struct _NspAxes {
  /*< private >*/
  NspGraphic father;
  NspTypeAxes*type;
  /*< public >*/
  nsp_axes *obj;
};

extern int nsp_type_axes_id;
extern NspTypeAxes *nsp_type_axes;

/* type instances for graphic */

NspTypeAxes *new_type_axes(type_mode mode);

/* instance for NspAxes */

NspAxes *new_axes();

/*
 * Object methods redefined for axes 
 */


#define NULLAXES (NspAxes*) 0

extern NspAxes *nsp_axes_create(const char *name,nsp_gcscale scale,NspMatrix* wrect,double rho,gboolean top,NspMatrix* bounds,NspMatrix* arect,NspMatrix* frect,char* title,char* x,char* y,NspList* children,gboolean fixed,gboolean iso,gboolean auto_axis,int grid,int axes,gboolean xlog,gboolean ylog,int lpos,NspMatrix* rect,gboolean zoom,NspMatrix* zrect,gboolean clip,int line_width,int font_size,int background,NspMatrix* nax,NspTypeBase *type);
extern NspAxes *nsp_axes_create_default(const char *name);

/* from NspAxesObj.c */

extern NspAxes *nsp_axes_copy(NspAxes *H);
extern void nsp_axes_destroy(NspAxes *H);
extern int nsp_axes_info(NspAxes *H, int indent,const char *name, int rec_level);
extern int nsp_axes_print(NspAxes *H, int indent,const char *name, int rec_level);
extern int nsp_axes_latex(NspAxes *H, int indent,const char *name, int rec_level);
extern NspAxes *nsp_axes_object (NspObject *O);
extern int IsAxesObj (Stack stack, int i);
extern int IsAxes(NspObject *O);
extern NspAxes *GetAxesCopy (Stack stack, int i);
extern NspAxes *GetAxes (Stack stack, int i);
extern int nsp_axes_create_partial(NspAxes *H);
extern void nsp_axes_destroy_partial(NspAxes *H);
extern NspAxes * nsp_axes_copy_partial(NspAxes *H,NspAxes *self);
extern NspAxes * nsp_axes_full_copy_partial(NspAxes *H,NspAxes *self);
extern NspAxes * nsp_axes_full_copy(NspAxes *self);
extern int nsp_axes_check_values(NspAxes *H);
extern int int_axes_create(Stack stack, int rhs, int opt, int lhs);
extern NspAxes *nsp_axes_xdr_load_partial(XDR *xdrs, NspAxes *M);
extern int nsp_axes_xdr_save(XDR  *xdrs, NspAxes *M);

#line 16 "codegen/axes.override"

/* inserted at the end of public part of include file */

extern NspAxes * nsp_check_for_axes(BCG *Xgc,const double *wrect) ;
extern void nsp_axes_i2f(nsp_axes *A,int x,int y,double pt[2]);
extern BCG *nsp_check_graphic_context(void);
extern void nsp_list_unlink_figure(NspList *L, nsp_figure *F);
extern int nsp_list_check_figure(NspList *L, nsp_figure *F);
extern void nsp_strf_axes(NspAxes *A,double *rect, char scale);
extern void nsp_strf_axes_new(NspAxes *A,double *rect, char scale,int auto_axis,int iso);
extern int nsp_axes_insert_child(NspAxes *A, NspGraphic *G, int invalidate);
extern void nsp_axes_invalidate(NspGraphic *G);
extern void nsp_figure_unzoom(NspGraphic *Obj);
extern void nsp_figure_zoom(BCG *Xgc,int *box);
extern void nsp_axes_update_frame_bounds(BCG *Xgc,double *wrect,double *frect,double *arect,
					 int *aaint,int isomode, int auto_axes, char *xf);
extern void nsp_draw_colorbar(BCG *Xgc,nsp_axes *P,double vmin , double vmax, int *colminmax);
/* XXX */
extern int gr_compute_ticks(double *xminv,double *xmaxv,double *grads, int *ngrads);
extern NspAxes *nsp_check_for_current_axes(int create);
extern NspAxes * nsp_check_for_axes_in_figure(NspFigure *F,const double *wrect, int create); 
extern int nsp_axes_remove_children(NspAxes *A);
  
#line 161 "./axes.h"
#endif /* NSP_INC_NspAxes */ 

#ifdef NspAxes_Private 
static int init_axes(NspAxes *o,NspTypeAxes *type);
static int nsp_axes_size(NspAxes *Mat, int flag);
static char *nsp_axes_type_as_string(void);
static char *nsp_axes_type_short_string(NspObject *v);
static int nsp_axes_eq(NspAxes *A, NspObject *B);
static int nsp_axes_neq(NspAxes *A, NspObject *B);
static NspAxes *nsp_axes_xdr_load(XDR *xdrs);
static AttrTab axes_attrs[];
static NspMethods *axes_get_methods(void);
/* static int int_axes_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspAxes *nsp_axes_create_void(const char *name,NspTypeBase *type);
#line 41 "codegen/axes.override"

/* inserted in the private part of include file
 * of axes.h
 */

static void nsp_draw_axes(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_axes(NspGraphic *o,const double *tr);
static void nsp_rotate_axes(NspGraphic *o,double *R);
static void nsp_scale_axes(NspGraphic *o,double *alpha);
static int nsp_getbounds_axes(NspGraphic *o,double *bounds);
static void nsp_axes_compute_inside_bounds(NspGraphic *Obj,double *bounds);
static void nsp_axes_link_figure(NspGraphic *G, void *F,void *A);
static void nsp_axes_unlink_figure(NspGraphic *G, void *F);
static NspList *nsp_axes_children(NspGraphic *Obj);
static void gr_rescale_new(char *logf, double *FRectI, int *Xdec, int *Ydec, int *xnax, int *ynax);
static int getticks(double xmin,double xmax,double *grads,int *start);

/* requested for nsp_gcscale */

static void nsp_destroy_nsp_gcscale(nsp_gcscale *locks,NspAxes *H);
static int nsp_print_nsp_gcscale(int indent,nsp_gcscale *locks,NspAxes *M);
static int nsp_check_nsp_gcscale(nsp_gcscale *locks,NspAxes *M);
static int nsp_nsp_gcscale_full_copy(NspAxes *C,nsp_gcscale *locks,NspAxes *M);
static int nsp_eq_nsp_gcscale(nsp_gcscale *scale1, nsp_gcscale *scale2);
static void nsp_init_nsp_gcscale(nsp_gcscale *scale);

#line 203 "./axes.h"
#endif /* NspAxes_Private */

