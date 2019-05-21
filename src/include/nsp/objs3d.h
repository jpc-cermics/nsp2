/* -*- Mode: C -*- */
#ifndef NSP_INC_NspObjs3d
#define NSP_INC_NspObjs3d

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

#line 4 "codegen/objs3d.override"
/* inserted at the start of include file */
#include <nsp/figure.h>
#include <nsp/axes.h>
#include <nsp/spolyhedron.h>
#include <nsp/polyhedron.h>
#include "../graphics-new/Plo3dObj.h"
#include <nsp/grcommon.h>

#line 34 "./objs3d.h"
/* NspObjs3d */

#include <nsp/graphic.h>

/*
 * NspObjs3d inherits from Graphic
 */

typedef struct _NspObjs3d NspObjs3d ;
typedef struct _NspTypeObjs3d NspTypeObjs3d ;

struct _NspTypeObjs3d {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_objs3d nsp_objs3d;
struct _nsp_objs3d {
  nsp_gcscale scale;
  NspMatrix* wrect;
  double rho;
  gboolean top;
  NspMatrix* bounds;
  NspMatrix* arect;
  NspMatrix* frect;
  char* title;
  NspList* children;
  NspMatrix* colormap;
  double alpha;
  double theta;
  gboolean with_box;
  int box_color;
  int box_style;
  gboolean fixed;
  NspMatrix* ebox;
  int scale_flag;
  int ref_count;
};

struct _NspObjs3d {
  /*< private >*/
  NspGraphic father;
  NspTypeObjs3d*type;
  /*< public >*/
  nsp_objs3d *obj;
};

extern int nsp_type_objs3d_id;
extern NspTypeObjs3d *nsp_type_objs3d;

/* type instances for graphic */

NspTypeObjs3d *new_type_objs3d(type_mode mode);

/* instance for NspObjs3d */

NspObjs3d *new_objs3d();

/*
 * Object methods redefined for objs3d 
 */


#define NULLOBJS3D (NspObjs3d*) 0

extern NspObjs3d *nsp_objs3d_create(const char *name,nsp_gcscale scale,NspMatrix* wrect,double rho,gboolean top,NspMatrix* bounds,NspMatrix* arect,NspMatrix* frect,char* title,NspList* children,NspMatrix* colormap,double alpha,double theta,gboolean with_box,int box_color,int box_style,gboolean fixed,NspMatrix* ebox,int scale_flag,NspTypeBase *type);
extern NspObjs3d *nsp_objs3d_create_default(const char *name);

/* from NspObjs3dObj.c */

extern NspObjs3d *nsp_objs3d_copy(NspObjs3d *H);
extern void nsp_objs3d_destroy(NspObjs3d *H);
extern int nsp_objs3d_info(NspObjs3d *H, int indent,const char *name, int rec_level);
extern int nsp_objs3d_print(NspObjs3d *H, int indent,const char *name, int rec_level);
extern int nsp_objs3d_latex(NspObjs3d *H, int indent,const char *name, int rec_level);
extern NspObjs3d *nsp_objs3d_object (NspObject *O);
extern int IsObjs3dObj (Stack stack, int i);
extern int IsObjs3d(NspObject *O);
extern NspObjs3d *GetObjs3dCopy (Stack stack, int i);
extern NspObjs3d *GetObjs3d (Stack stack, int i);
extern int nsp_objs3d_create_partial(NspObjs3d *H);
extern void nsp_objs3d_destroy_partial(NspObjs3d *H);
extern NspObjs3d * nsp_objs3d_copy_partial(NspObjs3d *H,NspObjs3d *self);
extern NspObjs3d * nsp_objs3d_full_copy_partial(NspObjs3d *H,NspObjs3d *self);
extern NspObjs3d * nsp_objs3d_full_copy(NspObjs3d *self);
extern int nsp_objs3d_check_values(NspObjs3d *H);
extern int int_objs3d_create(Stack stack, int rhs, int opt, int lhs);
extern NspObjs3d *nsp_objs3d_xdr_load_partial(XDR *xdrs, NspObjs3d *M);
extern int nsp_objs3d_xdr_save(XDR  *xdrs, NspObjs3d *M);

#line 14 "codegen/objs3d.override"

/* inserted at the end of public part of include file */
extern NspObjs3d * nsp_check_for_objs3d(BCG *Xgc,const double *wrect);
extern int nsp_objs3d_insert_child(NspObjs3d *A, NspGraphic *G,int invalidate);
extern void nsp_objs3d_invalidate(NspGraphic *G);
extern int gr_compute_ticks(double *xminv, double *xmaxv, double *grads, int *ngrads);
extern void drawsegments3D(BCG *Xgc,double *x,double *y,double *z, int n, int *style, int iflag);
extern int nsp_figure_change3d_orientation(BCG *Xgc,double theta,double alpha,const int *pt);
extern NspObjs3d *nsp_check_for_current_objs3d(int create);
extern NspObjs3d * nsp_check_for_objs3d_in_figure(NspFigure *F,const double *wrect,int create);
extern void nsp_strf_objs3d(NspObjs3d *A,double *ebox, int scale);
extern void apply_transforms_new(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[],
			  int Mm,int ncoord);
extern void apply_transforms_new1(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],
				  const double lim[], int ncoord);

extern int nsp_objs3d_remove_children(NspObjs3d *O);
extern void nsp_draw_objs3d_colorbar(BCG *Xgc,nsp_objs3d *P,double vmin , double vmax, int *colminmax);

#line 146 "./objs3d.h"
#endif /* NSP_INC_NspObjs3d */ 

#ifdef NspObjs3d_Private 
static int init_objs3d(NspObjs3d *o,NspTypeObjs3d *type);
static int nsp_objs3d_size(NspObjs3d *Mat, int flag);
static char *nsp_objs3d_type_as_string(void);
static char *nsp_objs3d_type_short_string(NspObject *v);
static int nsp_objs3d_eq(NspObjs3d *A, NspObject *B);
static int nsp_objs3d_neq(NspObjs3d *A, NspObject *B);
static NspObjs3d *nsp_objs3d_xdr_load(XDR *xdrs);
static AttrTab objs3d_attrs[];
static NspMethods *objs3d_get_methods(void);
/* static int int_objs3d_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspObjs3d *nsp_objs3d_create_void(const char *name,NspTypeBase *type);
#line 35 "codegen/objs3d.override"

/* inserted in the private part of include file */
static void nsp_draw_objs3d(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_objs3d(NspGraphic *o,const double *tr);
static void nsp_rotate_objs3d(NspGraphic *o,double *R);
static void nsp_scale_objs3d(NspGraphic *o,double *alpha);
static int nsp_getbounds_objs3d(NspGraphic *o,double *bounds);
static void nsp_objs3d_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj,double *bounds);
static void nsp_objs3d_link_figure(NspGraphic *G, void *F, void *A);
static void nsp_objs3d_unlink_figure(NspGraphic *G, void *F);
static NspList *nsp_objs3d_children(NspGraphic *Obj);
static void nsp_draw_objs3d_s2( BCG *Xgc,NspObjs3d *Obj,double theta,double alpha,
				const char *legend,
				int *flag,double *ebox,int with_box,int box_color,int box_style);
static Plot3dBox* make_box(BCG *Xgc,double Box[], GBoolean with_ticks, BoxStyle box_style,int box_color, double lim[],
			   const char *legend);
static void nsp_obj3d_draw_box(BCG *Xgc,Plot3dBox *B, int foreground_color);
static void nsp_obj3d_draw_near_box_segments(BCG *Xgc,Plot3dBox *B, int foreground_color);
static void nsp_obj3d_free_box(Plot3dBox *B);
static void nsp_obj3d_dsortc(double x[], int *n, int p[]);
static void nsp_plot3d_update_bounds(BCG *Xgc,char *name, double *x, double *y,
				     double *z, int *p, int *q, double *teta,
				     double *alpha,const char *legend, int *flag,
				     double *bbox,double *zmin,
				     double *zmax,nsp_plot3d_type type3d);
static void SetEch3d1(BCG *Xgc, nsp_box_3d *box,const double *bbox, double Teta, double Alpha, int flag);

/* requested for nsp_gcscale */

static void nsp_destroy_nsp_gcscale(nsp_gcscale *locks,NspObjs3d *H);
static int nsp_print_nsp_gcscale(int indent,nsp_gcscale *locks,NspObjs3d *M);
static int nsp_check_nsp_gcscale(nsp_gcscale *locks,NspObjs3d *M);
static int nsp_nsp_gcscale_full_copy(NspObjs3d *C,nsp_gcscale *locks,NspObjs3d *M);
static int nsp_eq_nsp_gcscale(nsp_gcscale *scale1, nsp_gcscale *scale2);
static void nsp_init_nsp_gcscale(nsp_gcscale *scale);
static NspMatrix *nsp_objs3d_get_ebox(NspObjs3d *self);
#line 198 "./objs3d.h"
#endif /* NspObjs3d_Private */

