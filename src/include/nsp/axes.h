/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAxes
#define NSP_INC_NspAxes

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#line 4 "codegen/axes.override"
/* inserted at the start of include file */

#include <stdio.h>   /* for file declaration */
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/graphics-new/gcscale.h"

/**
 * NspAxes:
 * @obj: a #nsp_axes pointer 
 *
 * inherits from #NspGraphics 
 */


#line 27 "./axes.h"
/* NspAxes */

#include <nsp/graphic.h>

/*
 * NspAxes inherits from Graphic
 */

typedef struct _NspAxes NspAxes ;
typedef struct _NspTypeAxes NspTypeAxes ;

#line 39 "./axes.h"

struct _NspTypeAxes {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 46 "./axes.h"
};

typedef struct _nsp_axes nsp_axes;
struct _nsp_axes {
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
  nsp_gcscale scale;
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

extern NspAxes *nsp_axes_create(char *name,NspMatrix* wrect,double rho,gboolean top,NspMatrix* bounds,NspMatrix* arect,NspMatrix* frect,char* title,char* x,char* y,NspList* children,gboolean fixed,gboolean iso,gboolean auto_axis,int grid,int axes,gboolean xlog,gboolean ylog,int lpos,NspMatrix* rect,gboolean zoom,NspMatrix* zrect,nsp_gcscale scale,NspTypeBase *type);
extern NspAxes *nsp_axes_create_default(char *name);

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

#line 21 "codegen/axes.override"

/* inserted at the end of public part of include file */

extern NspAxes * nsp_check_for_axes(BCG *Xgc,const double *wrect) ;
extern void nsp_axes_i2f(nsp_axes *A,int x,int y,double pt[2]);
extern NspObject * nsp_check_pt_axes_or_objs3d(BCG *Xgc,const int *pt);
extern BCG *nsp_check_graphic_context(void);
extern void tape_store_graphic_object(BCG *Xgc,NspObject *obj);
extern void nsp_list_unlink_figure(NspList *L, nsp_figure *F);
extern int nsp_list_check_figure(NspList *L, nsp_figure *F);
extern void nsp_strf_axes(BCG *Xgc,NspAxes *A,double *rect, char scale);
extern int nsp_axes_insert_child(NspAxes *A, NspGraphic *G);
extern void nsp_axes_invalidate(NspGraphic *G);

#line 142 "./axes.h"
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
static NspAxes *nsp_axes_create_void(char *name,NspTypeBase *type);
#line 37 "codegen/axes.override"

/* inserted in the private part of include file
 * of classa.h
 */

static void nsp_draw_axes(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_axes(NspGraphic *o,const double *tr);
static void nsp_rotate_axes(NspGraphic *o,double *R);
static void nsp_scale_axes(NspGraphic *o,double *alpha);
static int nsp_getbounds_axes(NspGraphic *o,double *bounds);
static void nsp_axes_compute_inside_bounds(BCG *Xgc,NspGraphic *Obj,double *bounds);
static void nsp_axes_link_figure(NspGraphic *G, void *F,void *A);
static void nsp_axes_unlink_figure(NspGraphic *G, void *F);
static NspList *nsp_axes_children(NspGraphic *Obj);
static void gr_rescale_new(char *logf, double *FRectI, int *Xdec, int *Ydec, int *xnax, int *ynax);

/* requested for nsp_gcscale */

static void nsp_destroy_nsp_gcscale(nsp_gcscale *locks,NspAxes *H);
static int nsp_print_nsp_gcscale(int indent,nsp_gcscale *locks,NspAxes *M);
static int nsp_check_nsp_gcscale(nsp_gcscale *locks,NspAxes *M);
static int nsp_nsp_gcscale_full_copy(NspAxes *C,nsp_gcscale *locks,NspAxes *M);
static int nsp_eq_nsp_gcscale(nsp_gcscale *scale1, nsp_gcscale *scale2);
static void nsp_init_nsp_gcscale(nsp_gcscale *scale);

#line 183 "./axes.h"
#endif /* NspAxes_Private */

