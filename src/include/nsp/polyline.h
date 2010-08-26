/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPolyline
#define NSP_INC_NspPolyline

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#line 4 "codegen/polyline.override"
/* inserted at the start of include file */
#include <nsp/figure.h>

#line 15 "./polyline.h"
/* NspPolyline */

#include <nsp/graphic.h>

/*
 * NspPolyline inherits from Graphic
 */

typedef struct _NspPolyline NspPolyline ;
typedef struct _NspTypePolyline NspTypePolyline ;

#line 27 "./polyline.h"

struct _NspTypePolyline {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 34 "./polyline.h"
};

typedef struct _nsp_polyline nsp_polyline;
struct _nsp_polyline {
  NspMatrix* x;
  NspMatrix* y;
  gboolean close;
  int color;
  int mark;
  int mark_size;
  int fill_color;
  int thickness;
  int ref_count;
};

struct _NspPolyline {
  /*< private >*/
  NspGraphic father;
  NspTypePolyline*type;
  /*< public >*/
  nsp_polyline *obj;
};

extern int nsp_type_polyline_id;
extern NspTypePolyline *nsp_type_polyline;

/* type instances for graphic */

NspTypePolyline *new_type_polyline(type_mode mode);

/* instance for NspPolyline */

NspPolyline *new_polyline();

/*
 * Object methods redefined for polyline 
 */


#define NULLPOLYLINE (NspPolyline*) 0

extern NspPolyline *nsp_polyline_create(char *name,NspMatrix* x,NspMatrix* y,gboolean close,int color,int mark,int mark_size,int fill_color,int thickness,NspTypeBase *type);
extern NspPolyline *nsp_polyline_create_default(char *name);

/* from NspPolylineObj.c */

extern NspPolyline *nsp_polyline_copy(NspPolyline *H);
extern void nsp_polyline_destroy(NspPolyline *H);
extern int nsp_polyline_info(NspPolyline *H, int indent,const char *name, int rec_level);
extern int nsp_polyline_print(NspPolyline *H, int indent,const char *name, int rec_level);
extern int nsp_polyline_latex(NspPolyline *H, int indent,const char *name, int rec_level);
extern NspPolyline *nsp_polyline_object (NspObject *O);
extern int IsPolylineObj (Stack stack, int i);
extern int IsPolyline(NspObject *O);
extern NspPolyline *GetPolylineCopy (Stack stack, int i);
extern NspPolyline *GetPolyline (Stack stack, int i);
extern int nsp_polyline_create_partial(NspPolyline *H);
extern void nsp_polyline_destroy_partial(NspPolyline *H);
extern NspPolyline * nsp_polyline_copy_partial(NspPolyline *H,NspPolyline *self);
extern NspPolyline * nsp_polyline_full_copy_partial(NspPolyline *H,NspPolyline *self);
extern NspPolyline * nsp_polyline_full_copy(NspPolyline *self);
extern int nsp_polyline_check_values(NspPolyline *H);
extern int int_polyline_create(Stack stack, int rhs, int opt, int lhs);
extern NspPolyline *nsp_polyline_xdr_load_partial(XDR *xdrs, NspPolyline *M);
extern int nsp_polyline_xdr_save(XDR  *xdrs, NspPolyline *M);

#line 9 "codegen/polyline.override"
/* inserted at the end of public part of include file */

#line 104 "./polyline.h"
#endif /* NSP_INC_NspPolyline */ 

#ifdef NspPolyline_Private 
static int init_polyline(NspPolyline *o,NspTypePolyline *type);
static int nsp_polyline_size(NspPolyline *Mat, int flag);
static char *nsp_polyline_type_as_string(void);
static char *nsp_polyline_type_short_string(NspObject *v);
static int nsp_polyline_eq(NspPolyline *A, NspObject *B);
static int nsp_polyline_neq(NspPolyline *A, NspObject *B);
static NspPolyline *nsp_polyline_xdr_load(XDR *xdrs);
static AttrTab polyline_attrs[];
static NspMethods *polyline_get_methods(void);
/* static int int_polyline_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspPolyline *nsp_polyline_create_void(char *name,NspTypeBase *type);
#line 13 "codegen/polyline.override"
/* inserted in the private part of include file */
static void nsp_draw_polyline(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_polyline(NspGraphic *o,const double *tr);
static void nsp_rotate_polyline(NspGraphic *o,double *R);
static void nsp_scale_polyline(NspGraphic *o,double *alpha);
static int nsp_getbounds_polyline(NspGraphic *o,double *bounds);

#line 127 "./polyline.h"
#endif /* NspPolyline_Private */

