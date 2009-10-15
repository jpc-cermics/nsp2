/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPolyline3d
#define NSP_INC_NspPolyline3d

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspPolyline3d */

#include <nsp/graphic.h>

/*
 * NspPolyline3d inherits from Graphic
 */

typedef struct _NspPolyline3d NspPolyline3d ;
typedef struct _NspTypePolyline3d NspTypePolyline3d ;

#line 22 "./polyline3d.h"

struct _NspTypePolyline3d {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./polyline3d.h"
};

typedef struct _nsp_polyline3d nsp_polyline3d;
struct _nsp_polyline3d {
  NspMatrix* Mcoord;
  void* Mcoord_l;
  NspMatrix* Mcolor;
  int* pos;  int pos_length;
  int ref_count;
};

struct _NspPolyline3d {
  /*< private >*/
  NspGraphic father;
  NspTypePolyline3d*type;
  /*< public >*/
  nsp_polyline3d *obj;
};

extern int nsp_type_polyline3d_id;
extern NspTypePolyline3d *nsp_type_polyline3d;

/* type instances for graphic */

NspTypePolyline3d *new_type_polyline3d(type_mode mode);

/* instance for NspPolyline3d */

NspPolyline3d *new_polyline3d();

/*
 * Object methods redefined for polyline3d 
 */


#define NULLPOLYLINE3D (NspPolyline3d*) 0

extern NspPolyline3d *nsp_polyline3d_create(char *name,NspMatrix* Mcoord,void* Mcoord_l,NspMatrix* Mcolor,int* pos, int pos_length,NspTypeBase *type);
extern NspPolyline3d *nsp_polyline3d_create_default(char *name);

/* from NspPolyline3dObj.c */

extern NspPolyline3d *nsp_polyline3d_copy(NspPolyline3d *H);
extern void nsp_polyline3d_destroy(NspPolyline3d *H);
extern int nsp_polyline3d_info(NspPolyline3d *H, int indent,const char *name, int rec_level);
extern int nsp_polyline3d_print(NspPolyline3d *H, int indent,const char *name, int rec_level);
extern int nsp_polyline3d_latex(NspPolyline3d *H, int indent,const char *name, int rec_level);
extern NspPolyline3d *nsp_polyline3d_object (NspObject *O);
extern int IsPolyline3dObj (Stack stack, int i);
extern int IsPolyline3d(NspObject *O);
extern NspPolyline3d *GetPolyline3dCopy (Stack stack, int i);
extern NspPolyline3d *GetPolyline3d (Stack stack, int i);
extern int nsp_polyline3d_create_partial(NspPolyline3d *H);
extern void nsp_polyline3d_destroy_partial(NspPolyline3d *H);
extern NspPolyline3d * nsp_polyline3d_copy_partial(NspPolyline3d *H,NspPolyline3d *self);
extern NspPolyline3d * nsp_polyline3d_full_copy_partial(NspPolyline3d *H,NspPolyline3d *self);
extern NspPolyline3d * nsp_polyline3d_full_copy(NspPolyline3d *self);
extern int nsp_polyline3d_check_values(NspPolyline3d *H);
extern int int_polyline3d_create(Stack stack, int rhs, int opt, int lhs);
extern NspPolyline3d *nsp_polyline3d_xdr_load_partial(XDR *xdrs, NspPolyline3d *M);
extern int nsp_polyline3d_xdr_save(XDR  *xdrs, NspPolyline3d *M);

#line 4 "codegen/polyline3d.override"

/* inserted at the end of public part of include file */
extern void drawsegments3D(BCG *Xgc,double *x,double *y,double *z, int n, int *style, int iflag);

#line 97 "./polyline3d.h"
#endif /* NSP_INC_NspPolyline3d */ 

#ifdef NspPolyline3d_Private 
static int init_polyline3d(NspPolyline3d *o,NspTypePolyline3d *type);
static int nsp_polyline3d_size(NspPolyline3d *Mat, int flag);
static char *nsp_polyline3d_type_as_string(void);
static char *nsp_polyline3d_type_short_string(NspObject *v);
static int nsp_polyline3d_eq(NspPolyline3d *A, NspObject *B);
static int nsp_polyline3d_neq(NspPolyline3d *A, NspObject *B);
static NspPolyline3d *nsp_polyline3d_xdr_load(XDR *xdrs);
static AttrTab polyline3d_attrs[];
static NspMethods *polyline3d_get_methods(void);
/* static int int_polyline3d_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspPolyline3d *nsp_polyline3d_create_void(char *name,NspTypeBase *type);
#line 10 "codegen/polyline3d.override"

/* inserted in the private part of include file */
static void nsp_draw_polyline3d(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_polyline3d(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_polyline3d(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_polyline3d(BCG *Xgc,NspGraphic *o,double *alpha);
static int nsp_getbounds_polyline3d(BCG *Xgc,NspGraphic *o,double *bounds);

static void nsp_polyline3d_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF,
				 int *n, int k, double *lim);
static int nsp_polyline3d_n_faces(BCG *Xgc,NspGraphic *Obj);
static int nsp_check_polyline3d(NspPolyline3d *P);

static void draw_polyline3d_ogl(BCG *Xgc,void *Ob);
static void draw_polyline3d_face(BCG *Xgc,NspGraphic *Ob, int j);

#line 129 "./polyline3d.h"
#endif /* NspPolyline3d_Private */

