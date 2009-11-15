/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPolyhedron
#define NSP_INC_NspPolyhedron

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspPolyhedron */

#include <nsp/graphic.h>

/*
 * NspPolyhedron inherits from Graphic
 */

typedef struct _NspPolyhedron NspPolyhedron ;
typedef struct _NspTypePolyhedron NspTypePolyhedron ;

#line 22 "./polyhedron.h"

struct _NspTypePolyhedron {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./polyhedron.h"
};

typedef struct _nsp_polyhedron nsp_polyhedron;
struct _nsp_polyhedron {
  NspMatrix* Mcoord;
  void* Mcoord_l;
  NspMatrix* Mface;
  NspMatrix* Mcolor;
  NspMatrix* Mback_color;
  gboolean mesh;
  int* pos;  int pos_length;
  int ref_count;
};

struct _NspPolyhedron {
  /*< private >*/
  NspGraphic father;
  NspTypePolyhedron*type;
  /*< public >*/
  nsp_polyhedron *obj;
};

extern int nsp_type_polyhedron_id;
extern NspTypePolyhedron *nsp_type_polyhedron;

/* type instances for graphic */

NspTypePolyhedron *new_type_polyhedron(type_mode mode);

/* instance for NspPolyhedron */

NspPolyhedron *new_polyhedron();

/*
 * Object methods redefined for polyhedron 
 */


#define NULLPOLYHEDRON (NspPolyhedron*) 0

extern NspPolyhedron *nsp_polyhedron_create(char *name,NspMatrix* Mcoord,void* Mcoord_l,NspMatrix* Mface,NspMatrix* Mcolor,NspMatrix* Mback_color,gboolean mesh,int* pos, int pos_length,NspTypeBase *type);
extern NspPolyhedron *nsp_polyhedron_create_default(char *name);

/* from NspPolyhedronObj.c */

extern NspPolyhedron *nsp_polyhedron_copy(NspPolyhedron *H);
extern void nsp_polyhedron_destroy(NspPolyhedron *H);
extern int nsp_polyhedron_info(NspPolyhedron *H, int indent,const char *name, int rec_level);
extern int nsp_polyhedron_print(NspPolyhedron *H, int indent,const char *name, int rec_level);
extern int nsp_polyhedron_latex(NspPolyhedron *H, int indent,const char *name, int rec_level);
extern NspPolyhedron *nsp_polyhedron_object (NspObject *O);
extern int IsPolyhedronObj (Stack stack, int i);
extern int IsPolyhedron(NspObject *O);
extern NspPolyhedron *GetPolyhedronCopy (Stack stack, int i);
extern NspPolyhedron *GetPolyhedron (Stack stack, int i);
extern int nsp_polyhedron_create_partial(NspPolyhedron *H);
extern void nsp_polyhedron_destroy_partial(NspPolyhedron *H);
extern NspPolyhedron * nsp_polyhedron_copy_partial(NspPolyhedron *H,NspPolyhedron *self);
extern NspPolyhedron * nsp_polyhedron_full_copy_partial(NspPolyhedron *H,NspPolyhedron *self);
extern NspPolyhedron * nsp_polyhedron_full_copy(NspPolyhedron *self);
extern int nsp_polyhedron_check_values(NspPolyhedron *H);
extern int int_polyhedron_create(Stack stack, int rhs, int opt, int lhs);
extern NspPolyhedron *nsp_polyhedron_xdr_load_partial(XDR *xdrs, NspPolyhedron *M);
extern int nsp_polyhedron_xdr_save(XDR  *xdrs, NspPolyhedron *M);

#line 4 "codegen/polyhedron.override"
/* inserted at the end of public part of include file */

extern NspPolyhedron *nsp_polyhedron_create_from_triplet(char *name,double *x,double *y,double *z,int m,int n);
extern NspPolyhedron *nsp_polyhedron_create_from_facets(char *name,double *xx,double *yy,double *zz,int m,int n);
extern NspMatrix *nsp_surf_to_coords(const char *name,double *x,double *y,double *z,int m,int n);
extern NspMatrix *nsp_surf_to_faces(const char *name,double *x,int xmn,double *y,int ymn) ; 
extern int nsp_facets_to_faces(double *x,double *y,double *z,int *colors,int ncol, int m,int n,
			       NspMatrix **Cr,NspMatrix **Fr,NspMatrix **Colr);
extern int nsp_obj3d_orientation(int x[], int y[], int n);

#line 106 "./polyhedron.h"
#endif /* NSP_INC_NspPolyhedron */ 

#ifdef NspPolyhedron_Private 
static int init_polyhedron(NspPolyhedron *o,NspTypePolyhedron *type);
static int nsp_polyhedron_size(NspPolyhedron *Mat, int flag);
static char *nsp_polyhedron_type_as_string(void);
static char *nsp_polyhedron_type_short_string(NspObject *v);
static int nsp_polyhedron_eq(NspPolyhedron *A, NspObject *B);
static int nsp_polyhedron_neq(NspPolyhedron *A, NspObject *B);
static NspPolyhedron *nsp_polyhedron_xdr_load(XDR *xdrs);
static AttrTab polyhedron_attrs[];
static NspMethods *polyhedron_get_methods(void);
/* static int int_polyhedron_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspPolyhedron *nsp_polyhedron_create_void(char *name,NspTypeBase *type);
#line 16 "codegen/polyhedron.override"
/* inserted in the private part of include file */
static void nsp_draw_polyhedron(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_polyhedron(NspGraphic *o,const double *tr);
static void nsp_rotate_polyhedron(NspGraphic *o,double *R);
static void nsp_scale_polyhedron(NspGraphic *o,double *alpha);
static int nsp_getbounds_polyhedron(NspGraphic *o,double *bounds);
static void nsp_polyhedron_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim);
static int nsp_polyhedron_n_faces(BCG *Xgc,NspGraphic *Obj);
static int nsp_check_polyhedron(NspPolyhedron *P);
static void draw_polyhedron_ogl(BCG *Xgc,void *Ob);
static void draw_polyhedron_face(BCG *Xgc,NspGraphic *Ob, int j);

#line 134 "./polyhedron.h"
#endif /* NspPolyhedron_Private */

