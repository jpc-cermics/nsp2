/* -*- Mode: C -*- */
#ifndef NSP_INC_NspContour
#define NSP_INC_NspContour

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspContour */

#include <nsp/graphic.h>

/*
 * NspContour inherits from Graphic
 */

typedef struct _NspContour NspContour ;
typedef struct _NspTypeNspContour NspTypeNspContour ;

#line 22 "./contour.h"

struct _NspTypeNspContour {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./contour.h"
};

typedef struct _nsp_contour nsp_contour;
struct _nsp_contour {
  NspMatrix* z;
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* levels;
  int nlevels;
  NspMatrix* style;
  int ref_count;
};

struct _NspContour {
  /*< private >*/
  NspGraphic father;
  NspTypeNspContour*type;
  /*< public >*/
  nsp_contour *obj;
};

extern int nsp_type_contour_id;
extern NspTypeNspContour *nsp_type_contour;

/* type instances for graphic */

NspTypeNspContour *new_type_contour(type_mode mode);

/* instance for NspContour */

NspContour *new_contour();

/*
* Object methods redefined for contour 
*/


#define NULLCONTOUR (NspContour*) 0

extern NspContour *nsp_contour_create(char *name,NspMatrix* z,NspMatrix* x,NspMatrix* y,NspMatrix* levels,int nlevels,NspMatrix* style,NspTypeBase *type);
extern NspContour *nsp_contour_create_default(char *name);

/* from NspContourObj.c */

extern NspContour *nsp_contour_copy(NspContour *H);
extern void nsp_contour_destroy(NspContour *H);
extern int nsp_contour_info(NspContour *H, int indent,const char *name, int rec_level);
extern int nsp_contour_print(NspContour *H, int indent,const char *name, int rec_level);
extern int nsp_contour_latex(NspContour *H, int indent,const char *name, int rec_level);
extern NspContour *nsp_contour_object (NspObject *O); 
extern int IsContourObj (Stack stack, int i); 
extern int IsContour(NspObject *O);
extern NspContour *GetContourCopy (Stack stack, int i); 
extern NspContour *GetContour (Stack stack, int i); 
extern int nsp_contour_create_partial(NspContour *H);
extern void nsp_contour_destroy_partial(NspContour *H);
extern NspContour * nsp_contour_copy_partial(NspContour *H,NspContour *self);
extern NspContour * nsp_contour_full_copy_partial(NspContour *H,NspContour *self);
extern NspContour * nsp_contour_full_copy(NspContour *self);
extern int nsp_contour_check_values(NspContour *H);
extern int int_contour_create(Stack stack, int rhs, int opt, int lhs); 
extern NspContour *nsp_contour_xdr_load_partial(XDR *xdrs, NspContour *M);
extern int nsp_contour_xdr_save(XDR  *xdrs, NspContour *M);

#line 4 "codegen/contour.override"

extern int nsp_contour2_obj(BCG *Xgc,double *x, double *y, double *z, int *n1, int *n2, 
			    int *flagnz, int *nz, double *zz, int *style);

/* inserted at the end of public part of include file */

#line 101 "./contour.h"
#endif /* NSP_INC_NspContour */ 

#ifdef NspContour_Private 
static int init_contour(NspContour *o,NspTypeNspContour *type);
static int nsp_contour_size(NspContour *Mat, int flag);
static char *nsp_contour_type_as_string(void);
static char *nsp_contour_type_short_string(NspObject *v);
static int nsp_contour_eq(NspContour *A, NspObject *B);
static int nsp_contour_neq(NspContour *A, NspObject *B);
static NspContour *nsp_contour_xdr_load(XDR *xdrs);
static AttrTab contour_attrs[];
static NspMethods *contour_get_methods(void);
/* static int int_contour_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspContour *nsp_contour_create_void(char *name,NspTypeBase *type);
#line 12 "codegen/contour.override"

/* inserted in the private part of include file */

static void nsp_draw_contour(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_contour(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_contour(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_contour(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_contour(BCG *Xgc,NspGraphic *o,double *bounds);

#line 126 "./contour.h"
#endif /* NspContour_Private */

