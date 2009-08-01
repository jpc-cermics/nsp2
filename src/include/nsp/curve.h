/* -*- Mode: C -*- */
#ifndef NSP_INC_NspCurve
#define NSP_INC_NspCurve

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspCurve */

#include <nsp/graphic.h>

/*
 * NspCurve inherits from Graphic
 */

typedef struct _NspCurve NspCurve ;
typedef struct _NspTypeNspCurve NspTypeNspCurve ;

#line 22 "./curve.h"

struct _NspTypeNspCurve {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./curve.h"
};

typedef struct _nsp_curve nsp_curve;
struct _nsp_curve {
  int mark;
  int width;
  int style;
  int color;
  int mode;
  NspMatrix* Pts;
  char* legend;
  int ref_count;
};

struct _NspCurve {
  /*< private >*/
  NspGraphic father;
  NspTypeNspCurve*type;
  /*< public >*/
  nsp_curve *obj;
};

extern int nsp_type_curve_id;
extern NspTypeNspCurve *nsp_type_curve;

/* type instances for graphic */

NspTypeNspCurve *new_type_curve(type_mode mode);

/* instance for NspCurve */

NspCurve *new_curve();

/*
* Object methods redefined for curve 
*/


#define NULLCURVE (NspCurve*) 0

extern NspCurve *nsp_curve_create(char *name,int mark,int width,int style,int color,int mode,NspMatrix* Pts,char* legend,NspTypeBase *type);
extern NspCurve *nsp_curve_create_default(char *name);

/* from NspCurveObj.c */

extern NspCurve *nsp_curve_copy(NspCurve *H);
extern void nsp_curve_destroy(NspCurve *H);
extern int nsp_curve_info(NspCurve *H, int indent,const char *name, int rec_level);
extern int nsp_curve_print(NspCurve *H, int indent,const char *name, int rec_level);
extern int nsp_curve_latex(NspCurve *H, int indent,const char *name, int rec_level);
extern NspCurve *nsp_curve_object (NspObject *O); 
extern int IsCurveObj (Stack stack, int i); 
extern int IsCurve(NspObject *O);
extern NspCurve *GetCurveCopy (Stack stack, int i); 
extern NspCurve *GetCurve (Stack stack, int i); 
extern int nsp_curve_create_partial(NspCurve *H);
extern void nsp_curve_destroy_partial(NspCurve *H);
extern NspCurve * nsp_curve_copy_partial(NspCurve *H,NspCurve *self);
extern NspCurve * nsp_curve_full_copy_partial(NspCurve *H,NspCurve *self);
extern NspCurve * nsp_curve_full_copy(NspCurve *self);
extern int nsp_curve_check_values(NspCurve *H);
extern int int_curve_create(Stack stack, int rhs, int opt, int lhs); 
extern NspCurve *nsp_curve_xdr_load_partial(XDR *xdrs, NspCurve *M);
extern int nsp_curve_xdr_save(XDR  *xdrs, NspCurve *M);

#line 4 "codegen/curve.override"

/* inserted at the end of public part of include file */

#line 99 "./curve.h"
#endif /* NSP_INC_NspCurve */ 

#ifdef NspCurve_Private 
static int init_curve(NspCurve *o,NspTypeNspCurve *type);
static int nsp_curve_size(NspCurve *Mat, int flag);
static char *nsp_curve_type_as_string(void);
static char *nsp_curve_type_short_string(NspObject *v);
static int nsp_curve_eq(NspCurve *A, NspObject *B);
static int nsp_curve_neq(NspCurve *A, NspObject *B);
static NspCurve *nsp_curve_xdr_load(XDR *xdrs);
static AttrTab curve_attrs[];
static NspMethods *curve_get_methods(void);
/* static int int_curve_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspCurve *nsp_curve_create_void(char *name,NspTypeBase *type);
#line 9 "codegen/curve.override"

/* inserted in the private part of include file */
static void nsp_draw_curve(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_curve(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_curve(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_curve(BCG *Xgc,NspGraphic *o,double *alpha);
static void nsp_getbounds_curve(BCG *Xgc,NspGraphic *o,double *bounds);

#line 123 "./curve.h"
#endif /* NspCurve_Private */

