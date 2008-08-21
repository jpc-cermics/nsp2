/* -*- Mode: C -*- */
#ifndef NSP_INC_Curve
#define NSP_INC_Curve

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Curve */

#include "nsp/graphic.h"

/*
 * NspCurve inherits from NspGraphic
 */

typedef struct _NspCurve NspCurve ;
typedef struct _NspTypeCurve NspTypeCurve ;

#line 22 "./curve.h"

struct _NspTypeCurve {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./curve.h"
};

typedef struct _nsp_curve nsp_curve;
struct _nsp_curve {
  int mark;
  double width;
  int style;
  int mode;
  NspMatrix* Pts;
  int ref_count;
};

struct _NspCurve {
  /*< private >*/
  NspGraphic father;
  NspTypeCurve*type;
  /*< public >*/
  nsp_curve *obj;
};

extern int nsp_type_curve_id;
extern NspTypeCurve *nsp_type_curve;

/* type instances for graphic */

NspTypeCurve *new_type_curve(type_mode mode);

/* instance for Curve */

NspCurve *new_curve();

/*
* Object methods redefined for curve 
*/


#define NULLCURVE (NspCurve*) 0

extern NspCurve *nsp_curve_create(char *name,int mark,double width,int style,int mode,NspMatrix* Pts,NspTypeBase *type);

/* from CurveObj.c */

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

#endif /* NSP_INC_Curve */ 

#ifdef Curve_Private 
static int init_curve(NspCurve *o,NspTypeCurve *type);
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
#endif /* Curve_Private */

