/* -*- Mode: C -*- */
#ifndef NSP_INC_NspQcurve
#define NSP_INC_NspQcurve

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspQcurve */

#include <nsp/graphic.h>

/*
 * NspQcurve inherits from Graphic
 */

typedef struct _NspQcurve NspQcurve ;
typedef struct _NspTypeQcurve NspTypeQcurve ;

#line 22 "./qcurve.h"

struct _NspTypeQcurve {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./qcurve.h"
};

typedef struct _nsp_qcurve nsp_qcurve;
struct _nsp_qcurve {
  int mark;
  int width;
  int style;
  int color;
  int mode;
  NspMatrix* Pts;
  char* legend;
  int start;
  int last;
  int ref_count;
};

struct _NspQcurve {
  /*< private >*/
  NspGraphic father;
  NspTypeQcurve*type;
  /*< public >*/
  nsp_qcurve *obj;
};

extern int nsp_type_qcurve_id;
extern NspTypeQcurve *nsp_type_qcurve;

/* type instances for graphic */

NspTypeQcurve *new_type_qcurve(type_mode mode);

/* instance for NspQcurve */

NspQcurve *new_qcurve();

/*
 * Object methods redefined for qcurve 
 */


#define NULLQCURVE (NspQcurve*) 0

extern NspQcurve *nsp_qcurve_create(char *name,int mark,int width,int style,int color,int mode,NspMatrix* Pts,char* legend,int start,int last,NspTypeBase *type);
extern NspQcurve *nsp_qcurve_create_default(char *name);

/* from NspQcurveObj.c */

extern NspQcurve *nsp_qcurve_copy(NspQcurve *H);
extern void nsp_qcurve_destroy(NspQcurve *H);
extern int nsp_qcurve_info(NspQcurve *H, int indent,const char *name, int rec_level);
extern int nsp_qcurve_print(NspQcurve *H, int indent,const char *name, int rec_level);
extern int nsp_qcurve_latex(NspQcurve *H, int indent,const char *name, int rec_level);
extern NspQcurve *nsp_qcurve_object (NspObject *O);
extern int IsQcurveObj (Stack stack, int i);
extern int IsQcurve(NspObject *O);
extern NspQcurve *GetQcurveCopy (Stack stack, int i);
extern NspQcurve *GetQcurve (Stack stack, int i);
extern int nsp_qcurve_create_partial(NspQcurve *H);
extern void nsp_qcurve_destroy_partial(NspQcurve *H);
extern NspQcurve * nsp_qcurve_copy_partial(NspQcurve *H,NspQcurve *self);
extern NspQcurve * nsp_qcurve_full_copy_partial(NspQcurve *H,NspQcurve *self);
extern NspQcurve * nsp_qcurve_full_copy(NspQcurve *self);
extern int nsp_qcurve_check_values(NspQcurve *H);
extern int int_qcurve_create(Stack stack, int rhs, int opt, int lhs);
extern NspQcurve *nsp_qcurve_xdr_load_partial(XDR *xdrs, NspQcurve *M);
extern int nsp_qcurve_xdr_save(XDR  *xdrs, NspQcurve *M);

#line 4 "codegen/qcurve.override"

/* inserted at the end of public part of include file */

extern NspFigure *nsp_oscillo_obj(int win,int ncurves,int style[],int bufsize,int yfree,
				  double ymin,double ymax,NspList **Lc);
extern void  nsp_oscillo_add_point(NspList *L,double t,double *y, int n);


#line 106 "./qcurve.h"
#endif /* NSP_INC_NspQcurve */ 

#ifdef NspQcurve_Private 
static int init_qcurve(NspQcurve *o,NspTypeQcurve *type);
static int nsp_qcurve_size(NspQcurve *Mat, int flag);
static char *nsp_qcurve_type_as_string(void);
static char *nsp_qcurve_type_short_string(NspObject *v);
static int nsp_qcurve_eq(NspQcurve *A, NspObject *B);
static int nsp_qcurve_neq(NspQcurve *A, NspObject *B);
static NspQcurve *nsp_qcurve_xdr_load(XDR *xdrs);
static AttrTab qcurve_attrs[];
static NspMethods *qcurve_get_methods(void);
/* static int int_qcurve_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspQcurve *nsp_qcurve_create_void(char *name,NspTypeBase *type);
#line 14 "codegen/qcurve.override"

/* inserted in the private part of include file */
static void nsp_draw_qcurve(BCG *Xgc,NspGraphic *Obj, GdkRectangle *rect,void *data);
static void nsp_translate_qcurve(NspGraphic *o,const double *tr);
static void nsp_rotate_qcurve(NspGraphic *o,double *R);
static void nsp_scale_qcurve(NspGraphic *o,double *alpha);
static int nsp_getbounds_qcurve(NspGraphic *o,double *bounds);
static void nsp_qcurve_addPts(NspQcurve *C,NspMatrix *Pts);
static void nsp_qcurve_addpt(NspQcurve *C,double *x,double *y,int n);
static void nsp_qcurve_clear(NspQcurve *C);
static int nsp_qcurve_get_len(NspQcurve *C);
static void nsp_qcurve_get_xy(NspQcurve *C,double *cx,double *cy);
static void oscillo_test();

#line 136 "./qcurve.h"
#endif /* NspQcurve_Private */

