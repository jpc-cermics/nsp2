/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGrRect
#define NSP_INC_NspGrRect

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspGrRect */

#include <nsp/graphic.h>

/*
 * NspGrRect inherits from Graphic
 */

typedef struct _NspGrRect NspGrRect ;
typedef struct _NspTypeGrRect NspTypeGrRect ;

#line 22 "./grrect.h"

struct _NspTypeGrRect {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./grrect.h"
};

typedef struct _nsp_grrect nsp_grrect;
struct _nsp_grrect {
  double x;
  double y;
  double w;
  double h;
  int fill_color;
  int thickness;
  int color;
  int ref_count;
};

struct _NspGrRect {
  /*< private >*/
  NspGraphic father;
  NspTypeGrRect*type;
  /*< public >*/
  nsp_grrect *obj;
};

extern int nsp_type_grrect_id;
extern NspTypeGrRect *nsp_type_grrect;

/* type instances for graphic */

NspTypeGrRect *new_type_grrect(type_mode mode);

/* instance for NspGrRect */

NspGrRect *new_grrect();

/*
 * Object methods redefined for grrect 
 */


#define NULLGRRECT (NspGrRect*) 0

extern NspGrRect *nsp_grrect_create(char *name,double x,double y,double w,double h,int fill_color,int thickness,int color,NspTypeBase *type);
extern NspGrRect *nsp_grrect_create_default(char *name);

/* from NspGrRectObj.c */

extern NspGrRect *nsp_grrect_copy(NspGrRect *H);
extern void nsp_grrect_destroy(NspGrRect *H);
extern int nsp_grrect_info(NspGrRect *H, int indent,const char *name, int rec_level);
extern int nsp_grrect_print(NspGrRect *H, int indent,const char *name, int rec_level);
extern int nsp_grrect_latex(NspGrRect *H, int indent,const char *name, int rec_level);
extern NspGrRect *nsp_grrect_object (NspObject *O);
extern int IsGrRectObj (Stack stack, int i);
extern int IsGrRect(NspObject *O);
extern NspGrRect *GetGrRectCopy (Stack stack, int i);
extern NspGrRect *GetGrRect (Stack stack, int i);
extern int nsp_grrect_create_partial(NspGrRect *H);
extern void nsp_grrect_destroy_partial(NspGrRect *H);
extern NspGrRect * nsp_grrect_copy_partial(NspGrRect *H,NspGrRect *self);
extern NspGrRect * nsp_grrect_full_copy_partial(NspGrRect *H,NspGrRect *self);
extern NspGrRect * nsp_grrect_full_copy(NspGrRect *self);
extern int nsp_grrect_check_values(NspGrRect *H);
extern int int_grrect_create(Stack stack, int rhs, int opt, int lhs);
extern NspGrRect *nsp_grrect_xdr_load_partial(XDR *xdrs, NspGrRect *M);
extern int nsp_grrect_xdr_save(XDR  *xdrs, NspGrRect *M);

#line 4 "codegen/grrect.override"

/* inserted at the end of public part of include file */

#line 99 "./grrect.h"
#endif /* NSP_INC_NspGrRect */ 

#ifdef NspGrRect_Private 
static int init_grrect(NspGrRect *o,NspTypeGrRect *type);
static int nsp_grrect_size(NspGrRect *Mat, int flag);
static char *nsp_grrect_type_as_string(void);
static char *nsp_grrect_type_short_string(NspObject *v);
static int nsp_grrect_eq(NspGrRect *A, NspObject *B);
static int nsp_grrect_neq(NspGrRect *A, NspObject *B);
static NspGrRect *nsp_grrect_xdr_load(XDR *xdrs);
static AttrTab grrect_attrs[];
static NspMethods *grrect_get_methods(void);
/* static int int_grrect_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGrRect *nsp_grrect_create_void(char *name,NspTypeBase *type);
#line 9 "codegen/grrect.override"

/* inserted in the private part of include file */
static void nsp_draw_grrect(BCG *Xgc,NspGraphic *Obj, const GdkRectangle *rect,void *data);
static void nsp_translate_grrect(NspGraphic *o,const double *tr);
static void nsp_rotate_grrect(NspGraphic *o,double *R);
static void nsp_scale_grrect(NspGraphic *o,double *alpha);
static int nsp_getbounds_grrect(NspGraphic *o,double *bounds);

#line 123 "./grrect.h"
#endif /* NspGrRect_Private */

