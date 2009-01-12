/* -*- Mode: C -*- */
#ifndef NSP_INC_GrArc
#define NSP_INC_GrArc

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* GrArc */

#include "nsp/graphic.h"

/*
 * NspGrArc inherits from NspGraphic
 */

typedef struct _NspGrArc NspGrArc ;
typedef struct _NspTypeGrArc NspTypeGrArc ;

#line 22 "./grarc.h"

struct _NspTypeGrArc {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./grarc.h"
};

typedef struct _nsp_grarc nsp_grarc;
struct _nsp_grarc {
  double x;
  double y;
  double w;
  double h;
  double a1;
  double a2;
  int fill_color;
  int thickness;
  int color;
  int ref_count;
};

struct _NspGrArc {
  /*< private >*/
  NspGraphic father;
  NspTypeGrArc*type;
  /*< public >*/
  nsp_grarc *obj;
};

extern int nsp_type_grarc_id;
extern NspTypeGrArc *nsp_type_grarc;

/* type instances for graphic */

NspTypeGrArc *new_type_grarc(type_mode mode);

/* instance for GrArc */

NspGrArc *new_grarc();

/*
* Object methods redefined for grarc 
*/


#define NULLGRARC (NspGrArc*) 0

extern NspGrArc *nsp_grarc_create(char *name,double x,double y,double w,double h,double a1,double a2,int fill_color,int thickness,int color,NspTypeBase *type);

/* from GrArcObj.c */

extern NspGrArc *nsp_grarc_copy(NspGrArc *H);
extern void nsp_grarc_destroy(NspGrArc *H);
extern int nsp_grarc_info(NspGrArc *H, int indent,const char *name, int rec_level);
extern int nsp_grarc_print(NspGrArc *H, int indent,const char *name, int rec_level);
extern int nsp_grarc_latex(NspGrArc *H, int indent,const char *name, int rec_level);
extern NspGrArc *nsp_grarc_object (NspObject *O); 
extern int IsGrArcObj (Stack stack, int i); 
extern int IsGrArc(NspObject *O);
extern NspGrArc *GetGrArcCopy (Stack stack, int i); 
extern NspGrArc *GetGrArc (Stack stack, int i); 
extern int nsp_grarc_create_partial(NspGrArc *H);
extern void nsp_grarc_destroy_partial(NspGrArc *H);
extern NspGrArc * nsp_grarc_copy_partial(NspGrArc *H,NspGrArc *self);
extern NspGrArc * nsp_grarc_full_copy_partial(NspGrArc *H,NspGrArc *self);
extern NspGrArc * nsp_grarc_full_copy(NspGrArc *self);
extern int nsp_grarc_check_values(NspGrArc *H);
extern int int_grarc_create(Stack stack, int rhs, int opt, int lhs); 
extern NspGrArc *nsp_grarc_xdr_load_partial(XDR *xdrs, NspGrArc *M);
extern int nsp_grarc_xdr_save(XDR  *xdrs, NspGrArc *M);

#endif /* NSP_INC_GrArc */ 

#ifdef GrArc_Private 
static int init_grarc(NspGrArc *o,NspTypeGrArc *type);
static int nsp_grarc_size(NspGrArc *Mat, int flag);
static char *nsp_grarc_type_as_string(void);
static char *nsp_grarc_type_short_string(NspObject *v);
static int nsp_grarc_eq(NspGrArc *A, NspObject *B);
static int nsp_grarc_neq(NspGrArc *A, NspObject *B);
static NspGrArc *nsp_grarc_xdr_load(XDR *xdrs);
static AttrTab grarc_attrs[];
static NspMethods *grarc_get_methods(void);
/* static int int_grarc_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGrArc *nsp_grarc_create_void(char *name,NspTypeBase *type);
#endif /* GrArc_Private */

