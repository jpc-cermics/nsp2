/* -*- Mode: C -*- */
#ifndef NSP_INC_NspBox3d
#define NSP_INC_NspBox3d

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspBox3d */

#include <nsp/graphic.h>

/*
 * NspBox3d inherits from Graphic
 */

typedef struct _NspBox3d NspBox3d ;
typedef struct _NspTypeNspBox3d NspTypeNspBox3d ;

#line 22 "./box3d.h"

struct _NspTypeNspBox3d {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./box3d.h"
};

typedef struct _nsp_box3d nsp_box3d;
struct _nsp_box3d {
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* z;
  gboolean mesh;
  int mesh_color;
  int face_color;
  int ref_count;
};

struct _NspBox3d {
  /*< private >*/
  NspGraphic father;
  NspTypeNspBox3d*type;
  /*< public >*/
  nsp_box3d *obj;
};

extern int nsp_type_box3d_id;
extern NspTypeNspBox3d *nsp_type_box3d;

/* type instances for graphic */

NspTypeNspBox3d *new_type_box3d(type_mode mode);

/* instance for NspBox3d */

NspBox3d *new_box3d();

/*
* Object methods redefined for box3d 
*/


#define NULLBOX3D (NspBox3d*) 0

extern NspBox3d *nsp_box3d_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* z,gboolean mesh,int mesh_color,int face_color,NspTypeBase *type);
extern NspBox3d *nsp_box3d_create_default(char *name);

/* from NspBox3dObj.c */

extern NspBox3d *nsp_box3d_copy(NspBox3d *H);
extern void nsp_box3d_destroy(NspBox3d *H);
extern int nsp_box3d_info(NspBox3d *H, int indent,const char *name, int rec_level);
extern int nsp_box3d_print(NspBox3d *H, int indent,const char *name, int rec_level);
extern int nsp_box3d_latex(NspBox3d *H, int indent,const char *name, int rec_level);
extern NspBox3d *nsp_box3d_object (NspObject *O); 
extern int IsBox3dObj (Stack stack, int i); 
extern int IsBox3d(NspObject *O);
extern NspBox3d *GetBox3dCopy (Stack stack, int i); 
extern NspBox3d *GetBox3d (Stack stack, int i); 
extern int nsp_box3d_create_partial(NspBox3d *H);
extern void nsp_box3d_destroy_partial(NspBox3d *H);
extern NspBox3d * nsp_box3d_copy_partial(NspBox3d *H,NspBox3d *self);
extern NspBox3d * nsp_box3d_full_copy_partial(NspBox3d *H,NspBox3d *self);
extern NspBox3d * nsp_box3d_full_copy(NspBox3d *self);
extern int nsp_box3d_check_values(NspBox3d *H);
extern int int_box3d_create(Stack stack, int rhs, int opt, int lhs); 
extern NspBox3d *nsp_box3d_xdr_load_partial(XDR *xdrs, NspBox3d *M);
extern int nsp_box3d_xdr_save(XDR  *xdrs, NspBox3d *M);

#line 4 "codegen/box3d.override"

/* inserted at the end of public part of include file */

#line 98 "./box3d.h"
#endif /* NSP_INC_NspBox3d */ 

#ifdef NspBox3d_Private 
static int init_box3d(NspBox3d *o,NspTypeNspBox3d *type);
static int nsp_box3d_size(NspBox3d *Mat, int flag);
static char *nsp_box3d_type_as_string(void);
static char *nsp_box3d_type_short_string(NspObject *v);
static int nsp_box3d_eq(NspBox3d *A, NspObject *B);
static int nsp_box3d_neq(NspBox3d *A, NspObject *B);
static NspBox3d *nsp_box3d_xdr_load(XDR *xdrs);
static AttrTab box3d_attrs[];
static NspMethods *box3d_get_methods(void);
/* static int int_box3d_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspBox3d *nsp_box3d_create_void(char *name,NspTypeBase *type);
#line 9 "codegen/box3d.override"

/* inserted in the private part of include file */

static void nsp_draw_box3d(BCG *Xgc,NspGraphic *Obj, void *data);
static void nsp_translate_box3d(BCG *Xgc,NspGraphic *o,double *tr);
static void nsp_rotate_box3d(BCG *Xgc,NspGraphic *o,double *R);
static void nsp_scale_box3d(BCG *Xgc,NspGraphic *o,double *alpha);
static int nsp_getbounds_box3d(BCG *Xgc,NspGraphic *o,double *bounds);

#line 123 "./box3d.h"
#endif /* NspBox3d_Private */

