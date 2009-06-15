/* -*- Mode: C -*- */
#ifndef NSP_INC_NspPoints3d
#define NSP_INC_NspPoints3d

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspPoints3d */

#include "nsp/graphic.h"

/*
 * NspPoints3d inherits from Graphic
 */

typedef struct _NspPoints3d NspPoints3d ;
typedef struct _NspTypeNspPoints3d NspTypeNspPoints3d ;

#line 22 "./points3d.h"

struct _NspTypeNspPoints3d {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./points3d.h"
};

typedef struct _nsp_points3d nsp_points3d;
struct _nsp_points3d {
  NspMatrix* Mcoord;
  void* Mcoord_l;
  int color;
  int mark_type;
  int mark_size;
  int* pos;  int pos_length;
  int ref_count;
};

struct _NspPoints3d {
  /*< private >*/
  NspGraphic father;
  NspTypeNspPoints3d*type;
  /*< public >*/
  nsp_points3d *obj;
};

extern int nsp_type_points3d_id;
extern NspTypeNspPoints3d *nsp_type_points3d;

/* type instances for graphic */

NspTypeNspPoints3d *new_type_points3d(type_mode mode);

/* instance for NspPoints3d */

NspPoints3d *new_points3d();

/*
* Object methods redefined for points3d 
*/


#define NULLPOINTS3D (NspPoints3d*) 0

extern NspPoints3d *nsp_points3d_create(char *name,NspMatrix* Mcoord,void* Mcoord_l,int color,int mark_type,int mark_size,int* pos, int pos_length,NspTypeBase *type);
extern NspPoints3d *nsp_points3d_create_default(char *name);

/* from NspPoints3dObj.c */

extern NspPoints3d *nsp_points3d_copy(NspPoints3d *H);
extern void nsp_points3d_destroy(NspPoints3d *H);
extern int nsp_points3d_info(NspPoints3d *H, int indent,const char *name, int rec_level);
extern int nsp_points3d_print(NspPoints3d *H, int indent,const char *name, int rec_level);
extern int nsp_points3d_latex(NspPoints3d *H, int indent,const char *name, int rec_level);
extern NspPoints3d *nsp_points3d_object (NspObject *O); 
extern int IsPoints3dObj (Stack stack, int i); 
extern int IsPoints3d(NspObject *O);
extern NspPoints3d *GetPoints3dCopy (Stack stack, int i); 
extern NspPoints3d *GetPoints3d (Stack stack, int i); 
extern int nsp_points3d_create_partial(NspPoints3d *H);
extern void nsp_points3d_destroy_partial(NspPoints3d *H);
extern NspPoints3d * nsp_points3d_copy_partial(NspPoints3d *H,NspPoints3d *self);
extern NspPoints3d * nsp_points3d_full_copy_partial(NspPoints3d *H,NspPoints3d *self);
extern NspPoints3d * nsp_points3d_full_copy(NspPoints3d *self);
extern int nsp_points3d_check_values(NspPoints3d *H);
extern int int_points3d_create(Stack stack, int rhs, int opt, int lhs); 
extern NspPoints3d *nsp_points3d_xdr_load_partial(XDR *xdrs, NspPoints3d *M);
extern int nsp_points3d_xdr_save(XDR  *xdrs, NspPoints3d *M);

#endif /* NSP_INC_NspPoints3d */ 

#ifdef NspPoints3d_Private 
static int init_points3d(NspPoints3d *o,NspTypeNspPoints3d *type);
static int nsp_points3d_size(NspPoints3d *Mat, int flag);
static char *nsp_points3d_type_as_string(void);
static char *nsp_points3d_type_short_string(NspObject *v);
static int nsp_points3d_eq(NspPoints3d *A, NspObject *B);
static int nsp_points3d_neq(NspPoints3d *A, NspObject *B);
static NspPoints3d *nsp_points3d_xdr_load(XDR *xdrs);
static AttrTab points3d_attrs[];
static NspMethods *points3d_get_methods(void);
/* static int int_points3d_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspPoints3d *nsp_points3d_create_void(char *name,NspTypeBase *type);
#endif /* NspPoints3d_Private */

