/* -*- Mode: C -*- */
#ifndef NSP_INC_Polyline3d
#define NSP_INC_Polyline3d

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Polyline3d */

#include "nsp/graphic.h"

/*
 * NspPolyline3d inherits from NspGraphic
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

/* instance for Polyline3d */

NspPolyline3d *new_polyline3d();

/*
* Object methods redefined for polyline3d 
*/


#define NULLPOLYLINE3D (NspPolyline3d*) 0

extern NspPolyline3d *nsp_polyline3d_create(char *name,NspMatrix* Mcoord,void* Mcoord_l,NspMatrix* Mcolor,int* pos, int pos_length,NspTypeBase *type);

/* from Polyline3dObj.c */

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

#endif /* NSP_INC_Polyline3d */ 

#ifdef Polyline3d_Private 
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
#endif /* Polyline3d_Private */

