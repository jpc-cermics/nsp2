/* -*- Mode: C -*- */
#ifndef NSP_INC_Objs3d
#define NSP_INC_Objs3d

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Objs3d */

#include "nsp/graphic.h"

/*
 * NspObjs3d inherits from NspGraphic
 */

typedef struct _NspObjs3d NspObjs3d ;
typedef struct _NspTypeObjs3d NspTypeObjs3d ;

#line 22 "./objs3d.h"

struct _NspTypeObjs3d {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./objs3d.h"
};

typedef struct _nsp_objs3d nsp_objs3d;
struct _nsp_objs3d {
  NspMatrix* wrect;
  double alpha;
  gboolean top;
  NspMatrix* bounds;
  NspMatrix* arect;
  NspMatrix* frect;
  char* title;
  char* x;
  char* y;
  NspMatrix* colormap;
  NspList* children;
  int ref_count;
};

struct _NspObjs3d {
  /*< private >*/
  NspGraphic father;
  NspTypeObjs3d*type;
  /*< public >*/
  nsp_objs3d *obj;
};

extern int nsp_type_objs3d_id;
extern NspTypeObjs3d *nsp_type_objs3d;

/* type instances for graphic */

NspTypeObjs3d *new_type_objs3d(type_mode mode);

/* instance for Objs3d */

NspObjs3d *new_objs3d();

/*
* Object methods redefined for objs3d 
*/


#define NULLOBJS3D (NspObjs3d*) 0

extern NspObjs3d *nsp_objs3d_create(char *name,NspMatrix* wrect,double alpha,gboolean top,NspMatrix* bounds,NspMatrix* arect,NspMatrix* frect,char* title,char* x,char* y,NspMatrix* colormap,NspList* children,NspTypeBase *type);

/* from Objs3dObj.c */

extern NspObjs3d *nsp_objs3d_copy(NspObjs3d *H);
extern void nsp_objs3d_destroy(NspObjs3d *H);
extern int nsp_objs3d_info(NspObjs3d *H, int indent,const char *name, int rec_level);
extern int nsp_objs3d_print(NspObjs3d *H, int indent,const char *name, int rec_level);
extern int nsp_objs3d_latex(NspObjs3d *H, int indent,const char *name, int rec_level);
extern NspObjs3d *nsp_objs3d_object (NspObject *O); 
extern int IsObjs3dObj (Stack stack, int i); 
extern int IsObjs3d(NspObject *O);
extern NspObjs3d *GetObjs3dCopy (Stack stack, int i); 
extern NspObjs3d *GetObjs3d (Stack stack, int i); 
extern int nsp_objs3d_create_partial(NspObjs3d *H);
extern void nsp_objs3d_destroy_partial(NspObjs3d *H);
extern NspObjs3d * nsp_objs3d_copy_partial(NspObjs3d *H,NspObjs3d *self);
extern NspObjs3d * nsp_objs3d_full_copy_partial(NspObjs3d *H,NspObjs3d *self);
extern NspObjs3d * nsp_objs3d_full_copy(NspObjs3d *self);
extern int nsp_objs3d_check_values(NspObjs3d *H);
extern int int_objs3d_create(Stack stack, int rhs, int opt, int lhs); 
extern NspObjs3d *nsp_objs3d_xdr_load_partial(XDR *xdrs, NspObjs3d *M);
extern int nsp_objs3d_xdr_save(XDR  *xdrs, NspObjs3d *M);

#endif /* NSP_INC_Objs3d */ 

#ifdef Objs3d_Private 
static int init_objs3d(NspObjs3d *o,NspTypeObjs3d *type);
static int nsp_objs3d_size(NspObjs3d *Mat, int flag);
static char *nsp_objs3d_type_as_string(void);
static char *nsp_objs3d_type_short_string(NspObject *v);
static int nsp_objs3d_eq(NspObjs3d *A, NspObject *B);
static int nsp_objs3d_neq(NspObjs3d *A, NspObject *B);
static NspObjs3d *nsp_objs3d_xdr_load(XDR *xdrs);
static AttrTab objs3d_attrs[];
static NspMethods *objs3d_get_methods(void);
/* static int int_objs3d_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspObjs3d *nsp_objs3d_create_void(char *name,NspTypeBase *type);
#endif /* Objs3d_Private */

