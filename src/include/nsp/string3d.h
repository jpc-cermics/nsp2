/* -*- Mode: C -*- */
#ifndef NSP_INC_String3d
#define NSP_INC_String3d

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* String3d */

#include "nsp/graphic.h"

/*
 * NspString3d inherits from NspGraphic
 */

typedef struct _NspString3d NspString3d ;
typedef struct _NspTypeString3d NspTypeString3d ;

#line 22 "./string3d.h"

struct _NspTypeString3d {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./string3d.h"
};

typedef struct _nsp_string3d nsp_string3d;
struct _nsp_string3d {
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* z;
  gboolean mesh;
  int mesh_color;
  int face_color;
  int ref_count;
};

struct _NspString3d {
  /*< private >*/
  NspGraphic father;
  NspTypeString3d*type;
  /*< public >*/
  nsp_string3d *obj;
};

extern int nsp_type_string3d_id;
extern NspTypeString3d *nsp_type_string3d;

/* type instances for graphic */

NspTypeString3d *new_type_string3d(type_mode mode);

/* instance for String3d */

NspString3d *new_string3d();

/*
* Object methods redefined for string3d 
*/


#define NULLSTRING3D (NspString3d*) 0

extern NspString3d *nsp_string3d_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* z,gboolean mesh,int mesh_color,int face_color,NspTypeBase *type);

/* from String3dObj.c */

extern NspString3d *nsp_string3d_copy(NspString3d *H);
extern void nsp_string3d_destroy(NspString3d *H);
extern int nsp_string3d_info(NspString3d *H, int indent,const char *name, int rec_level);
extern int nsp_string3d_print(NspString3d *H, int indent,const char *name, int rec_level);
extern int nsp_string3d_latex(NspString3d *H, int indent,const char *name, int rec_level);
extern NspString3d *nsp_string3d_object (NspObject *O); 
extern int IsString3dObj (Stack stack, int i); 
extern int IsString3d(NspObject *O);
extern NspString3d *GetString3dCopy (Stack stack, int i); 
extern NspString3d *GetString3d (Stack stack, int i); 
extern int nsp_string3d_create_partial(NspString3d *H);
extern void nsp_string3d_destroy_partial(NspString3d *H);
extern NspString3d * nsp_string3d_copy_partial(NspString3d *H,NspString3d *self);
extern NspString3d * nsp_string3d_full_copy_partial(NspString3d *H,NspString3d *self);
extern NspString3d * nsp_string3d_full_copy(NspString3d *self);
extern int nsp_string3d_check_values(NspString3d *H);
extern int int_string3d_create(Stack stack, int rhs, int opt, int lhs); 
extern NspString3d *nsp_string3d_xdr_load_partial(XDR *xdrs, NspString3d *M);
extern int nsp_string3d_xdr_save(XDR  *xdrs, NspString3d *M);

#endif /* NSP_INC_String3d */ 

#ifdef String3d_Private 
static int init_string3d(NspString3d *o,NspTypeString3d *type);
static int nsp_string3d_size(NspString3d *Mat, int flag);
static char *nsp_string3d_type_as_string(void);
static char *nsp_string3d_type_short_string(NspObject *v);
static int nsp_string3d_eq(NspString3d *A, NspObject *B);
static int nsp_string3d_neq(NspString3d *A, NspObject *B);
static NspString3d *nsp_string3d_xdr_load(XDR *xdrs);
static AttrTab string3d_attrs[];
static NspMethods *string3d_get_methods(void);
/* static int int_string3d_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspString3d *nsp_string3d_create_void(char *name,NspTypeBase *type);
#endif /* String3d_Private */

