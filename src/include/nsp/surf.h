/* -*- Mode: C -*- */
#ifndef NSP_INC_NspSurf
#define NSP_INC_NspSurf

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspSurf */

#include <nsp/graphic.h>

/*
 * NspSurf inherits from Graphic
 */

typedef struct _NspSurf NspSurf ;
typedef struct _NspTypeNspSurf NspTypeNspSurf ;

#line 22 "./surf.h"

struct _NspTypeNspSurf {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./surf.h"
};

typedef struct _nsp_surf nsp_surf;
struct _nsp_surf {
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* z;
  NspMatrix* colors;
  gboolean mesh;
  gboolean zcolor;
  int mesh_color;
  int face_color;
  int ref_count;
};

struct _NspSurf {
  /*< private >*/
  NspGraphic father;
  NspTypeNspSurf*type;
  /*< public >*/
  nsp_surf *obj;
};

extern int nsp_type_surf_id;
extern NspTypeNspSurf *nsp_type_surf;

/* type instances for graphic */

NspTypeNspSurf *new_type_surf(type_mode mode);

/* instance for NspSurf */

NspSurf *new_surf();

/*
* Object methods redefined for surf 
*/


#define NULLSURF (NspSurf*) 0

extern NspSurf *nsp_surf_create(char *name,NspMatrix* x,NspMatrix* y,NspMatrix* z,NspMatrix* colors,gboolean mesh,gboolean zcolor,int mesh_color,int face_color,NspTypeBase *type);
extern NspSurf *nsp_surf_create_default(char *name);

/* from NspSurfObj.c */

extern NspSurf *nsp_surf_copy(NspSurf *H);
extern void nsp_surf_destroy(NspSurf *H);
extern int nsp_surf_info(NspSurf *H, int indent,const char *name, int rec_level);
extern int nsp_surf_print(NspSurf *H, int indent,const char *name, int rec_level);
extern int nsp_surf_latex(NspSurf *H, int indent,const char *name, int rec_level);
extern NspSurf *nsp_surf_object (NspObject *O); 
extern int IsSurfObj (Stack stack, int i); 
extern int IsSurf(NspObject *O);
extern NspSurf *GetSurfCopy (Stack stack, int i); 
extern NspSurf *GetSurf (Stack stack, int i); 
extern int nsp_surf_create_partial(NspSurf *H);
extern void nsp_surf_destroy_partial(NspSurf *H);
extern NspSurf * nsp_surf_copy_partial(NspSurf *H,NspSurf *self);
extern NspSurf * nsp_surf_full_copy_partial(NspSurf *H,NspSurf *self);
extern NspSurf * nsp_surf_full_copy(NspSurf *self);
extern int nsp_surf_check_values(NspSurf *H);
extern int int_surf_create(Stack stack, int rhs, int opt, int lhs); 
extern NspSurf *nsp_surf_xdr_load_partial(XDR *xdrs, NspSurf *M);
extern int nsp_surf_xdr_save(XDR  *xdrs, NspSurf *M);

#endif /* NSP_INC_NspSurf */ 

#ifdef NspSurf_Private 
static int init_surf(NspSurf *o,NspTypeNspSurf *type);
static int nsp_surf_size(NspSurf *Mat, int flag);
static char *nsp_surf_type_as_string(void);
static char *nsp_surf_type_short_string(NspObject *v);
static int nsp_surf_eq(NspSurf *A, NspObject *B);
static int nsp_surf_neq(NspSurf *A, NspObject *B);
static NspSurf *nsp_surf_xdr_load(XDR *xdrs);
static AttrTab surf_attrs[];
static NspMethods *surf_get_methods(void);
/* static int int_surf_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspSurf *nsp_surf_create_void(char *name,NspTypeBase *type);
#endif /* NspSurf_Private */

