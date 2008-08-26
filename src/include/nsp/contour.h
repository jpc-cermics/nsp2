/* -*- Mode: C -*- */
#ifndef NSP_INC_Contour
#define NSP_INC_Contour

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Contour */

#include "nsp/graphic.h"

/*
 * NspContour inherits from NspGraphic
 */

typedef struct _NspContour NspContour ;
typedef struct _NspTypeContour NspTypeContour ;

#line 22 "./contour.h"

struct _NspTypeContour {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./contour.h"
};

typedef struct _nsp_contour nsp_contour;
struct _nsp_contour {
  NspMatrix* z;
  NspMatrix* x;
  NspMatrix* y;
  NspMatrix* levels;
  int nlevels;
  int ref_count;
};

struct _NspContour {
  /*< private >*/
  NspGraphic father;
  NspTypeContour*type;
  /*< public >*/
  nsp_contour *obj;
};

extern int nsp_type_contour_id;
extern NspTypeContour *nsp_type_contour;

/* type instances for graphic */

NspTypeContour *new_type_contour(type_mode mode);

/* instance for Contour */

NspContour *new_contour();

/*
* Object methods redefined for contour 
*/


#define NULLCONTOUR (NspContour*) 0

extern NspContour *nsp_contour_create(char *name,NspMatrix* z,NspMatrix* x,NspMatrix* y,NspMatrix* levels,int nlevels,NspTypeBase *type);

/* from ContourObj.c */

extern NspContour *nsp_contour_copy(NspContour *H);
extern void nsp_contour_destroy(NspContour *H);
extern int nsp_contour_info(NspContour *H, int indent,const char *name, int rec_level);
extern int nsp_contour_print(NspContour *H, int indent,const char *name, int rec_level);
extern int nsp_contour_latex(NspContour *H, int indent,const char *name, int rec_level);
extern NspContour *nsp_contour_object (NspObject *O); 
extern int IsContourObj (Stack stack, int i); 
extern int IsContour(NspObject *O);
extern NspContour *GetContourCopy (Stack stack, int i); 
extern NspContour *GetContour (Stack stack, int i); 
extern int nsp_contour_create_partial(NspContour *H);
extern void nsp_contour_destroy_partial(NspContour *H);
extern NspContour * nsp_contour_copy_partial(NspContour *H,NspContour *self);
extern NspContour * nsp_contour_full_copy_partial(NspContour *H,NspContour *self);
extern NspContour * nsp_contour_full_copy(NspContour *self);
extern int nsp_contour_check_values(NspContour *H);
extern int int_contour_create(Stack stack, int rhs, int opt, int lhs); 
extern NspContour *nsp_contour_xdr_load_partial(XDR *xdrs, NspContour *M);
extern int nsp_contour_xdr_save(XDR  *xdrs, NspContour *M);

#endif /* NSP_INC_Contour */ 

#ifdef Contour_Private 
static int init_contour(NspContour *o,NspTypeContour *type);
static int nsp_contour_size(NspContour *Mat, int flag);
static char *nsp_contour_type_as_string(void);
static char *nsp_contour_type_short_string(NspObject *v);
static int nsp_contour_eq(NspContour *A, NspObject *B);
static int nsp_contour_neq(NspContour *A, NspObject *B);
static NspContour *nsp_contour_xdr_load(XDR *xdrs);
static AttrTab contour_attrs[];
static NspMethods *contour_get_methods(void);
/* static int int_contour_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspContour *nsp_contour_create_void(char *name,NspTypeBase *type);
#endif /* Contour_Private */

