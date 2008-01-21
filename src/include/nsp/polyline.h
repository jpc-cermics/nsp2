/* -*- Mode: C -*- */
#ifndef NSP_INC_Polyline
#define NSP_INC_Polyline

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Polyline */

#include "nsp/graphic.h"

/*
 * NspPolyline inherits from NspGraphic
 */

typedef struct _NspPolyline NspPolyline ;
typedef struct _NspTypePolyline NspTypePolyline ;

struct _NspTypePolyline {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_polyline nsp_polyline;
struct _nsp_polyline {
  int color;
  NspMatrix* Pts;
  int ref_count;
};

struct _NspPolyline {
  /*< private >*/
  NspGraphic father;
  NspTypePolyline*type;
  /*< public >*/
  nsp_polyline *obj;
};

extern int nsp_type_polyline_id;
extern NspTypePolyline *nsp_type_polyline;

/* type instances for graphic */

NspTypePolyline *new_type_polyline(type_mode mode);

/* instance for Polyline */

NspPolyline *new_polyline();

/*
* Object methods redefined for polyline 
*/


#define NULLPOLYLINE (NspPolyline*) 0

extern NspPolyline *nsp_polyline_create(char *name,int color,NspMatrix* Pts,NspTypeBase *type);

/* from PolylineObj.c */

extern NspPolyline *nsp_polyline_copy(NspPolyline *H);
extern void nsp_polyline_destroy(NspPolyline *H);
extern void nsp_polyline_info(NspPolyline *H, int indent,const char *name, int rec_level);
extern void nsp_polyline_print(NspPolyline *H, int indent,const char *name, int rec_level);
extern void nsp_polyline_latex(NspPolyline *H, int indent,const char *name, int rec_level);
extern NspPolyline *nsp_polyline_object (NspObject *O); 
extern int IsPolylineObj (Stack stack, int i); 
extern int IsPolyline(NspObject *O);
extern NspPolyline *GetPolylineCopy (Stack stack, int i); 
extern NspPolyline *GetPolyline (Stack stack, int i); 
extern int nsp_polyline_create_partial(NspPolyline *H);
extern void nsp_polyline_destroy_partial(NspPolyline *H);
extern NspPolyline * nsp_polyline_copy_partial(NspPolyline *H,NspPolyline *self);
extern int nsp_polyline_check_values(NspPolyline *H);
extern int int_polyline_create(Stack stack, int rhs, int opt, int lhs); 
extern NspPolyline *nsp_polyline_xdr_load_partial(XDR *xdrs, NspPolyline *M);
extern int nsp_polyline_xdr_save(XDR  *xdrs, NspPolyline *M);

#endif /* NSP_INC_Polyline */ 

#ifdef Polyline_Private 
static int init_polyline(NspPolyline *o,NspTypePolyline *type);
static int nsp_polyline_size(NspPolyline *Mat, int flag);
static char *nsp_polyline_type_as_string(void);
static char *nsp_polyline_type_short_string(NspObject *v);
static int nsp_polyline_eq(NspPolyline *A, NspObject *B);
static int nsp_polyline_neq(NspPolyline *A, NspObject *B);
static NspPolyline *nsp_polyline_xdr_load(XDR *xdrs);
static AttrTab polyline_attrs[];
static NspMethods *polyline_get_methods(void);
/* static int int_polyline_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspPolyline *nsp_polyline_create_void(char *name,NspTypeBase *type);
#endif /* Polyline_Private */

