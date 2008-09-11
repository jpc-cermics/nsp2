/* -*- Mode: C -*- */
#ifndef NSP_INC_Polyhedron
#define NSP_INC_Polyhedron

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Polyhedron */

#include "nsp/graphic.h"

/*
 * NspPolyhedron inherits from NspGraphic
 */

typedef struct _NspPolyhedron NspPolyhedron ;
typedef struct _NspTypePolyhedron NspTypePolyhedron ;

#line 22 "./polyhedron.h"

struct _NspTypePolyhedron {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./polyhedron.h"
};

typedef struct _nsp_polyhedron nsp_polyhedron;
struct _nsp_polyhedron {
  NspMatrix* Mcoord;
  void* Mcoord_l;
  NspMatrix* Mface;
  NspMatrix* Mcolor;
  NspMatrix* Mback_color;
  gboolean mesh;
  int* pos;  int pos_length;
  int ref_count;
};

struct _NspPolyhedron {
  /*< private >*/
  NspGraphic father;
  NspTypePolyhedron*type;
  /*< public >*/
  nsp_polyhedron *obj;
};

extern int nsp_type_polyhedron_id;
extern NspTypePolyhedron *nsp_type_polyhedron;

/* type instances for graphic */

NspTypePolyhedron *new_type_polyhedron(type_mode mode);

/* instance for Polyhedron */

NspPolyhedron *new_polyhedron();

/*
* Object methods redefined for polyhedron 
*/


#define NULLPOLYHEDRON (NspPolyhedron*) 0

extern NspPolyhedron *nsp_polyhedron_create(char *name,NspMatrix* Mcoord,void* Mcoord_l,NspMatrix* Mface,NspMatrix* Mcolor,NspMatrix* Mback_color,gboolean mesh,int* pos, int pos_length,NspTypeBase *type);

/* from PolyhedronObj.c */

extern NspPolyhedron *nsp_polyhedron_copy(NspPolyhedron *H);
extern void nsp_polyhedron_destroy(NspPolyhedron *H);
extern int nsp_polyhedron_info(NspPolyhedron *H, int indent,const char *name, int rec_level);
extern int nsp_polyhedron_print(NspPolyhedron *H, int indent,const char *name, int rec_level);
extern int nsp_polyhedron_latex(NspPolyhedron *H, int indent,const char *name, int rec_level);
extern NspPolyhedron *nsp_polyhedron_object (NspObject *O); 
extern int IsPolyhedronObj (Stack stack, int i); 
extern int IsPolyhedron(NspObject *O);
extern NspPolyhedron *GetPolyhedronCopy (Stack stack, int i); 
extern NspPolyhedron *GetPolyhedron (Stack stack, int i); 
extern int nsp_polyhedron_create_partial(NspPolyhedron *H);
extern void nsp_polyhedron_destroy_partial(NspPolyhedron *H);
extern NspPolyhedron * nsp_polyhedron_copy_partial(NspPolyhedron *H,NspPolyhedron *self);
extern NspPolyhedron * nsp_polyhedron_full_copy_partial(NspPolyhedron *H,NspPolyhedron *self);
extern NspPolyhedron * nsp_polyhedron_full_copy(NspPolyhedron *self);
extern int nsp_polyhedron_check_values(NspPolyhedron *H);
extern int int_polyhedron_create(Stack stack, int rhs, int opt, int lhs); 
extern NspPolyhedron *nsp_polyhedron_xdr_load_partial(XDR *xdrs, NspPolyhedron *M);
extern int nsp_polyhedron_xdr_save(XDR  *xdrs, NspPolyhedron *M);

#endif /* NSP_INC_Polyhedron */ 

#ifdef Polyhedron_Private 
static int init_polyhedron(NspPolyhedron *o,NspTypePolyhedron *type);
static int nsp_polyhedron_size(NspPolyhedron *Mat, int flag);
static char *nsp_polyhedron_type_as_string(void);
static char *nsp_polyhedron_type_short_string(NspObject *v);
static int nsp_polyhedron_eq(NspPolyhedron *A, NspObject *B);
static int nsp_polyhedron_neq(NspPolyhedron *A, NspObject *B);
static NspPolyhedron *nsp_polyhedron_xdr_load(XDR *xdrs);
static AttrTab polyhedron_attrs[];
static NspMethods *polyhedron_get_methods(void);
/* static int int_polyhedron_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspPolyhedron *nsp_polyhedron_create_void(char *name,NspTypeBase *type);
#endif /* Polyhedron_Private */

