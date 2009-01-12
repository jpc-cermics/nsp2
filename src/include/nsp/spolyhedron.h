/* -*- Mode: C -*- */
#ifndef NSP_INC_SPolyhedron
#define NSP_INC_SPolyhedron

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* SPolyhedron */

#include "nsp/graphic.h"

/*
 * NspSPolyhedron inherits from NspGraphic
 */

typedef struct _NspSPolyhedron NspSPolyhedron ;
typedef struct _NspTypeSPolyhedron NspTypeSPolyhedron ;

#line 22 "./spolyhedron.h"

struct _NspTypeSPolyhedron {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./spolyhedron.h"
};

typedef struct _nsp_spolyhedron nsp_spolyhedron;
struct _nsp_spolyhedron {
  NspMatrix* Mcoord;
  NspMatrix* Mface;
  NspMatrix* Mval;
  double vmin;
  double vmax;
  int colmin;
  int colmax;
  int coloutmin;
  int coloutmax;
  gboolean mesh;
  int back_color;
  void* Mcoord_l;
  int* pos;  int pos_length;
  int* fill;  int fill_length;
  double* vlevel;  int vlevel_length;
  int coldef;
  int ref_count;
};

struct _NspSPolyhedron {
  /*< private >*/
  NspGraphic father;
  NspTypeSPolyhedron*type;
  /*< public >*/
  nsp_spolyhedron *obj;
};

extern int nsp_type_spolyhedron_id;
extern NspTypeSPolyhedron *nsp_type_spolyhedron;

/* type instances for graphic */

NspTypeSPolyhedron *new_type_spolyhedron(type_mode mode);

/* instance for SPolyhedron */

NspSPolyhedron *new_spolyhedron();

/*
* Object methods redefined for spolyhedron 
*/


#define NULLSPOLYHEDRON (NspSPolyhedron*) 0

extern NspSPolyhedron *nsp_spolyhedron_create(char *name,NspMatrix* Mcoord,NspMatrix* Mface,NspMatrix* Mval,double vmin,double vmax,int colmin,int colmax,int coloutmin,int coloutmax,gboolean mesh,int back_color,void* Mcoord_l,int* pos, int pos_length,int* fill, int fill_length,double* vlevel, int vlevel_length,int coldef,NspTypeBase *type);

/* from SPolyhedronObj.c */

extern NspSPolyhedron *nsp_spolyhedron_copy(NspSPolyhedron *H);
extern void nsp_spolyhedron_destroy(NspSPolyhedron *H);
extern int nsp_spolyhedron_info(NspSPolyhedron *H, int indent,const char *name, int rec_level);
extern int nsp_spolyhedron_print(NspSPolyhedron *H, int indent,const char *name, int rec_level);
extern int nsp_spolyhedron_latex(NspSPolyhedron *H, int indent,const char *name, int rec_level);
extern NspSPolyhedron *nsp_spolyhedron_object (NspObject *O); 
extern int IsSPolyhedronObj (Stack stack, int i); 
extern int IsSPolyhedron(NspObject *O);
extern NspSPolyhedron *GetSPolyhedronCopy (Stack stack, int i); 
extern NspSPolyhedron *GetSPolyhedron (Stack stack, int i); 
extern int nsp_spolyhedron_create_partial(NspSPolyhedron *H);
extern void nsp_spolyhedron_destroy_partial(NspSPolyhedron *H);
extern NspSPolyhedron * nsp_spolyhedron_copy_partial(NspSPolyhedron *H,NspSPolyhedron *self);
extern NspSPolyhedron * nsp_spolyhedron_full_copy_partial(NspSPolyhedron *H,NspSPolyhedron *self);
extern NspSPolyhedron * nsp_spolyhedron_full_copy(NspSPolyhedron *self);
extern int nsp_spolyhedron_check_values(NspSPolyhedron *H);
extern int int_spolyhedron_create(Stack stack, int rhs, int opt, int lhs); 
extern NspSPolyhedron *nsp_spolyhedron_xdr_load_partial(XDR *xdrs, NspSPolyhedron *M);
extern int nsp_spolyhedron_xdr_save(XDR  *xdrs, NspSPolyhedron *M);

#endif /* NSP_INC_SPolyhedron */ 

#ifdef SPolyhedron_Private 
static int init_spolyhedron(NspSPolyhedron *o,NspTypeSPolyhedron *type);
static int nsp_spolyhedron_size(NspSPolyhedron *Mat, int flag);
static char *nsp_spolyhedron_type_as_string(void);
static char *nsp_spolyhedron_type_short_string(NspObject *v);
static int nsp_spolyhedron_eq(NspSPolyhedron *A, NspObject *B);
static int nsp_spolyhedron_neq(NspSPolyhedron *A, NspObject *B);
static NspSPolyhedron *nsp_spolyhedron_xdr_load(XDR *xdrs);
static AttrTab spolyhedron_attrs[];
static NspMethods *spolyhedron_get_methods(void);
/* static int int_spolyhedron_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspSPolyhedron *nsp_spolyhedron_create_void(char *name,NspTypeBase *type);
#endif /* SPolyhedron_Private */

