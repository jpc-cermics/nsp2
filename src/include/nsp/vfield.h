/* -*- Mode: C -*- */
#ifndef NSP_INC_NspVField
#define NSP_INC_NspVField

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspVField */

#include <nsp/graphic.h>

/*
 * NspVField inherits from Graphic
 */

typedef struct _NspVField NspVField ;
typedef struct _NspTypeNspVField NspTypeNspVField ;

#line 22 "./vfield.h"

struct _NspTypeNspVField {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./vfield.h"
};

typedef struct _nsp_vfield nsp_vfield;
struct _nsp_vfield {
  NspMatrix* fx;
  NspMatrix* fy;
  NspMatrix* x;
  NspMatrix* y;
  gboolean colored;
  int ref_count;
};

struct _NspVField {
  /*< private >*/
  NspGraphic father;
  NspTypeNspVField*type;
  /*< public >*/
  nsp_vfield *obj;
};

extern int nsp_type_vfield_id;
extern NspTypeNspVField *nsp_type_vfield;

/* type instances for graphic */

NspTypeNspVField *new_type_vfield(type_mode mode);

/* instance for NspVField */

NspVField *new_vfield();

/*
* Object methods redefined for vfield 
*/


#define NULLVFIELD (NspVField*) 0

extern NspVField *nsp_vfield_create(char *name,NspMatrix* fx,NspMatrix* fy,NspMatrix* x,NspMatrix* y,gboolean colored,NspTypeBase *type);
extern NspVField *nsp_vfield_create_default(char *name);

/* from NspVFieldObj.c */

extern NspVField *nsp_vfield_copy(NspVField *H);
extern void nsp_vfield_destroy(NspVField *H);
extern int nsp_vfield_info(NspVField *H, int indent,const char *name, int rec_level);
extern int nsp_vfield_print(NspVField *H, int indent,const char *name, int rec_level);
extern int nsp_vfield_latex(NspVField *H, int indent,const char *name, int rec_level);
extern NspVField *nsp_vfield_object (NspObject *O); 
extern int IsVFieldObj (Stack stack, int i); 
extern int IsVField(NspObject *O);
extern NspVField *GetVFieldCopy (Stack stack, int i); 
extern NspVField *GetVField (Stack stack, int i); 
extern int nsp_vfield_create_partial(NspVField *H);
extern void nsp_vfield_destroy_partial(NspVField *H);
extern NspVField * nsp_vfield_copy_partial(NspVField *H,NspVField *self);
extern NspVField * nsp_vfield_full_copy_partial(NspVField *H,NspVField *self);
extern NspVField * nsp_vfield_full_copy(NspVField *self);
extern int nsp_vfield_check_values(NspVField *H);
extern int int_vfield_create(Stack stack, int rhs, int opt, int lhs); 
extern NspVField *nsp_vfield_xdr_load_partial(XDR *xdrs, NspVField *M);
extern int nsp_vfield_xdr_save(XDR  *xdrs, NspVField *M);

#endif /* NSP_INC_NspVField */ 

#ifdef NspVField_Private 
static int init_vfield(NspVField *o,NspTypeNspVField *type);
static int nsp_vfield_size(NspVField *Mat, int flag);
static char *nsp_vfield_type_as_string(void);
static char *nsp_vfield_type_short_string(NspObject *v);
static int nsp_vfield_eq(NspVField *A, NspObject *B);
static int nsp_vfield_neq(NspVField *A, NspObject *B);
static NspVField *nsp_vfield_xdr_load(XDR *xdrs);
static AttrTab vfield_attrs[];
static NspMethods *vfield_get_methods(void);
/* static int int_vfield_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspVField *nsp_vfield_create_void(char *name,NspTypeBase *type);
#endif /* NspVField_Private */

