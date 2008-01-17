/* -*- Mode: C -*- */
#ifndef NSP_INC_ClassBRef
#define NSP_INC_ClassBRef

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* ClassBRef */

#include "nsp/classaref.h"

/*
 * NspClassBRef inherits from NspClassARef
 */

typedef struct _NspClassBRef NspClassBRef ;
typedef struct _NspTypeClassBRef NspTypeClassBRef ;

struct _NspTypeClassBRef {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_classbref nsp_classbref;
struct _nsp_classbref {
  int clb_color;
  int clb_thickness;
  NspMatrix* clb_val;
  int ref_count;
};

struct _NspClassBRef {
  /*< private >*/
  NspClassARef father;
  NspTypeClassBRef*type;
  /*< public >*/
  nsp_classbref *obj;
};

extern int nsp_type_classbref_id;
extern NspTypeClassBRef *nsp_type_classbref;

/* type instances for classaref */

NspTypeClassBRef *new_type_classbref(type_mode mode);

/* instance for ClassBRef */

NspClassBRef *new_classbref();

/*
* Object methods redefined for classbref 
*/


#define NULLCLASSBREF (NspClassBRef*) 0

extern NspClassBRef *nsp_classbref_create(char *name,int clb_color,int clb_thickness,NspMatrix* clb_val,NspTypeBase *type);

/* from ClassBRefObj.c */

extern NspClassBRef *nsp_classbref_copy(NspClassBRef *H);
extern void nsp_classbref_destroy(NspClassBRef *H);
extern void nsp_classbref_info(NspClassBRef *H, int indent,const char *name, int rec_level);
extern void nsp_classbref_print(NspClassBRef *H, int indent,const char *name, int rec_level);
extern void nsp_classbref_latex(NspClassBRef *H, int indent,const char *name, int rec_level);
extern NspClassBRef *nsp_classbref_object (NspObject *O); 
extern int IsClassBRefObj (Stack stack, int i); 
extern int IsClassBRef(NspObject *O);
extern NspClassBRef *GetClassBRefCopy (Stack stack, int i); 
extern NspClassBRef *GetClassBRef (Stack stack, int i); 
extern int nsp_classbref_create_partial(NspClassBRef *H);
extern void nsp_classbref_destroy_partial(NspClassBRef *H);
extern NspClassBRef * nsp_classbref_copy_partial(NspClassBRef *H,NspClassBRef *self);
extern int int_classbref_create(Stack stack, int rhs, int opt, int lhs); 
extern NspClassBRef *nsp_classbref_xdr_load_partial(XDR *xdrs, NspClassBRef *M);
extern int nsp_classbref_xdr_save(XDR  *xdrs, NspClassBRef *M);

#endif /* NSP_INC_ClassBRef */ 

#ifdef ClassBRef_Private 
static int init_classbref(NspClassBRef *o,NspTypeClassBRef *type);
static int nsp_classbref_size(NspClassBRef *Mat, int flag);
static char *nsp_classbref_type_as_string(void);
static char *nsp_classbref_type_short_string(NspObject *v);
static int nsp_classbref_eq(NspClassBRef *A, NspObject *B);
static int nsp_classbref_neq(NspClassBRef *A, NspObject *B);
static NspClassBRef *nsp_classbref_xdr_load(XDR *xdrs);
static AttrTab classbref_attrs[];
static NspMethods *classbref_get_methods(void);
/* static int int_classbref_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspClassBRef *nsp_classbref_create_void(char *name,NspTypeBase *type);
#endif /* ClassBRef_Private */

