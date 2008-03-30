/* -*- Mode: C -*- */
#ifndef NSP_INC_ClassARef
#define NSP_INC_ClassARef

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* ClassARef */

#include "nsp/object.h"

/*
 * NspClassARef inherits from NspObject
 */

typedef struct _NspClassARef NspClassARef ;
typedef struct _NspTypeClassARef NspTypeClassARef ;


struct _NspTypeClassARef {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
  
};

typedef struct _nsp_classaref nsp_classaref;
struct _nsp_classaref {
  int cla_color;
  int cla_thickness;
  NspMatrix* cla_val;
  NspBMatrix* cla_bval;
  NspList* cla_lval;
  int ref_count;
};

struct _NspClassARef {
  /*< private >*/
  NspObject father;
  NspTypeClassARef*type;
  /*< public >*/
  nsp_classaref *obj;
};

extern int nsp_type_classaref_id;
extern NspTypeClassARef *nsp_type_classaref;

/* type instances for object */

NspTypeClassARef *new_type_classaref(type_mode mode);

/* instance for ClassARef */

NspClassARef *new_classaref();

/*
* Object methods redefined for classaref 
*/


#define NULLCLASSAREF (NspClassARef*) 0

extern NspClassARef *nsp_classaref_create(char *name,int cla_color,int cla_thickness,NspMatrix* cla_val,NspBMatrix* cla_bval,NspList* cla_lval,NspTypeBase *type);

/* from ClassARefObj.c */

extern NspClassARef *nsp_classaref_copy(NspClassARef *H);
extern void nsp_classaref_destroy(NspClassARef *H);
extern int nsp_classaref_info(NspClassARef *H, int indent,const char *name, int rec_level);
extern int nsp_classaref_print(NspClassARef *H, int indent,const char *name, int rec_level);
extern int nsp_classaref_latex(NspClassARef *H, int indent,const char *name, int rec_level);
extern NspClassARef *nsp_classaref_object (NspObject *O); 
extern int IsClassARefObj (Stack stack, int i); 
extern int IsClassARef(NspObject *O);
extern NspClassARef *GetClassARefCopy (Stack stack, int i); 
extern NspClassARef *GetClassARef (Stack stack, int i); 
extern int nsp_classaref_create_partial(NspClassARef *H);
extern void nsp_classaref_destroy_partial(NspClassARef *H);
extern NspClassARef * nsp_classaref_copy_partial(NspClassARef *H,NspClassARef *self);
extern NspClassARef * nsp_classaref_full_copy_partial(NspClassARef *H,NspClassARef *self);
extern NspClassARef * nsp_classaref_full_copy(NspClassARef *self);
extern int nsp_classaref_check_values(NspClassARef *H);
extern int int_classaref_create(Stack stack, int rhs, int opt, int lhs); 
extern NspClassARef *nsp_classaref_xdr_load_partial(XDR *xdrs, NspClassARef *M);
extern int nsp_classaref_xdr_save(XDR  *xdrs, NspClassARef *M);

#endif /* NSP_INC_ClassARef */ 

#ifdef ClassARef_Private 
static int init_classaref(NspClassARef *o,NspTypeClassARef *type);
static int nsp_classaref_size(NspClassARef *Mat, int flag);
static char *nsp_classaref_type_as_string(void);
static char *nsp_classaref_type_short_string(NspObject *v);
static int nsp_classaref_eq(NspClassARef *A, NspObject *B);
static int nsp_classaref_neq(NspClassARef *A, NspObject *B);
static NspClassARef *nsp_classaref_xdr_load(XDR *xdrs);
static AttrTab classaref_attrs[];
static NspMethods *classaref_get_methods(void);
/* static int int_classaref_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspClassARef *nsp_classaref_create_void(char *name,NspTypeBase *type);
#endif /* ClassARef_Private */

