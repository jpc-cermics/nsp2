/* -*- Mode: C -*- */
#ifndef NSP_INC_NspClassB
#define NSP_INC_NspClassB

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* NspClassB */

#include <nsp/classa.h>

/*
 * NspClassB inherits from ClassA
 */

typedef struct _NspClassB NspClassB ;
typedef struct _NspTypeClassB NspTypeClassB ;

#line 22 "./classb.h"

struct _NspTypeClassB {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
 
#line 29 "./classb.h"
};

struct _NspClassB {
  /*< private >*/
  NspClassA father;
  NspTypeClassB*type;
  /*< public >*/
    int clb_color;
  int clb_thickness;
  NspMatrix* clb_val;
};

extern int nsp_type_classb_id;
extern NspTypeClassB *nsp_type_classb;

/* type instances for classa */

NspTypeClassB *new_type_classb(type_mode mode);

/* instance for NspClassB */

NspClassB *new_classb();

/*
 * Object methods redefined for classb 
 */


#define NULLCLASSB (NspClassB*) 0

extern NspClassB *nsp_classb_create(char *name,int clb_color,int clb_thickness,NspMatrix* clb_val,NspTypeBase *type);
extern NspClassB *nsp_classb_create_default(char *name);

/* from NspClassBObj.c */

extern NspClassB *nsp_classb_copy(NspClassB *H);
extern void nsp_classb_destroy(NspClassB *H);
extern int nsp_classb_info(NspClassB *H, int indent,const char *name, int rec_level);
extern int nsp_classb_print(NspClassB *H, int indent,const char *name, int rec_level);
extern int nsp_classb_latex(NspClassB *H, int indent,const char *name, int rec_level);
extern NspClassB *nsp_classb_object (NspObject *O);
extern int IsClassBObj (Stack stack, int i);
extern int IsClassB(NspObject *O);
extern NspClassB *GetClassBCopy (Stack stack, int i);
extern NspClassB *GetClassB (Stack stack, int i);
extern int nsp_classb_create_partial(NspClassB *H);
extern void nsp_classb_destroy_partial(NspClassB *H);
extern NspClassB * nsp_classb_copy_partial(NspClassB *H,NspClassB *self);
extern NspClassB * nsp_classb_full_copy_partial(NspClassB *H,NspClassB *self);
extern NspClassB * nsp_classb_full_copy(NspClassB *self);
extern int nsp_classb_check_values(NspClassB *H);
extern int int_classb_create(Stack stack, int rhs, int opt, int lhs);
extern NspClassB *nsp_classb_xdr_load_partial(XDR *xdrs, NspClassB *M);
extern int nsp_classb_xdr_save(XDR  *xdrs, NspClassB *M);

#endif /* NSP_INC_NspClassB */ 

#ifdef NspClassB_Private 
static int init_classb(NspClassB *o,NspTypeClassB *type);
static int nsp_classb_size(NspClassB *Mat, int flag);
static char *nsp_classb_type_as_string(void);
static char *nsp_classb_type_short_string(NspObject *v);
static int nsp_classb_eq(NspClassB *A, NspObject *B);
static int nsp_classb_neq(NspClassB *A, NspObject *B);
static NspClassB *nsp_classb_xdr_load(XDR *xdrs);
static AttrTab classb_attrs[];
static NspMethods *classb_get_methods(void);
/* static int int_classb_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspClassB *nsp_classb_create_void(char *name,NspTypeBase *type);
#endif /* NspClassB_Private */

