/* -*- Mode: C -*- */
#ifndef NSP_INC_ClassB
#define NSP_INC_ClassB

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include "nsp/classa.h"

/**
 * NspClassB:
 * @classb_color: an integer 
 * @classb_thickness: an integer 
 * @classb_val: a #NspMatrix 
 *
 * inherits from NspClassA used as a basic demo 
 * of a class implementation 
 */

typedef struct _NspClassB NspClassB;
typedef struct _NspTypeClassB NspTypeClassB;

typedef int (*classb_save) (XDR  *xdrs, NspClassB *M);

struct _NspTypeClassB { 
  /*< private >*/
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

struct _NspClassB {
  /*< private >*/
  NspClassA father; 
  NspTypeClassB *type; 
  /*< public >*/
  int classb_color;
  int classb_thickness;
  NspMatrix *classb_val;
};

extern int nsp_type_classb_id;
extern NspTypeClassB *nsp_type_classb;

/* type instances for classa */

NspTypeClassB *new_type_classb(type_mode mode);

/* instance for ClassB */

NspClassB *new_classb();

/*
 * Object methods redefined for classb 
 */

#define NULLCLB (NspClassB*) 0

NspClassB *nsp_classb_create(char *name,int color,int thickness,NspTypeBase *type);
NspClassB *nsp_classb_copy(NspClassB *H);
void nsp_classb_destroy(NspClassB *H);
void nsp_classb_info(NspClassB *H, int indent,const char *name, int rec_level);
void nsp_classb_print(NspClassB *H, int indent,const char *name, int rec_level);

/* from ClassBObj.c */

extern NspClassB *nsp_classb_object (NspObject *O); 
extern int IsClassBObj (Stack stack, int i); 
extern int IsClassB(NspObject *O);
extern NspClassB *GetClassBCopy (Stack stack, int i); 
extern NspClassB *GetClassB (Stack stack, int i); 

#endif 

/* private part */

#ifdef ClassB_Private 
static int init_classb(NspClassB *o,NspTypeClassB *type);
static int nsp_classb_size(NspClassB *Mat, int flag);
static char *nsp_classb_type_as_string(void);
static char *nsp_classb_type_short_string(NspObject *v);
static int nsp_classb_eq(NspClassB *A, NspObject *B);
static int nsp_classb_neq(NspClassB *A, NspObject *B);
static int nsp_classb_xdr_save(XDR *xdrs, NspClassB *M);
static NspClassB  *nsp_classb_xdr_load(XDR *xdrs);
static AttrTab classb_attrs[];
static NspMethods *classb_get_methods(void); 
static int int_clb_create(Stack stack, int rhs, int opt, int lhs);
#endif /* ClassB_Private */
