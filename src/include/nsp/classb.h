/* -*- Mode: C -*- */
#ifndef INC_NSP_ClassB
#define INC_NSP_ClassB

/*-----------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )
 * Jean-Philippe Chancelier Enpc/Cermics
 *-----------------------------------------------------------------*/
  
/* ClassB */

#include "nsp/classa.h"

/*
 * NspClassB inherits from NspClassA
 */

typedef struct _nsp_classb NspClassB;

typedef int (*classb_save) (NspFile  *F, NspClassB *M);

typedef struct _nsp_type_ClassB { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
} NspTypeClassB;

struct _nsp_classb {
  NspClassA father; 
  NspTypeClassB *type; 
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

NspClassB *classb_create(char *name,int color,int thickness,NspTypeBase *type);
NspClassB *classb_copy(NspClassB *H);
void classb_destroy(NspClassB *H);
void classb_info(NspClassB *H, int indent);
void classb_print(NspClassB *H, int indent);

/* from ClassBObj.c */

extern NspClassB *classb_object (NspObject *O); 
extern int IsClassBObj (Stack stack, int i); 
extern int IsClassB(NspObject *O);
extern NspClassB *GetClassBCopy (Stack stack, int i); 
extern NspClassB *GetClassB (Stack stack, int i); 

#endif 

#ifdef ClassB_Private 
static int init_classb(NspClassB *o,NspTypeClassB *type);
static int classb_size(NspClassB *Mat, int flag);
static char *classb_type_as_string(void);
static char *classb_type_short_string(void);
static int classb_eq(NspClassB *A, NspObject *B);
static int classb_neq(NspClassB *A, NspObject *B);
static int classb_xdr_save(NspFile  *F, NspClassB *M);
static NspClassB  *classb_xdr_load(NspFile   *F);
static AttrTab classb_attrs[]; 
static NspMethods *classb_get_methods(void); 
static NspObject *classb_path_extract(NspClassB *A, NspObject *O);
static int int_clb_create(Stack stack, int rhs, int opt, int lhs);
#endif /* ClassB_Private */

