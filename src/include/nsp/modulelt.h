#ifndef NSP_INC_ModuleElt
#define NSP_INC_ModuleElt

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* ModuleElt */

#include "nsp/object.h"

/*
 * NspModuleElt inherits from NspObject
 */

typedef struct _NspModulelt NspModuleElt;

typedef struct _NspTypeModuleElt { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeModuleElt;

struct _NspModulelt {
  /*< private >*/
  NspObject father; 
  NspTypeModuleElt *type; 
  /*< public >*/
  char *path ;
  char *module;
};

extern int nsp_type_modulelt_id;
extern NspTypeModuleElt *nsp_type_modulelt;

/* type instances for classa */

NspTypeModuleElt *new_type_modulelt(type_mode mode);

/* instance for ModuleElt */

NspModuleElt *new_modulelt();

/*
 * Object methods redefined for modulelt 
 */

#define NULLME (NspModuleElt*) 0

NspModuleElt *modulelt_create(char *name,NspTypeBase *type);
NspModuleElt *modulelt_copy(NspModuleElt *H);
void modulelt_destroy(NspModuleElt *H);
void modulelt_info(NspModuleElt *H, int indent,char *name, int rec_level);
void modulelt_print(NspModuleElt *H, int indent,char *name, int rec_level);

/* from ModuleEltObj.c */

extern NspModuleElt *modulelt_object (NspObject *O); 
extern int IsModuleEltObj (Stack stack, int i); 
extern int IsModuleElt(NspObject *O);
extern NspModuleElt *GetModuleEltCopy (Stack stack, int i); 
extern NspModuleElt *GetModuleElt (Stack stack, int i); 

#endif 

/* private part */

#ifdef ModuleElt_Private 
static int init_modulelt(NspModuleElt *o,NspTypeModuleElt *type);
static int modulelt_size(NspModuleElt *Mat, int flag);
static char *modulelt_type_as_string(void);
static char *modulelt_type_short_string(NspObject *v);
static int modulelt_eq(NspModuleElt *A, NspObject *B);
static int modulelt_neq(NspModuleElt *A, NspObject *B);
static int modulelt_xdr_save(XDR  *F, NspModuleElt *M);
static NspModuleElt  *modulelt_xdr_load(XDR  *F);
static AttrTab modulelt_attrs[];
static NspMethods *modulelt_get_methods(void); 
static int int_me_create(Stack stack, int rhs, int opt, int lhs);
#endif /* ModuleElt_Private */
