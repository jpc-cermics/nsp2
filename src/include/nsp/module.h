#ifndef NSP_INC_Module
#define NSP_INC_Module

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* Module */

#include "nsp/object.h"

/*
 * NspModule inherits from NspObject
 */

typedef struct _NspModule NspModule;

typedef struct _NspTypeModule { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeModule;

struct _NspModule {
  /*< private >*/
  NspObject father; 
  NspTypeModule *type; 
  /*< public >*/
  char *path ; /* absolute XXX path of the module */
  char *mname;  /* module name */
  NspHash *T;      /* hash table to store module objects of type me (module elts) */
  NspList *L;       /* List of submodules */
  int flag;     /* are we a copy ? */
};

extern int nsp_type_module_id;
extern NspTypeModule *nsp_type_module;

/* type instances for classa */

NspTypeModule *new_type_module(type_mode mode);

/* instance for Module */

NspModule *new_module();

/*
 * Object methods redefined for module 
 */

#define NULLMODULE (NspModule*) 0

NspModule *module_create(char *name,const char *path,const char *mname,NspTypeBase *type);
NspModule *module_copy(NspModule *H);
void module_destroy(NspModule *H);
void module_info(NspModule *H, int indent,char *name, int rec_level);
void module_print(NspModule *H, int indent,char *name, int rec_level);

/* from ModuleObj.c */

extern NspModule *module_object (NspObject *O); 
extern int IsModuleObj (Stack stack, int i); 
extern int IsModule(NspObject *O);
extern NspModule *GetModuleCopy (Stack stack, int i); 
extern NspModule *GetModule (Stack stack, int i); 

extern NspModule *module_copy_ref(NspModule *Mod);

extern NspObject * nsp_module_search_name(NspList *L,char **Mname);
extern int nsp_insert_module_last(NspList *L,char *dir,char **Mname);
extern int nsp_module_import(NspList *L,char *dir,char **Mname);

#endif 

/* private part */

#ifdef Module_Private 
static int init_module(NspModule *o,NspTypeModule *type);
static int module_size(NspModule *Mat, int flag);
static char *module_type_as_string(void);
static char *module_type_short_string(NspObject *v);
static int module_eq(NspModule *A, NspObject *B);
static int module_neq(NspModule *A, NspObject *B);
static int module_xdr_save(XDR  *F, NspModule *M);
static NspModule  *module_xdr_load(XDR  *F);
static AttrTab module_attrs[];
static NspMethods *module_get_methods(void); 
static int int_mo_create(Stack stack, int rhs, int opt, int lhs);
#endif /* Module_Private */
