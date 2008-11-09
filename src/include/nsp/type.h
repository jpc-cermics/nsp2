#ifndef NSP_INC_Type
#define NSP_INC_Type

/*
 * This Software is GPL(Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* Nsp Types as Nsp Objects  */

#include "nsp/object.h"

/*
 * NspType inherits from NspObject
 */

typedef struct _NspType NspType;

typedef struct _NspTypeType { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeType;

struct _NspType {
  /*< private >*/
  NspObject father; 
  NspTypeType *type; 
  /*< public >*/
  NspTypeBase *nsp_type;
};

extern int nsp_type_type_id;
extern NspTypeType *nsp_type_type;

/* type instances for classa */

NspTypeType *new_type_type(type_mode mode);

/* instance for Type */

NspType *new_type();

/*
 * Object methods redefined for type 
 */

#ifdef Type_Private 
static int init_type(NspType *o,NspTypeType *type);
static int type_size(NspType *Mat, int flag);
static char *type_type_as_string(void);
static char *type_type_short_string(NspObject *v);
static int type_eq(NspType *A, NspObject *B);
static int type_neq(NspType *A, NspObject *B);
static int type_xdr_save(XDR  *F, NspType *M);
static NspType  *type_xdr_load(XDR  *F);
static AttrTab type_attrs[];
static NspMethods *type_get_methods(void); 
static NspObject *type_path_extract(NspType *A,int n, NspObject **Objs, int *copy);
#endif /* Type_Private */

#define NULLTYPE (NspType*) 0

extern NspType *type_create(char *name,NspTypeBase *type,NspTypeBase *derived_type);
extern NspType *type_copy(NspType *H);
extern void type_destroy(NspType *H);
extern int type_info(NspType *H, int indent,char *name, int rec_level);
extern int type_print(NspType *H, int indent,char *name, int rec_level);

/* from TypeObj.c */

extern NspType *type_object (NspObject *O); 
extern int IsTypeObj (Stack stack, int i); 
extern int IsType(NspObject *O);
extern NspType *GetTypeCopy (Stack stack, int i); 
extern NspType *GetType (Stack stack, int i); 


extern NspHash *nsp_types_hash_table; 
extern NspHash *nsp_gtk_hash_table; 
extern NspHash *nsp_atk_hash_table; 
extern NspHash *nsp_gdk_hash_table; 
extern NspHash *nsp_pango_hash_table; 


extern void *nsp_get_type_from_name(const char *name); 
extern char *type_get_name(void *type);


extern const int nsp_dynamic_id ; 

#endif 

