#ifndef NSP_INC_None
#define NSP_INC_None

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* None */

#include "nsp/object.h"

/*
 * NspNone inherits from NspObject
 */

typedef struct _NspNone NspNone;

typedef struct _NspTypeNone { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeNone;

struct _NspNone {
  /*< private >*/
  NspObject father; 
  NspTypeNone *type; 
  /*< public >*/
  
};

extern int nsp_type_none_id;
extern NspTypeNone *nsp_type_none;

/* type instances for classa */

NspTypeNone *new_type_none(type_mode mode);

/* instance for None */

NspNone *new_none();

/*
 * Object methods redefined for none 
 */

#ifdef None_Private 
static int init_none(NspNone *o,NspTypeNone *type);
static int none_size(NspNone *Mat, int flag);
static char *none_type_as_string(void);
static char *none_type_short_string(void);
static int none_eq(NspNone *A, NspObject *B);
static int none_neq(NspNone *A, NspObject *B);
static int none_xdr_save(NspFile  *F, NspNone *M);
static NspNone  *none_xdr_load(NspFile  *F);
static AttrTab none_attrs[];
static NspMethods *none_get_methods(void); 
static NspObject *none_path_extract(NspNone *A, NspObject *O);
#endif /* None_Private */

#define NULLNONE (NspNone*) 0

NspNone *none_create(char *name,NspTypeBase *type);
NspNone *none_copy(NspNone *H);
void none_destroy(NspNone *H);
void none_info(NspNone *H, int indent);
void none_print(NspNone *H, int indent);

/* from NoneObj.c */

extern NspNone *none_object (NspObject *O); 
extern int IsNoneObj (Stack stack, int i); 
extern int IsNone(NspObject *O);
extern NspNone *GetNoneCopy (Stack stack, int i); 
extern NspNone *GetNone (Stack stack, int i); 



#endif 

