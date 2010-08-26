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

/* typedef struct _NspNone NspNone; */

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
static int nsp_init_none(NspNone *o,NspTypeNone *type);
static int nsp_none_size(NspNone *Mat, int flag);
static char *nsp_none_type_as_string(void);
static char *nsp_none_type_short_string(NspObject *v);
static int nsp_none_eq(NspNone *A, NspObject *B);
static int nsp_none_neq(NspNone *A, NspObject *B);
static int nsp_none_xdr_save(XDR  *F, NspNone *M);
static NspNone  *nsp_none_xdr_load(XDR  *F);
static AttrTab none_attrs[];
static NspMethods *nsp_none_get_methods(void); 
static NspObject *nsp_none_path_extract(NspNone *A,int n, NspObject **Objs, int *copy);
#endif /* None_Private */

#define NULLNONE (NspNone*) 0

extern NspNone *nsp_none_create(char *name,NspTypeBase *type);
extern NspNone *nsp_none_copy(NspNone *H);
extern void nsp_none_destroy(NspNone *H);
extern int nsp_none_info(NspNone *H, int indent,char *name, int rec_level);
extern int nsp_none_print(NspNone *H, int indent,char *name, int rec_level);

/* from NoneObj.c */

extern NspNone *nsp_none_object (NspObject *O); 
extern int IsNoneObj (Stack stack, int i); 
extern int IsNone(NspObject *O);
extern NspNone *GetNoneCopy (Stack stack, int i); 
extern NspNone *GetNone (Stack stack, int i); 



#endif 

