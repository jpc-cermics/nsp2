#ifndef NSP_INC_DClass
#define NSP_INC_DClass

/*
 * This Software is GPL (Copyright ENPC 2007-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* class DClass */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"

/*
 * NspDClass inherits from NspObject 
 */

typedef struct _NspTypeDClass NspTypeDClass ;

struct _NspTypeDClass { 
  /*< private >*/
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

/**
 * NspDClass: 
 * @hsize: size of dclass table
 * @filled:  number of present entries
 * @htable: an array of size hsize+1.
 * 
 * inherits from #NspObject, used to store dclass tables.
 */

typedef struct _NspDClass NspDClass;

struct _NspDClass {
  /*< private >*/
  NspObject father; 
  NspTypeDClass *type; 
  /*< public >*/
  nsp_string type_name;
  NspHash *hash; 
};

extern int nsp_type_dclass_id;
extern NspTypeDClass *nsp_type_dclass;

int nsp_type_dclass_init();

/* only useful when building a new class derived from dclass */

NspTypeDClass *new_type_dclass(type_mode mode);

/* initialize type for Object */

NspDClass *new_dclass();

/**
 * NULLDCLASS:
 **/

#define NULLDCLASS (NspDClass*) 0

extern NspDClass *nsp_dclass_create(char *name,NspHash *H, const char *type_name);
extern NspDClass *nsp_dclass_copy(NspDClass *H);
extern void nsp_dclass_destroy(NspDClass *H);
extern void nsp_dclass_info(NspDClass *H, int indent,char *name, int rec_level);
extern void nsp_dclass_print(NspDClass *H, int indent,char *name, int rec_level);

/* from DClassObj.c */

extern NspDClass *nsp_dclass_object (NspObject *O); 
extern int IsDClassObj (Stack stack, int i); 
extern int IsDClass(const NspObject *O);
extern NspDClass *GetDClassCopy (Stack stack, int i); 
extern NspDClass *GetDClass (Stack stack, int i); 

/* from file NspDClass.c */ 

extern int nsp_dclass_resize(NspDClass *H, unsigned int new_size); 
extern int nsp_dclass_merge(NspDClass *H1, NspDClass *H2); 
extern int nsp_dclass_get_next_object(NspDClass *H, int *i, NspObject **O); 
extern int nsp_dclass_enter_copy(NspDClass *H, NspObject *O); 
extern int nsp_dclass_enter(NspDClass *H, NspObject *O); 
extern void nsp_dclass_remove(NspDClass *H, char *str); 
extern int nsp_dclass_find_and_copy(NspDClass *H, char *str, NspObject **O); 
extern int nsp_dclass_find(NspDClass *H,const char *str, NspObject **O);
extern NspBMatrix  *nsp_dclass_equal(NspDClass *L1, NspDClass *L2);
extern NspBMatrix  *nsp_dclass_not_equal(NspDClass *L1, NspDClass *L2);
extern int nsp_dclass_full_equal(NspDClass *L1, NspDClass *L2);
extern int nsp_dclass_full_not_equal(NspDClass *L1, NspDClass *L2);

#endif

#ifdef DClass_Private 
/*
 * Object methods redefined for dclass 
 */
static int init_dclass(NspDClass *ob,NspTypeDClass *type);
static int dclass_size(NspDClass *Mat, int flag);
static char *dclass_type_as_string(void);
static char *dclass_type_short_string(NspObject *v);
static int dclass_eq(NspDClass *A, NspObject *B);
static int dclass_neq(NspDClass *A, NspObject *B);
static int dclass_xdr_save(XDR  *F, NspDClass *M);
static NspDClass  *dclass_xdr_load(XDR  *F);
static NspObject *dclass_path_extract(NspDClass *A,int n, NspObject **Objs);
static AttrTab dclass_attrs[]; 
static int int_dclass_get_attribute(Stack stack, int rhs, int opt, int lhs);
static int int_dclass_set_attribute(Stack stack, int rhs, int opt, int lhs);
static NspMethods *dclass_get_methods(void); 
#endif /* DClass_Private */
