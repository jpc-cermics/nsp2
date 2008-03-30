/* -*- Mode: C -*- */
#ifndef NSP_INC_GRoot
#define NSP_INC_GRoot

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* GRoot */

#include "nsp/object.h"

/*
 * NspGRoot inherits from NspObject
 */

typedef struct _NspGRoot NspGRoot ;
typedef struct _NspTypeGRoot NspTypeGRoot ;


struct _NspTypeGRoot {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
  
};

typedef struct _nsp_groot nsp_groot;
struct _nsp_groot {
  NspList* figures;
  int ref_count;
};

struct _NspGRoot {
  /*< private >*/
  NspObject father;
  NspTypeGRoot*type;
  /*< public >*/
  nsp_groot *obj;
};

extern int nsp_type_groot_id;
extern NspTypeGRoot *nsp_type_groot;

/* type instances for object */

NspTypeGRoot *new_type_groot(type_mode mode);

/* instance for GRoot */

NspGRoot *new_groot();

/*
* Object methods redefined for groot 
*/


#define NULLGROOT (NspGRoot*) 0

extern NspGRoot *nsp_groot_create(char *name,NspList* figures,NspTypeBase *type);

/* from GRootObj.c */

extern NspGRoot *nsp_groot_copy(NspGRoot *H);
extern void nsp_groot_destroy(NspGRoot *H);
extern int nsp_groot_info(NspGRoot *H, int indent,const char *name, int rec_level);
extern int nsp_groot_print(NspGRoot *H, int indent,const char *name, int rec_level);
extern int nsp_groot_latex(NspGRoot *H, int indent,const char *name, int rec_level);
extern NspGRoot *nsp_groot_object (NspObject *O); 
extern int IsGRootObj (Stack stack, int i); 
extern int IsGRoot(NspObject *O);
extern NspGRoot *GetGRootCopy (Stack stack, int i); 
extern NspGRoot *GetGRoot (Stack stack, int i); 
extern int nsp_groot_create_partial(NspGRoot *H);
extern void nsp_groot_destroy_partial(NspGRoot *H);
extern NspGRoot * nsp_groot_copy_partial(NspGRoot *H,NspGRoot *self);
extern NspGRoot * nsp_groot_full_copy_partial(NspGRoot *H,NspGRoot *self);
extern NspGRoot * nsp_groot_full_copy(NspGRoot *self);
extern int nsp_groot_check_values(NspGRoot *H);
extern int int_groot_create(Stack stack, int rhs, int opt, int lhs); 
extern NspGRoot *nsp_groot_xdr_load_partial(XDR *xdrs, NspGRoot *M);
extern int nsp_groot_xdr_save(XDR  *xdrs, NspGRoot *M);

#endif /* NSP_INC_GRoot */ 

#ifdef GRoot_Private 
static int init_groot(NspGRoot *o,NspTypeGRoot *type);
static int nsp_groot_size(NspGRoot *Mat, int flag);
static char *nsp_groot_type_as_string(void);
static char *nsp_groot_type_short_string(NspObject *v);
static int nsp_groot_eq(NspGRoot *A, NspObject *B);
static int nsp_groot_neq(NspGRoot *A, NspObject *B);
static NspGRoot *nsp_groot_xdr_load(XDR *xdrs);
static AttrTab groot_attrs[];
static NspMethods *groot_get_methods(void);
/* static int int_groot_create(Stack stack, int rhs, int opt, int lhs);*/ 
static NspGRoot *nsp_groot_create_void(char *name,NspTypeBase *type);
#endif /* GRoot_Private */

