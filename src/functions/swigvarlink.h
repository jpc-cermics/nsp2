/* -*- Mode: C -*- */
#ifndef NSP_INC_SwigVarLink
#define NSP_INC_SwigVarLink

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* SwigVarLink */

#include "nsp/object.h"

/*
 * NspSwigVarLink inherits from NspObject
 */

typedef struct _NspSwigVarLink NspSwigVarLink ;
typedef struct _NspTypeSwigVarLink NspTypeSwigVarLink ;

struct _NspTypeSwigVarLink {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_swigvarlink nsp_swigvarlink;
struct _nsp_swigvarlink {
  NspHash *table;
  int ref_count;
};

struct _NspSwigVarLink {
  /*< private >*/
  NspObject father;
  NspTypeSwigVarLink*type;
  /*< public >*/
  nsp_swigvarlink *obj;
};

extern int nsp_type_swigvarlink_id;
extern NspTypeSwigVarLink *nsp_type_swigvarlink;

/* type instances for object */

NspTypeSwigVarLink *new_type_swigvarlink(type_mode mode);

/* instance for SwigVarLink */

NspSwigVarLink *new_swigvarlink();

/*
* Object methods redefined for swigvarlink 
*/


#define NULLSWIGVARLINK (NspSwigVarLink*) 0

extern NspSwigVarLink *swigvarlink_create(char *name);

/* from SwigVarLinkObj.c */

extern NspSwigVarLink *nsp_swigvarlink_copy(NspSwigVarLink *H);
extern void nsp_swigvarlink_destroy(NspSwigVarLink *H);
extern void nsp_swigvarlink_info(NspSwigVarLink *H, int indent,const char *name, int rec_level);
extern int nsp_swigvarlink_print(NspSwigVarLink *H, int indent,const char *name, int rec_level);
extern void nsp_swigvarlink_latex_print(NspSwigVarLink *H, int indent,const char *name, int rec_level);
extern NspSwigVarLink *nsp_swigvarlink_object (NspObject *O); 
extern int IsSwigVarLinkObj (Stack stack, int i); 
extern int IsSwigVarLink(NspObject *O);
extern NspSwigVarLink *GetSwigVarLinkCopy (Stack stack, int i); 
extern NspSwigVarLink *GetSwigVarLink (Stack stack, int i); 

#endif /* NSP_INC_SwigVarLink */ 

#ifdef SwigVarLink_Private 
static int init_swigvarlink(NspSwigVarLink *o,NspTypeSwigVarLink *type);
static int nsp_swigvarlink_size(NspSwigVarLink *Mat, int flag);
static char *nsp_swigvarlink_type_as_string(void);
static char *nsp_swigvarlink_type_short_string(NspObject *v);
static int nsp_swigvarlink_eq(NspSwigVarLink *A, NspObject *B);
static int nsp_swigvarlink_neq(NspSwigVarLink *A, NspObject *B);
static int nsp_swigvarlink_xdr_save(XDR  *xdrs, NspSwigVarLink *M);
static NspSwigVarLink *nsp_swigvarlink_xdr_load(XDR *xdrs);
static AttrTab swigvarlink_attrs[];
static NspMethods *swigvarlink_get_methods(void);
extern int int_swigvarlink_create(Stack stack, int rhs, int opt, int lhs); 
static NspSwigVarLink *swigvarlink_create_void(char *name,NspTypeBase *type);
static int int_swigvarlink_get_attribute(Stack stack, int rhs, int opt, int lhs);
static int int_swigvarlink_set_attribute(Stack stack, int rhs, int opt, int lhs);
#endif /* SwigVarLink_Private */

