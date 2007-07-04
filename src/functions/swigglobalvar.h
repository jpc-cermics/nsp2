/* -*- Mode: C -*- */
#ifndef NSP_INC_SwigGlobalVar
#define NSP_INC_SwigGlobalVar

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* SwigGlobalVar */

#include "nsp/object.h"

/*
 * NspSwigGlobalVar inherits from NspObject
 */

typedef struct _NspSwigGlobalVar NspSwigGlobalVar ;
typedef struct _NspTypeSwigGlobalVar NspTypeSwigGlobalVar ;

struct _NspTypeSwigGlobalVar {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};


typedef NspObject *(swig_gv_get_attr)(void);
typedef int (swig_gv_set_attr)(NspObject *);

struct _NspSwigGlobalVar {
  /*< private >*/
  NspObject father;
  NspTypeSwigGlobalVar*type;
  /*< public >*/
  swig_gv_get_attr *get_attr;
  swig_gv_set_attr *set_attr;
};

extern int nsp_type_swigglobalvar_id;
extern NspTypeSwigGlobalVar *nsp_type_swigglobalvar;

/* type instances for object */

NspTypeSwigGlobalVar *new_type_swigglobalvar(type_mode mode);

/* instance for SwigGlobalVar */

NspSwigGlobalVar *new_swigglobalvar();

/*
* Object methods redefined for swigglobalvar 
*/


#define NULLSWIGGLOBALVAR (NspSwigGlobalVar*) 0

extern NspSwigGlobalVar *swigglobalvar_create(char *name,swig_gv_get_attr *get_attr,swig_gv_set_attr *set_attr,
				       NspTypeBase *type);

/* from SwigGlobalVarObj.c */

extern NspSwigGlobalVar *nsp_swigglobalvar_copy(NspSwigGlobalVar *H);
extern void nsp_swigglobalvar_destroy(NspSwigGlobalVar *H);
extern void nsp_swigglobalvar_info(NspSwigGlobalVar *H, int indent,const char *name, int rec_level);
extern void nsp_swigglobalvar_print(NspSwigGlobalVar *H, int indent,const char *name, int rec_level);
extern void nsp_swigglobalvar_latex_print(NspSwigGlobalVar *H, int indent,const char *name, int rec_level);
extern NspSwigGlobalVar *nsp_swigglobalvar_object (NspObject *O); 
extern int IsSwigGlobalVarObj (Stack stack, int i); 
extern int IsSwigGlobalVar(NspObject *O);
extern NspSwigGlobalVar *GetSwigGlobalVarCopy (Stack stack, int i); 
extern NspSwigGlobalVar *GetSwigGlobalVar (Stack stack, int i); 

#endif /* NSP_INC_SwigGlobalVar */ 

#ifdef SwigGlobalVar_Private 
static int init_swigglobalvar(NspSwigGlobalVar *o,NspTypeSwigGlobalVar *type);
static int nsp_swigglobalvar_size(NspSwigGlobalVar *Mat, int flag);
static char *nsp_swigglobalvar_type_as_string(void);
static char *nsp_swigglobalvar_type_short_string(NspObject *v);
static int nsp_swigglobalvar_eq(NspSwigGlobalVar *A, NspObject *B);
static int nsp_swigglobalvar_neq(NspSwigGlobalVar *A, NspObject *B);
static int nsp_swigglobalvar_xdr_save(XDR  *xdrs, NspSwigGlobalVar *M);
static NspSwigGlobalVar *nsp_swigglobalvar_xdr_load(XDR *xdrs);
static AttrTab swigglobalvar_attrs[];
static NspMethods *swigglobalvar_get_methods(void);
static int int_swigglobalvar_create(Stack stack, int rhs, int opt, int lhs);
static NspSwigGlobalVar *swigglobalvar_create_void(char *name,NspTypeBase *type);
#endif /* SwigGlobalVar_Private */

