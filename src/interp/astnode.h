/* -*- Mode: C -*- */
#ifndef NSP_INC_AstNode
#define NSP_INC_AstNode

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* AstNode */

#include "nsp/object.h"

/*
 * NspAstNode inherits from NspObject
 */

typedef struct _NspAstNode NspAstNode ;
typedef struct _NspTypeAstNode NspTypeAstNode ;

struct _NspTypeAstNode {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_astnode nsp_astnode;
struct _nsp_astnode {
  int op;
  int arity; /* arity for operators or tag for names etc.. */
  void *obj; /* used to store a string pointer, 
	      * an object pointer a parse_double
	      * an int. Maybe an union could help.
	      */
  int ref_count;
};

struct _NspAstNode {
  /*< private >*/
  NspObject father;
  NspTypeAstNode*type;
  /*< public >*/
  nsp_astnode *obj;
};

extern int nsp_type_astnode_id;
extern NspTypeAstNode *nsp_type_astnode;

/* type instances for object */

NspTypeAstNode *new_type_astnode(type_mode mode);

/* instance for AstNode */

NspAstNode *new_astnode();

/*
 * Object methods redefined for astnode 
 */


#define NULLASTNODE (NspAstNode*) 0

extern NspAstNode *astnode_create(char *name,int op,int arity,void *data,NspTypeBase *type);

/* from AstNodeObj.c */

extern NspAstNode *nsp_astnode_copy(NspAstNode *H);
extern void nsp_astnode_destroy(NspAstNode *H);
extern void nsp_astnode_info(NspAstNode *M, int indent,const char *name, int rec_level);
extern void nsp_astnode_print(NspAstNode *M, int indent,const char *name, int rec_level);
extern NspAstNode *nsp_astnode_object (NspObject *O); 
extern int IsAstNodeObj (Stack stack, int i); 
extern int IsAstNode(NspObject *O);
extern NspAstNode *GetAstNodeCopy (Stack stack, int i); 
extern NspAstNode *GetAstNode (Stack stack, int i); 
extern int int_astnode_create(Stack stack, int rhs, int opt, int lhs);

#endif /* NSP_INC_AstNode */ 

#ifdef AstNode_Private 
static int init_astnode(NspAstNode *o,NspTypeAstNode *type);
static int nsp_astnode_size(NspAstNode *Mat, int flag);
static char *nsp_astnode_type_as_string(void);
static char *nsp_astnode_type_short_string(void);
static int nsp_astnode_eq(NspAstNode *A, NspObject *B);
static int nsp_astnode_neq(NspAstNode *A, NspObject *B);
static int nsp_astnode_xdr_save(XDR  *xdrs, NspAstNode *M);
static NspAstNode *nsp_astnode_xdr_load(XDR *xdrs);
static AttrTab astnode_attrs[];
static NspMethods *astnode_get_methods(void);
static NspAstNode *astnode_create_void(char *name,NspTypeBase *type);
#endif /* AstNode_Private */

