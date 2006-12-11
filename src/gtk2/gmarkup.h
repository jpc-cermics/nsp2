/* -*- Mode: C -*- */
#ifndef NSP_INC_GMarkupNode
#define NSP_INC_GMarkupNode

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* GMarkupNode */

#include "nsp/object.h"

/*
 * NspGMarkupNode inherits from NspObject
 */

typedef struct _NspGMarkupNode NspGMarkupNode ;
typedef struct _NspTypeGMarkupNode NspTypeGMarkupNode ;

struct _NspTypeGMarkupNode {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_gmarkup_node nsp_gmarkup_node;

struct _nsp_gmarkup_node {
  char *name;
  NspList *children;
  NspHash *attributes;
  int ref_count;
};

struct _NspGMarkupNode {
  /*< private >*/
  NspObject father;
  NspTypeGMarkupNode*type;
  /*< public >*/
  char *name;
  NspList *children;
  NspHash *attributes;
  NspGMarkupNode *gm_father;
  int level;
};

extern int nsp_type_gmarkup_node_id;
extern NspTypeGMarkupNode *nsp_type_gmarkup_node;

/* type instances for object */

NspTypeGMarkupNode *new_type_gmarkup_node(type_mode mode);

/* instance for GMarkupNode */

NspGMarkupNode *new_gmarkup_node();

/*
 * Object methods redefined for gmarkup_node 
 */


#define NULLMARKUPNODE (NspGMarkupNode*) 0

extern NspGMarkupNode *gmarkup_node_create(char *name,NspTypeBase *type);

/* from GMarkupNodeObj.c */

extern NspGMarkupNode *nsp_gmarkup_node_copy(NspGMarkupNode *H);
extern void nsp_gmarkup_node_destroy(NspGMarkupNode *H);
extern void nsp_gmarkup_node_info(NspGMarkupNode *M, int indent,const char *name, int rec_level);
extern void nsp_gmarkup_node_print(NspGMarkupNode *M, int indent,const char *name, int rec_level);
extern NspGMarkupNode *nsp_gmarkup_node_object (NspObject *O); 
extern int IsGMarkupNodeObj (Stack stack, int i); 
extern int IsGMarkupNode(NspObject *O);
extern NspGMarkupNode *GetGMarkupNodeCopy (Stack stack, int i); 
extern NspGMarkupNode *GetGMarkupNode (Stack stack, int i); 
extern int int_gmarkup_node_create(Stack stack, int rhs, int opt, int lhs);

#endif /* NSP_INC_GMarkupNode */ 

#ifdef GMarkupNode_Private 
static int init_gmarkup_node(NspGMarkupNode *o,NspTypeGMarkupNode *type);
static int nsp_gmarkup_node_size(NspGMarkupNode *Mat, int flag);
static char *nsp_gmarkup_node_type_as_string(void);
static char *nsp_gmarkup_node_type_short_string(void);
static int nsp_gmarkup_node_eq(NspGMarkupNode *A, NspObject *B);
static int nsp_gmarkup_node_neq(NspGMarkupNode *A, NspObject *B);
static int nsp_gmarkup_node_xdr_save(XDR  *xdrs, NspGMarkupNode *M);
static NspGMarkupNode *nsp_gmarkup_node_xdr_load(XDR *xdrs);
static AttrTab gmarkup_node_attrs[];
static NspMethods *gmarkup_node_get_methods(void);
static NspGMarkupNode *gmarkup_node_create_void(char *name,NspTypeBase *type);
#endif /* GMarkupNode_Private */

