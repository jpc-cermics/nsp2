/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCTreeNode
#define INC_NSP_GtkCTreeNode

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gpointer.h"

/*
* NspGtkCTreeNode inherits from NspGPointer
* just change some type attributes 
*/

typedef NspGPointer NspGtkCTreeNode ;
typedef NspTypeGPointer NspTypeGtkCTreeNode ;

extern int nsp_type_gtkctreenode_id;
extern NspTypeGtkCTreeNode *nsp_type_gtkctreenode;

/* type instances for gpointer */

NspTypeGtkCTreeNode *new_type_gtkctreenode(type_mode mode);

/* instance for GtkCTreeNode */

NspGtkCTreeNode *new_gtkctreenode();

/*
* Object methods redefined for gtkctreenode 
*/

#ifdef GtkCTreeNode_Private 
static int init_gtkctreenode(NspGtkCTreeNode *o,NspTypeGtkCTreeNode *type);
static char *gtkctreenode_type_as_string(void);
static char *gtkctreenode_type_short_string(void);
static AttrTab gtkctreenode_attrs[];
/* static int int_gtkctreenode_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkctreenode_get_methods(void); 
#endif /* GtkCTreeNode_Private */

#define NULLGTKCTREENODE (NspGtkCTreeNode*) 0

NspGtkCTreeNode *gtkctreenode_create(char *name,NspTypeBase *type);

/* from GtkCTreeNodeObj.c */

extern NspGtkCTreeNode *gtkctreenode_object (NspObject *O); 
extern int IsGtkCTreeNodeObj (Stack stack, int i); 
extern int IsGtkCTreeNode(NspObject *O);
extern NspGtkCTreeNode *GetGtkCTreeNodeCopy (Stack stack, int i); 
extern NspGtkCTreeNode *GetGtkCTreeNode (Stack stack, int i); 

#endif 
