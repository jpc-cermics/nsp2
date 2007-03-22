/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCTree
#define INC_NSP_GtkCTree

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkclist.h"

/*
* NspGtkCTree inherits from NspGtkCList
* just change some type attributes 
*/

typedef NspGtkCList NspGtkCTree ;
typedef NspTypeGtkCList NspTypeGtkCTree ;

extern int nsp_type_gtkctree_id;
extern NspTypeGtkCTree *nsp_type_gtkctree;

/* type instances for gtkclist */

NspTypeGtkCTree *new_type_gtkctree(type_mode mode);

/* instance for GtkCTree */

NspGtkCTree *new_gtkctree();

/*
* Object methods redefined for gtkctree 
*/

#ifdef GtkCTree_Private 
static int init_gtkctree(NspGtkCTree *o,NspTypeGtkCTree *type);
static char *gtkctree_type_as_string(void);
static char *gtkctree_type_short_string(NspObject *v);
static AttrTab gtkctree_attrs[];
/* static int int_gtkctree_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkctree_get_methods(void); 
#endif /* GtkCTree_Private */

#define NULLGTKCTREE (NspGtkCTree*) 0

NspGtkCTree *gtkctree_create(char *name,NspTypeBase *type);

/* from GtkCTreeObj.c */

extern NspGtkCTree *gtkctree_object (NspObject *O); 
extern int IsGtkCTreeObj (Stack stack, int i); 
extern int IsGtkCTree(NspObject *O);
extern NspGtkCTree *GetGtkCTreeCopy (Stack stack, int i); 
extern NspGtkCTree *GetGtkCTree (Stack stack, int i); 

#endif 
