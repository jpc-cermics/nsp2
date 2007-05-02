/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeModel
#define INC_NSP_GtkTreeModel

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTreeModel inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTreeModel ;
typedef NspTypeGObject NspTypeGtkTreeModel ;

extern int nsp_type_gtktreemodel_id;
extern NspTypeGtkTreeModel *nsp_type_gtktreemodel;

/* type instances for gobject */

NspTypeGtkTreeModel *new_type_gtktreemodel(type_mode mode);

/* instance for GtkTreeModel */

NspGtkTreeModel *new_gtktreemodel();

/*
* Object methods redefined for gtktreemodel 
*/

#define NULLGTKTREEMODEL (NspGtkTreeModel*) 0

NspGtkTreeModel *gtktreemodel_create(char *name,NspTypeBase *type);

/* from GtkTreeModelObj.c */

extern NspGtkTreeModel *gtktreemodel_object (NspObject *O); 
extern int IsGtkTreeModelObj (Stack stack, int i); 
extern int IsGtkTreeModel(NspObject *O);
extern NspGtkTreeModel *GetGtkTreeModelCopy (Stack stack, int i); 
extern NspGtkTreeModel *GetGtkTreeModel (Stack stack, int i); 

#endif 

#ifdef GtkTreeModel_Private 
static int init_gtktreemodel(NspGtkTreeModel *o,NspTypeGtkTreeModel *type);
static char *gtktreemodel_type_as_string(void);
static char *gtktreemodel_type_short_string(NspObject *v);
static AttrTab gtktreemodel_attrs[];
/* static int int_gtktreemodel_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreemodel_get_methods(void); 
#endif /* GtkTreeModel_Private */
