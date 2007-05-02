/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeViewColumn
#define INC_NSP_GtkTreeViewColumn

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkobject.h"

/*
* NspGtkTreeViewColumn inherits from NspGtkObject
* just change some type attributes 
*/

typedef NspGtkObject NspGtkTreeViewColumn ;
typedef NspTypeGtkObject NspTypeGtkTreeViewColumn ;

extern int nsp_type_gtktreeviewcolumn_id;
extern NspTypeGtkTreeViewColumn *nsp_type_gtktreeviewcolumn;

/* type instances for gtkobject */

NspTypeGtkTreeViewColumn *new_type_gtktreeviewcolumn(type_mode mode);

/* instance for GtkTreeViewColumn */

NspGtkTreeViewColumn *new_gtktreeviewcolumn();

/*
* Object methods redefined for gtktreeviewcolumn 
*/

#define NULLGTKTREEVIEWCOLUMN (NspGtkTreeViewColumn*) 0

NspGtkTreeViewColumn *gtktreeviewcolumn_create(char *name,NspTypeBase *type);

/* from GtkTreeViewColumnObj.c */

extern NspGtkTreeViewColumn *gtktreeviewcolumn_object (NspObject *O); 
extern int IsGtkTreeViewColumnObj (Stack stack, int i); 
extern int IsGtkTreeViewColumn(NspObject *O);
extern NspGtkTreeViewColumn *GetGtkTreeViewColumnCopy (Stack stack, int i); 
extern NspGtkTreeViewColumn *GetGtkTreeViewColumn (Stack stack, int i); 

#endif 

#ifdef GtkTreeViewColumn_Private 
static int init_gtktreeviewcolumn(NspGtkTreeViewColumn *o,NspTypeGtkTreeViewColumn *type);
static char *gtktreeviewcolumn_type_as_string(void);
static char *gtktreeviewcolumn_type_short_string(NspObject *v);
static AttrTab gtktreeviewcolumn_attrs[];
/* static int int_gtktreeviewcolumn_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreeviewcolumn_get_methods(void); 
#endif /* GtkTreeViewColumn_Private */
