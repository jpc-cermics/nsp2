/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeView
#define INC_NSP_GtkTreeView

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkTreeView inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkTreeView ;
typedef NspTypeGtkContainer NspTypeGtkTreeView ;

extern int nsp_type_gtktreeview_id;
extern NspTypeGtkTreeView *nsp_type_gtktreeview;

/* type instances for gtkcontainer */

NspTypeGtkTreeView *new_type_gtktreeview(type_mode mode);

/* instance for GtkTreeView */

NspGtkTreeView *new_gtktreeview();

/*
* Object methods redefined for gtktreeview 
*/

#ifdef GtkTreeView_Private 
static int init_gtktreeview(NspGtkTreeView *o,NspTypeGtkTreeView *type);
static char *gtktreeview_type_as_string(void);
static char *gtktreeview_type_short_string(void);
static AttrTab gtktreeview_attrs[];
/* static int int_gtktreeview_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreeview_get_methods(void); 
#endif /* GtkTreeView_Private */

#define NULLGTKTREEVIEW (NspGtkTreeView*) 0

NspGtkTreeView *gtktreeview_create(char *name,NspTypeBase *type);

/* from GtkTreeViewObj.c */

extern NspGtkTreeView *gtktreeview_object (NspObject *O); 
extern int IsGtkTreeViewObj (Stack stack, int i); 
extern int IsGtkTreeView(NspObject *O);
extern NspGtkTreeView *GetGtkTreeViewCopy (Stack stack, int i); 
extern NspGtkTreeView *GetGtkTreeView (Stack stack, int i); 

#endif 
