/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkMenu
#define INC_NSP_GtkMenu

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkmenushell.h"

/*
* NspGtkMenu inherits from NspGtkMenuShell
* just change some type attributes 
*/

typedef NspGtkMenuShell NspGtkMenu ;
typedef NspTypeGtkMenuShell NspTypeGtkMenu ;

extern int nsp_type_gtkmenu_id;
extern NspTypeGtkMenu *nsp_type_gtkmenu;

/* type instances for gtkmenushell */

NspTypeGtkMenu *new_type_gtkmenu(type_mode mode);

/* instance for GtkMenu */

NspGtkMenu *new_gtkmenu();

/*
* Object methods redefined for gtkmenu 
*/

#define NULLGTKMENU (NspGtkMenu*) 0

NspGtkMenu *gtkmenu_create(char *name,NspTypeBase *type);

/* from GtkMenuObj.c */

extern NspGtkMenu *gtkmenu_object (NspObject *O); 
extern int IsGtkMenuObj (Stack stack, int i); 
extern int IsGtkMenu(NspObject *O);
extern NspGtkMenu *GetGtkMenuCopy (Stack stack, int i); 
extern NspGtkMenu *GetGtkMenu (Stack stack, int i); 

#endif 

#ifdef GtkMenu_Private 
static int init_gtkmenu(NspGtkMenu *o,NspTypeGtkMenu *type);
static char *gtkmenu_type_as_string(void);
static char *gtkmenu_type_short_string(void);
static AttrTab gtkmenu_attrs[];
/* static int int_gtkmenu_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkmenu_get_methods(void); 
#endif /* GtkMenu_Private */
