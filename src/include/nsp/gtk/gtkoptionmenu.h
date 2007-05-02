/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkOptionMenu
#define INC_NSP_GtkOptionMenu

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbutton.h"

/*
* NspGtkOptionMenu inherits from NspGtkButton
* just change some type attributes 
*/

typedef NspGtkButton NspGtkOptionMenu ;
typedef NspTypeGtkButton NspTypeGtkOptionMenu ;

extern int nsp_type_gtkoptionmenu_id;
extern NspTypeGtkOptionMenu *nsp_type_gtkoptionmenu;

/* type instances for gtkbutton */

NspTypeGtkOptionMenu *new_type_gtkoptionmenu(type_mode mode);

/* instance for GtkOptionMenu */

NspGtkOptionMenu *new_gtkoptionmenu();

/*
* Object methods redefined for gtkoptionmenu 
*/

#define NULLGTKOPTIONMENU (NspGtkOptionMenu*) 0

NspGtkOptionMenu *gtkoptionmenu_create(char *name,NspTypeBase *type);

/* from GtkOptionMenuObj.c */

extern NspGtkOptionMenu *gtkoptionmenu_object (NspObject *O); 
extern int IsGtkOptionMenuObj (Stack stack, int i); 
extern int IsGtkOptionMenu(NspObject *O);
extern NspGtkOptionMenu *GetGtkOptionMenuCopy (Stack stack, int i); 
extern NspGtkOptionMenu *GetGtkOptionMenu (Stack stack, int i); 

#endif 

#ifdef GtkOptionMenu_Private 
static int init_gtkoptionmenu(NspGtkOptionMenu *o,NspTypeGtkOptionMenu *type);
static char *gtkoptionmenu_type_as_string(void);
static char *gtkoptionmenu_type_short_string(NspObject *v);
static AttrTab gtkoptionmenu_attrs[];
/* static int int_gtkoptionmenu_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkoptionmenu_get_methods(void); 
#endif /* GtkOptionMenu_Private */
