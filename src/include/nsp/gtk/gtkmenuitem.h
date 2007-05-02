/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkMenuItem
#define INC_NSP_GtkMenuItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkitem.h"

/*
* NspGtkMenuItem inherits from NspGtkItem
* just change some type attributes 
*/

typedef NspGtkItem NspGtkMenuItem ;
typedef NspTypeGtkItem NspTypeGtkMenuItem ;

extern int nsp_type_gtkmenuitem_id;
extern NspTypeGtkMenuItem *nsp_type_gtkmenuitem;

/* type instances for gtkitem */

NspTypeGtkMenuItem *new_type_gtkmenuitem(type_mode mode);

/* instance for GtkMenuItem */

NspGtkMenuItem *new_gtkmenuitem();

/*
* Object methods redefined for gtkmenuitem 
*/

#define NULLGTKMENUITEM (NspGtkMenuItem*) 0

NspGtkMenuItem *gtkmenuitem_create(char *name,NspTypeBase *type);

/* from GtkMenuItemObj.c */

extern NspGtkMenuItem *gtkmenuitem_object (NspObject *O); 
extern int IsGtkMenuItemObj (Stack stack, int i); 
extern int IsGtkMenuItem(NspObject *O);
extern NspGtkMenuItem *GetGtkMenuItemCopy (Stack stack, int i); 
extern NspGtkMenuItem *GetGtkMenuItem (Stack stack, int i); 

#endif 

#ifdef GtkMenuItem_Private 
static int init_gtkmenuitem(NspGtkMenuItem *o,NspTypeGtkMenuItem *type);
static char *gtkmenuitem_type_as_string(void);
static char *gtkmenuitem_type_short_string(NspObject *v);
static AttrTab gtkmenuitem_attrs[];
/* static int int_gtkmenuitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkmenuitem_get_methods(void); 
#endif /* GtkMenuItem_Private */
