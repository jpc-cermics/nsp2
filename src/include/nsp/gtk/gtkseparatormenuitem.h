/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkSeparatorMenuItem
#define INC_NSP_GtkSeparatorMenuItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkmenuitem.h"

/*
* NspGtkSeparatorMenuItem inherits from NspGtkMenuItem
* just change some type attributes 
*/

typedef NspGtkMenuItem NspGtkSeparatorMenuItem ;
typedef NspTypeGtkMenuItem NspTypeGtkSeparatorMenuItem ;

extern int nsp_type_gtkseparatormenuitem_id;
extern NspTypeGtkSeparatorMenuItem *nsp_type_gtkseparatormenuitem;

/* type instances for gtkmenuitem */

NspTypeGtkSeparatorMenuItem *new_type_gtkseparatormenuitem(type_mode mode);

/* instance for GtkSeparatorMenuItem */

NspGtkSeparatorMenuItem *new_gtkseparatormenuitem();

/*
* Object methods redefined for gtkseparatormenuitem 
*/

#ifdef GtkSeparatorMenuItem_Private 
static int init_gtkseparatormenuitem(NspGtkSeparatorMenuItem *o,NspTypeGtkSeparatorMenuItem *type);
static char *gtkseparatormenuitem_type_as_string(void);
static char *gtkseparatormenuitem_type_short_string(void);
static AttrTab gtkseparatormenuitem_attrs[];
/* static int int_gtkseparatormenuitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkseparatormenuitem_get_methods(void); 
#endif /* GtkSeparatorMenuItem_Private */

#define NULLGTKSEPARATORMENUITEM (NspGtkSeparatorMenuItem*) 0

NspGtkSeparatorMenuItem *gtkseparatormenuitem_create(char *name,NspTypeBase *type);

/* from GtkSeparatorMenuItemObj.c */

extern NspGtkSeparatorMenuItem *gtkseparatormenuitem_object (NspObject *O); 
extern int IsGtkSeparatorMenuItemObj (Stack stack, int i); 
extern int IsGtkSeparatorMenuItem(NspObject *O);
extern NspGtkSeparatorMenuItem *GetGtkSeparatorMenuItemCopy (Stack stack, int i); 
extern NspGtkSeparatorMenuItem *GetGtkSeparatorMenuItem (Stack stack, int i); 

#endif 
