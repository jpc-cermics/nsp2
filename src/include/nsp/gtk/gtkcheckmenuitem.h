/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCheckMenuItem
#define INC_NSP_GtkCheckMenuItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkmenuitem.h"

/*
* NspGtkCheckMenuItem inherits from NspGtkMenuItem
* just change some type attributes 
*/

typedef NspGtkMenuItem NspGtkCheckMenuItem ;
typedef NspTypeGtkMenuItem NspTypeGtkCheckMenuItem ;

extern int nsp_type_gtkcheckmenuitem_id;
extern NspTypeGtkCheckMenuItem *nsp_type_gtkcheckmenuitem;

/* type instances for gtkmenuitem */

NspTypeGtkCheckMenuItem *new_type_gtkcheckmenuitem(type_mode mode);

/* instance for GtkCheckMenuItem */

NspGtkCheckMenuItem *new_gtkcheckmenuitem();

/*
* Object methods redefined for gtkcheckmenuitem 
*/

#define NULLGTKCHECKMENUITEM (NspGtkCheckMenuItem*) 0

NspGtkCheckMenuItem *gtkcheckmenuitem_create(char *name,NspTypeBase *type);

/* from GtkCheckMenuItemObj.c */

extern NspGtkCheckMenuItem *gtkcheckmenuitem_object (NspObject *O); 
extern int IsGtkCheckMenuItemObj (Stack stack, int i); 
extern int IsGtkCheckMenuItem(NspObject *O);
extern NspGtkCheckMenuItem *GetGtkCheckMenuItemCopy (Stack stack, int i); 
extern NspGtkCheckMenuItem *GetGtkCheckMenuItem (Stack stack, int i); 

#endif 

#ifdef GtkCheckMenuItem_Private 
static int init_gtkcheckmenuitem(NspGtkCheckMenuItem *o,NspTypeGtkCheckMenuItem *type);
static char *gtkcheckmenuitem_type_as_string(void);
static char *gtkcheckmenuitem_type_short_string(NspObject *v);
static AttrTab gtkcheckmenuitem_attrs[];
/* static int int_gtkcheckmenuitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcheckmenuitem_get_methods(void); 
#endif /* GtkCheckMenuItem_Private */
