/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkRadioMenuItem
#define INC_NSP_GtkRadioMenuItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcheckmenuitem.h"

/*
* NspGtkRadioMenuItem inherits from NspGtkCheckMenuItem
* just change some type attributes 
*/

typedef NspGtkCheckMenuItem NspGtkRadioMenuItem ;
typedef NspTypeGtkCheckMenuItem NspTypeGtkRadioMenuItem ;

extern int nsp_type_gtkradiomenuitem_id;
extern NspTypeGtkRadioMenuItem *nsp_type_gtkradiomenuitem;

/* type instances for gtkcheckmenuitem */

NspTypeGtkRadioMenuItem *new_type_gtkradiomenuitem(type_mode mode);

/* instance for GtkRadioMenuItem */

NspGtkRadioMenuItem *new_gtkradiomenuitem();

/*
* Object methods redefined for gtkradiomenuitem 
*/

#ifdef GtkRadioMenuItem_Private 
static int init_gtkradiomenuitem(NspGtkRadioMenuItem *o,NspTypeGtkRadioMenuItem *type);
static char *gtkradiomenuitem_type_as_string(void);
static char *gtkradiomenuitem_type_short_string(void);
static AttrTab gtkradiomenuitem_attrs[];
/* static int int_gtkradiomenuitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkradiomenuitem_get_methods(void); 
#endif /* GtkRadioMenuItem_Private */

#define NULLGTKRADIOMENUITEM (NspGtkRadioMenuItem*) 0

NspGtkRadioMenuItem *gtkradiomenuitem_create(char *name,NspTypeBase *type);

/* from GtkRadioMenuItemObj.c */

extern NspGtkRadioMenuItem *gtkradiomenuitem_object (NspObject *O); 
extern int IsGtkRadioMenuItemObj (Stack stack, int i); 
extern int IsGtkRadioMenuItem(NspObject *O);
extern NspGtkRadioMenuItem *GetGtkRadioMenuItemCopy (Stack stack, int i); 
extern NspGtkRadioMenuItem *GetGtkRadioMenuItem (Stack stack, int i); 

#endif 
