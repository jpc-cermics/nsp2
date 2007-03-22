/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkImageMenuItem
#define INC_NSP_GtkImageMenuItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkmenuitem.h"

/*
* NspGtkImageMenuItem inherits from NspGtkMenuItem
* just change some type attributes 
*/

typedef NspGtkMenuItem NspGtkImageMenuItem ;
typedef NspTypeGtkMenuItem NspTypeGtkImageMenuItem ;

extern int nsp_type_gtkimagemenuitem_id;
extern NspTypeGtkImageMenuItem *nsp_type_gtkimagemenuitem;

/* type instances for gtkmenuitem */

NspTypeGtkImageMenuItem *new_type_gtkimagemenuitem(type_mode mode);

/* instance for GtkImageMenuItem */

NspGtkImageMenuItem *new_gtkimagemenuitem();

/*
* Object methods redefined for gtkimagemenuitem 
*/

#define NULLGTKIMAGEMENUITEM (NspGtkImageMenuItem*) 0

NspGtkImageMenuItem *gtkimagemenuitem_create(char *name,NspTypeBase *type);

/* from GtkImageMenuItemObj.c */

extern NspGtkImageMenuItem *gtkimagemenuitem_object (NspObject *O); 
extern int IsGtkImageMenuItemObj (Stack stack, int i); 
extern int IsGtkImageMenuItem(NspObject *O);
extern NspGtkImageMenuItem *GetGtkImageMenuItemCopy (Stack stack, int i); 
extern NspGtkImageMenuItem *GetGtkImageMenuItem (Stack stack, int i); 

#endif 

#ifdef GtkImageMenuItem_Private 
static int init_gtkimagemenuitem(NspGtkImageMenuItem *o,NspTypeGtkImageMenuItem *type);
static char *gtkimagemenuitem_type_as_string(void);
static char *gtkimagemenuitem_type_short_string(NspObject *v);
static AttrTab gtkimagemenuitem_attrs[];
/* static int int_gtkimagemenuitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkimagemenuitem_get_methods(void); 
#endif /* GtkImageMenuItem_Private */
