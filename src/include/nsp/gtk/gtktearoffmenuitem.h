/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTearoffMenuItem
#define INC_NSP_GtkTearoffMenuItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkmenuitem.h"

/*
* NspGtkTearoffMenuItem inherits from NspGtkMenuItem
* just change some type attributes 
*/

typedef NspGtkMenuItem NspGtkTearoffMenuItem ;
typedef NspTypeGtkMenuItem NspTypeGtkTearoffMenuItem ;

extern int nsp_type_gtktearoffmenuitem_id;
extern NspTypeGtkTearoffMenuItem *nsp_type_gtktearoffmenuitem;

/* type instances for gtkmenuitem */

NspTypeGtkTearoffMenuItem *new_type_gtktearoffmenuitem(type_mode mode);

/* instance for GtkTearoffMenuItem */

NspGtkTearoffMenuItem *new_gtktearoffmenuitem();

/*
* Object methods redefined for gtktearoffmenuitem 
*/

#define NULLGTKTEAROFFMENUITEM (NspGtkTearoffMenuItem*) 0

NspGtkTearoffMenuItem *gtktearoffmenuitem_create(char *name,NspTypeBase *type);

/* from GtkTearoffMenuItemObj.c */

extern NspGtkTearoffMenuItem *gtktearoffmenuitem_object (NspObject *O); 
extern int IsGtkTearoffMenuItemObj (Stack stack, int i); 
extern int IsGtkTearoffMenuItem(NspObject *O);
extern NspGtkTearoffMenuItem *GetGtkTearoffMenuItemCopy (Stack stack, int i); 
extern NspGtkTearoffMenuItem *GetGtkTearoffMenuItem (Stack stack, int i); 

#endif 

#ifdef GtkTearoffMenuItem_Private 
static int init_gtktearoffmenuitem(NspGtkTearoffMenuItem *o,NspTypeGtkTearoffMenuItem *type);
static char *gtktearoffmenuitem_type_as_string(void);
static char *gtktearoffmenuitem_type_short_string(NspObject *v);
static AttrTab gtktearoffmenuitem_attrs[];
/* static int int_gtktearoffmenuitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktearoffmenuitem_get_methods(void); 
#endif /* GtkTearoffMenuItem_Private */
