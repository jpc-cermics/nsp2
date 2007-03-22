/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkListItem
#define INC_NSP_GtkListItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkitem.h"

/*
* NspGtkListItem inherits from NspGtkItem
* just change some type attributes 
*/

typedef NspGtkItem NspGtkListItem ;
typedef NspTypeGtkItem NspTypeGtkListItem ;

extern int nsp_type_gtklistitem_id;
extern NspTypeGtkListItem *nsp_type_gtklistitem;

/* type instances for gtkitem */

NspTypeGtkListItem *new_type_gtklistitem(type_mode mode);

/* instance for GtkListItem */

NspGtkListItem *new_gtklistitem();

/*
* Object methods redefined for gtklistitem 
*/

#ifdef GtkListItem_Private 
static int init_gtklistitem(NspGtkListItem *o,NspTypeGtkListItem *type);
static char *gtklistitem_type_as_string(void);
static char *gtklistitem_type_short_string(NspObject *v);
static AttrTab gtklistitem_attrs[];
/* static int int_gtklistitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtklistitem_get_methods(void); 
#endif /* GtkListItem_Private */

#define NULLGTKLISTITEM (NspGtkListItem*) 0

NspGtkListItem *gtklistitem_create(char *name,NspTypeBase *type);

/* from GtkListItemObj.c */

extern NspGtkListItem *gtklistitem_object (NspObject *O); 
extern int IsGtkListItemObj (Stack stack, int i); 
extern int IsGtkListItem(NspObject *O);
extern NspGtkListItem *GetGtkListItemCopy (Stack stack, int i); 
extern NspGtkListItem *GetGtkListItem (Stack stack, int i); 

#endif 
