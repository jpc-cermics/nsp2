/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkItem
#define INC_NSP_GtkItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkItem inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkItem ;
typedef NspTypeGtkBin NspTypeGtkItem ;

extern int nsp_type_gtkitem_id;
extern NspTypeGtkItem *nsp_type_gtkitem;

/* type instances for gtkbin */

NspTypeGtkItem *new_type_gtkitem(type_mode mode);

/* instance for GtkItem */

NspGtkItem *new_gtkitem();

/*
* Object methods redefined for gtkitem 
*/

#ifdef GtkItem_Private 
static int init_gtkitem(NspGtkItem *o,NspTypeGtkItem *type);
static char *gtkitem_type_as_string(void);
static char *gtkitem_type_short_string(void);
static AttrTab gtkitem_attrs[];
/* static int int_gtkitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkitem_get_methods(void); 
#endif /* GtkItem_Private */

#define NULLGTKITEM (NspGtkItem*) 0

NspGtkItem *gtkitem_create(char *name,NspTypeBase *type);

/* from GtkItemObj.c */

extern NspGtkItem *gtkitem_object (NspObject *O); 
extern int IsGtkItemObj (Stack stack, int i); 
extern int IsGtkItem(NspObject *O);
extern NspGtkItem *GetGtkItemCopy (Stack stack, int i); 
extern NspGtkItem *GetGtkItem (Stack stack, int i); 

#endif 
