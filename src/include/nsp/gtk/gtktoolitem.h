/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkToolItem
#define INC_NSP_GtkToolItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkToolItem inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkToolItem ;
typedef NspTypeGtkBin NspTypeGtkToolItem ;

extern int nsp_type_gtktoolitem_id;
extern NspTypeGtkToolItem *nsp_type_gtktoolitem;

/* type instances for gtkbin */

NspTypeGtkToolItem *new_type_gtktoolitem(type_mode mode);

/* instance for GtkToolItem */

NspGtkToolItem *new_gtktoolitem();

/*
* Object methods redefined for gtktoolitem 
*/

#define NULLGTKTOOLITEM (NspGtkToolItem*) 0

NspGtkToolItem *gtktoolitem_create(char *name,NspTypeBase *type);

/* from GtkToolItemObj.c */

extern NspGtkToolItem *gtktoolitem_object (NspObject *O); 
extern int IsGtkToolItemObj (Stack stack, int i); 
extern int IsGtkToolItem(NspObject *O);
extern NspGtkToolItem *GetGtkToolItemCopy (Stack stack, int i); 
extern NspGtkToolItem *GetGtkToolItem (Stack stack, int i); 

#endif 

#ifdef GtkToolItem_Private 
static int init_gtktoolitem(NspGtkToolItem *o,NspTypeGtkToolItem *type);
static char *gtktoolitem_type_as_string(void);
static char *gtktoolitem_type_short_string(NspObject *v);
static AttrTab gtktoolitem_attrs[];
/* static int int_gtktoolitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktoolitem_get_methods(void); 
#endif /* GtkToolItem_Private */
