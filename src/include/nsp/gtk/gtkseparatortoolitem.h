/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkSeparatorToolItem
#define INC_NSP_GtkSeparatorToolItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtktoolitem.h"

/*
* NspGtkSeparatorToolItem inherits from NspGtkToolItem
* just change some type attributes 
*/

typedef NspGtkToolItem NspGtkSeparatorToolItem ;
typedef NspTypeGtkToolItem NspTypeGtkSeparatorToolItem ;

extern int nsp_type_gtkseparatortoolitem_id;
extern NspTypeGtkSeparatorToolItem *nsp_type_gtkseparatortoolitem;

/* type instances for gtktoolitem */

NspTypeGtkSeparatorToolItem *new_type_gtkseparatortoolitem(type_mode mode);

/* instance for GtkSeparatorToolItem */

NspGtkSeparatorToolItem *new_gtkseparatortoolitem();

/*
* Object methods redefined for gtkseparatortoolitem 
*/

#define NULLGTKSEPARATORTOOLITEM (NspGtkSeparatorToolItem*) 0

NspGtkSeparatorToolItem *gtkseparatortoolitem_create(char *name,NspTypeBase *type);

/* from GtkSeparatorToolItemObj.c */

extern NspGtkSeparatorToolItem *gtkseparatortoolitem_object (NspObject *O); 
extern int IsGtkSeparatorToolItemObj (Stack stack, int i); 
extern int IsGtkSeparatorToolItem(NspObject *O);
extern NspGtkSeparatorToolItem *GetGtkSeparatorToolItemCopy (Stack stack, int i); 
extern NspGtkSeparatorToolItem *GetGtkSeparatorToolItem (Stack stack, int i); 

#endif 

#ifdef GtkSeparatorToolItem_Private 
static int init_gtkseparatortoolitem(NspGtkSeparatorToolItem *o,NspTypeGtkSeparatorToolItem *type);
static char *gtkseparatortoolitem_type_as_string(void);
static char *gtkseparatortoolitem_type_short_string(NspObject *v);
static AttrTab gtkseparatortoolitem_attrs[];
/* static int int_gtkseparatortoolitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkseparatortoolitem_get_methods(void); 
#endif /* GtkSeparatorToolItem_Private */
