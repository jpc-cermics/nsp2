/* -*- Mode: C -*- */
#ifndef INC_NSP_WebKitWebHistoryItem
#define INC_NSP_WebKitWebHistoryItem

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspWebKitWebHistoryItem inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspWebKitWebHistoryItem ;
typedef NspTypeGObject NspTypeWebKitWebHistoryItem ;

extern int nsp_type_webkitwebhistoryitem_id;
extern NspTypeWebKitWebHistoryItem *nsp_type_webkitwebhistoryitem;

/* type instances for gobject */

NspTypeWebKitWebHistoryItem *new_type_webkitwebhistoryitem(type_mode mode);

/* instance for WebKitWebHistoryItem */

NspWebKitWebHistoryItem *new_webkitwebhistoryitem();

/*
* Object methods redefined for webkitwebhistoryitem 
*/

#define NULLWEBKITWEBHISTORYITEM (NspWebKitWebHistoryItem*) 0

NspWebKitWebHistoryItem *webkitwebhistoryitem_create(char *name,NspTypeBase *type);

/* from WebKitWebHistoryItemObj.c */

extern NspWebKitWebHistoryItem *webkitwebhistoryitem_object (NspObject *O); 
extern int IsWebKitWebHistoryItemObj (Stack stack, int i); 
extern int IsWebKitWebHistoryItem(NspObject *O);
extern NspWebKitWebHistoryItem *GetWebKitWebHistoryItemCopy (Stack stack, int i); 
extern NspWebKitWebHistoryItem *GetWebKitWebHistoryItem (Stack stack, int i); 

#endif 

#ifdef WebKitWebHistoryItem_Private 
static int init_webkitwebhistoryitem(NspWebKitWebHistoryItem *o,NspTypeWebKitWebHistoryItem *type);
static char *webkitwebhistoryitem_type_as_string(void);
static char *webkitwebhistoryitem_type_short_string(NspObject *v);
static AttrTab webkitwebhistoryitem_attrs[];
/* static int int_webkitwebhistoryitem_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *webkitwebhistoryitem_get_methods(void); 
#endif /* WebKitWebHistoryItem_Private */
