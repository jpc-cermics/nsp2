/* -*- Mode: C -*- */
#ifndef INC_NSP_WebKitWebBackForwardList
#define INC_NSP_WebKitWebBackForwardList

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspWebKitWebBackForwardList inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspWebKitWebBackForwardList ;
typedef NspTypeGObject NspTypeWebKitWebBackForwardList ;

extern int nsp_type_webkitwebbackforwardlist_id;
extern NspTypeWebKitWebBackForwardList *nsp_type_webkitwebbackforwardlist;

/* type instances for gobject */

NspTypeWebKitWebBackForwardList *new_type_webkitwebbackforwardlist(type_mode mode);

/* instance for WebKitWebBackForwardList */

NspWebKitWebBackForwardList *new_webkitwebbackforwardlist();

/*
* Object methods redefined for webkitwebbackforwardlist 
*/

#define NULLWEBKITWEBBACKFORWARDLIST (NspWebKitWebBackForwardList*) 0

NspWebKitWebBackForwardList *webkitwebbackforwardlist_create(char *name,NspTypeBase *type);

/* from WebKitWebBackForwardListObj.c */

extern NspWebKitWebBackForwardList *webkitwebbackforwardlist_object (NspObject *O); 
extern int IsWebKitWebBackForwardListObj (Stack stack, int i); 
extern int IsWebKitWebBackForwardList(NspObject *O);
extern NspWebKitWebBackForwardList *GetWebKitWebBackForwardListCopy (Stack stack, int i); 
extern NspWebKitWebBackForwardList *GetWebKitWebBackForwardList (Stack stack, int i); 

#endif 

#ifdef WebKitWebBackForwardList_Private 
static int init_webkitwebbackforwardlist(NspWebKitWebBackForwardList *o,NspTypeWebKitWebBackForwardList *type);
static char *webkitwebbackforwardlist_type_as_string(void);
static char *webkitwebbackforwardlist_type_short_string(NspObject *v);
static AttrTab webkitwebbackforwardlist_attrs[];
/* static int int_webkitwebbackforwardlist_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *webkitwebbackforwardlist_get_methods(void); 
#endif /* WebKitWebBackForwardList_Private */
