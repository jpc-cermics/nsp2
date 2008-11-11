/* -*- Mode: C -*- */
#ifndef INC_NSP_WebKitNetworkRequest
#define INC_NSP_WebKitNetworkRequest

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspWebKitNetworkRequest inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspWebKitNetworkRequest ;
typedef NspTypeGObject NspTypeWebKitNetworkRequest ;

extern int nsp_type_webkitnetworkrequest_id;
extern NspTypeWebKitNetworkRequest *nsp_type_webkitnetworkrequest;

/* type instances for gobject */

NspTypeWebKitNetworkRequest *new_type_webkitnetworkrequest(type_mode mode);

/* instance for WebKitNetworkRequest */

NspWebKitNetworkRequest *new_webkitnetworkrequest();

/*
* Object methods redefined for webkitnetworkrequest 
*/

#define NULLWEBKITNETWORKREQUEST (NspWebKitNetworkRequest*) 0

NspWebKitNetworkRequest *webkitnetworkrequest_create(char *name,NspTypeBase *type);

/* from WebKitNetworkRequestObj.c */

extern NspWebKitNetworkRequest *webkitnetworkrequest_object (NspObject *O); 
extern int IsWebKitNetworkRequestObj (Stack stack, int i); 
extern int IsWebKitNetworkRequest(NspObject *O);
extern NspWebKitNetworkRequest *GetWebKitNetworkRequestCopy (Stack stack, int i); 
extern NspWebKitNetworkRequest *GetWebKitNetworkRequest (Stack stack, int i); 

#endif 

#ifdef WebKitNetworkRequest_Private 
static int init_webkitnetworkrequest(NspWebKitNetworkRequest *o,NspTypeWebKitNetworkRequest *type);
static char *webkitnetworkrequest_type_as_string(void);
static char *webkitnetworkrequest_type_short_string(NspObject *v);
static AttrTab webkitnetworkrequest_attrs[];
/* static int int_webkitnetworkrequest_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *webkitnetworkrequest_get_methods(void); 
#endif /* WebKitNetworkRequest_Private */
