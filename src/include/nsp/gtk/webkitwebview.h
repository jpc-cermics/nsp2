/* -*- Mode: C -*- */
#ifndef INC_NSP_WebKitWebView
#define INC_NSP_WebKitWebView

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspWebKitWebView inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspWebKitWebView ;
typedef NspTypeGtkContainer NspTypeWebKitWebView ;

extern int nsp_type_webkitwebview_id;
extern NspTypeWebKitWebView *nsp_type_webkitwebview;

/* type instances for gtkcontainer */

NspTypeWebKitWebView *new_type_webkitwebview(type_mode mode);

/* instance for WebKitWebView */

NspWebKitWebView *new_webkitwebview();

/*
* Object methods redefined for webkitwebview 
*/

#define NULLWEBKITWEBVIEW (NspWebKitWebView*) 0

NspWebKitWebView *webkitwebview_create(char *name,NspTypeBase *type);

/* from WebKitWebViewObj.c */

extern NspWebKitWebView *webkitwebview_object (NspObject *O); 
extern int IsWebKitWebViewObj (Stack stack, int i); 
extern int IsWebKitWebView(NspObject *O);
extern NspWebKitWebView *GetWebKitWebViewCopy (Stack stack, int i); 
extern NspWebKitWebView *GetWebKitWebView (Stack stack, int i); 

#endif 

#ifdef WebKitWebView_Private 
static int init_webkitwebview(NspWebKitWebView *o,NspTypeWebKitWebView *type);
static char *webkitwebview_type_as_string(void);
static char *webkitwebview_type_short_string(NspObject *v);
static AttrTab webkitwebview_attrs[];
/* static int int_webkitwebview_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *webkitwebview_get_methods(void); 
#endif /* WebKitWebView_Private */
