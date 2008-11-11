/* -*- Mode: C -*- */
#ifndef INC_NSP_WebKitWebFrame
#define INC_NSP_WebKitWebFrame

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspWebKitWebFrame inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspWebKitWebFrame ;
typedef NspTypeGObject NspTypeWebKitWebFrame ;

extern int nsp_type_webkitwebframe_id;
extern NspTypeWebKitWebFrame *nsp_type_webkitwebframe;

/* type instances for gobject */

NspTypeWebKitWebFrame *new_type_webkitwebframe(type_mode mode);

/* instance for WebKitWebFrame */

NspWebKitWebFrame *new_webkitwebframe();

/*
* Object methods redefined for webkitwebframe 
*/

#define NULLWEBKITWEBFRAME (NspWebKitWebFrame*) 0

NspWebKitWebFrame *webkitwebframe_create(char *name,NspTypeBase *type);

/* from WebKitWebFrameObj.c */

extern NspWebKitWebFrame *webkitwebframe_object (NspObject *O); 
extern int IsWebKitWebFrameObj (Stack stack, int i); 
extern int IsWebKitWebFrame(NspObject *O);
extern NspWebKitWebFrame *GetWebKitWebFrameCopy (Stack stack, int i); 
extern NspWebKitWebFrame *GetWebKitWebFrame (Stack stack, int i); 

#endif 

#ifdef WebKitWebFrame_Private 
static int init_webkitwebframe(NspWebKitWebFrame *o,NspTypeWebKitWebFrame *type);
static char *webkitwebframe_type_as_string(void);
static char *webkitwebframe_type_short_string(NspObject *v);
static AttrTab webkitwebframe_attrs[];
/* static int int_webkitwebframe_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *webkitwebframe_get_methods(void); 
#endif /* WebKitWebFrame_Private */
