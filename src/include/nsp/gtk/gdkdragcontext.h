/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkDragContext
#define INC_NSP_GdkDragContext

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkDragContext inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkDragContext ;
typedef NspTypeGObject NspTypeGdkDragContext ;

extern int nsp_type_gdkdragcontext_id;
extern NspTypeGdkDragContext *nsp_type_gdkdragcontext;

/* type instances for gobject */

NspTypeGdkDragContext *new_type_gdkdragcontext(type_mode mode);

/* instance for GdkDragContext */

NspGdkDragContext *new_gdkdragcontext();

/*
* Object methods redefined for gdkdragcontext 
*/

#ifdef GdkDragContext_Private 
static int init_gdkdragcontext(NspGdkDragContext *o,NspTypeGdkDragContext *type);
static char *gdkdragcontext_type_as_string(void);
static char *gdkdragcontext_type_short_string(void);
static AttrTab gdkdragcontext_attrs[];
/* static int int_gdkdragcontext_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkdragcontext_get_methods(void); 
#endif /* GdkDragContext_Private */

#define NULLGDKDRAGCONTEXT (NspGdkDragContext*) 0

NspGdkDragContext *gdkdragcontext_create(char *name,NspTypeBase *type);

/* from GdkDragContextObj.c */

extern NspGdkDragContext *gdkdragcontext_object (NspObject *O); 
extern int IsGdkDragContextObj (Stack stack, int i); 
extern int IsGdkDragContext(NspObject *O);
extern NspGdkDragContext *GetGdkDragContextCopy (Stack stack, int i); 
extern NspGdkDragContext *GetGdkDragContext (Stack stack, int i); 

#endif 
