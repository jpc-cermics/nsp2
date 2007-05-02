/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoContext
#define INC_NSP_PangoContext

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspPangoContext inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspPangoContext ;
typedef NspTypeGObject NspTypePangoContext ;

extern int nsp_type_pangocontext_id;
extern NspTypePangoContext *nsp_type_pangocontext;

/* type instances for gobject */

NspTypePangoContext *new_type_pangocontext(type_mode mode);

/* instance for PangoContext */

NspPangoContext *new_pangocontext();

/*
* Object methods redefined for pangocontext 
*/

#define NULLPANGOCONTEXT (NspPangoContext*) 0

NspPangoContext *pangocontext_create(char *name,NspTypeBase *type);

/* from PangoContextObj.c */

extern NspPangoContext *pangocontext_object (NspObject *O); 
extern int IsPangoContextObj (Stack stack, int i); 
extern int IsPangoContext(NspObject *O);
extern NspPangoContext *GetPangoContextCopy (Stack stack, int i); 
extern NspPangoContext *GetPangoContext (Stack stack, int i); 

#endif 

#ifdef PangoContext_Private 
static int init_pangocontext(NspPangoContext *o,NspTypePangoContext *type);
static char *pangocontext_type_as_string(void);
static char *pangocontext_type_short_string(NspObject *v);
static AttrTab pangocontext_attrs[];
/* static int int_pangocontext_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangocontext_get_methods(void); 
#endif /* PangoContext_Private */
