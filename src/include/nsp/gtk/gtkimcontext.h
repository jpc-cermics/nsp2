/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkIMContext
#define INC_NSP_GtkIMContext

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkobject.h"

/*
* NspGtkIMContext inherits from NspGtkObject
* just change some type attributes 
*/

typedef NspGtkObject NspGtkIMContext ;
typedef NspTypeGtkObject NspTypeGtkIMContext ;

extern int nsp_type_gtkimcontext_id;
extern NspTypeGtkIMContext *nsp_type_gtkimcontext;

/* type instances for gtkobject */

NspTypeGtkIMContext *new_type_gtkimcontext(type_mode mode);

/* instance for GtkIMContext */

NspGtkIMContext *new_gtkimcontext();

/*
* Object methods redefined for gtkimcontext 
*/

#define NULLGTKIMCONTEXT (NspGtkIMContext*) 0

NspGtkIMContext *gtkimcontext_create(char *name,NspTypeBase *type);

/* from GtkIMContextObj.c */

extern NspGtkIMContext *gtkimcontext_object (NspObject *O); 
extern int IsGtkIMContextObj (Stack stack, int i); 
extern int IsGtkIMContext(NspObject *O);
extern NspGtkIMContext *GetGtkIMContextCopy (Stack stack, int i); 
extern NspGtkIMContext *GetGtkIMContext (Stack stack, int i); 

#endif 

#ifdef GtkIMContext_Private 
static int init_gtkimcontext(NspGtkIMContext *o,NspTypeGtkIMContext *type);
static char *gtkimcontext_type_as_string(void);
static char *gtkimcontext_type_short_string(void);
static AttrTab gtkimcontext_attrs[];
/* static int int_gtkimcontext_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkimcontext_get_methods(void); 
#endif /* GtkIMContext_Private */
