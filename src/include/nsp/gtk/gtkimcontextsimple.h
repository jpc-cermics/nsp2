/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkIMContextSimple
#define INC_NSP_GtkIMContextSimple

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkimcontext.h"

/*
* NspGtkIMContextSimple inherits from NspGtkIMContext
* just change some type attributes 
*/

typedef NspGtkIMContext NspGtkIMContextSimple ;
typedef NspTypeGtkIMContext NspTypeGtkIMContextSimple ;

extern int nsp_type_gtkimcontextsimple_id;
extern NspTypeGtkIMContextSimple *nsp_type_gtkimcontextsimple;

/* type instances for gtkimcontext */

NspTypeGtkIMContextSimple *new_type_gtkimcontextsimple(type_mode mode);

/* instance for GtkIMContextSimple */

NspGtkIMContextSimple *new_gtkimcontextsimple();

/*
* Object methods redefined for gtkimcontextsimple 
*/

#define NULLGTKIMCONTEXTSIMPLE (NspGtkIMContextSimple*) 0

NspGtkIMContextSimple *gtkimcontextsimple_create(char *name,NspTypeBase *type);

/* from GtkIMContextSimpleObj.c */

extern NspGtkIMContextSimple *gtkimcontextsimple_object (NspObject *O); 
extern int IsGtkIMContextSimpleObj (Stack stack, int i); 
extern int IsGtkIMContextSimple(NspObject *O);
extern NspGtkIMContextSimple *GetGtkIMContextSimpleCopy (Stack stack, int i); 
extern NspGtkIMContextSimple *GetGtkIMContextSimple (Stack stack, int i); 

#endif 

#ifdef GtkIMContextSimple_Private 
static int init_gtkimcontextsimple(NspGtkIMContextSimple *o,NspTypeGtkIMContextSimple *type);
static char *gtkimcontextsimple_type_as_string(void);
static char *gtkimcontextsimple_type_short_string(void);
static AttrTab gtkimcontextsimple_attrs[];
/* static int int_gtkimcontextsimple_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkimcontextsimple_get_methods(void); 
#endif /* GtkIMContextSimple_Private */
