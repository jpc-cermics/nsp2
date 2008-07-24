/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkEntryCompletion
#define INC_NSP_GtkEntryCompletion

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkEntryCompletion inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkEntryCompletion ;
typedef NspTypeGObject NspTypeGtkEntryCompletion ;

extern int nsp_type_gtkentrycompletion_id;
extern NspTypeGtkEntryCompletion *nsp_type_gtkentrycompletion;

/* type instances for gobject */

NspTypeGtkEntryCompletion *new_type_gtkentrycompletion(type_mode mode);

/* instance for GtkEntryCompletion */

NspGtkEntryCompletion *new_gtkentrycompletion();

/*
* Object methods redefined for gtkentrycompletion 
*/

#define NULLGTKENTRYCOMPLETION (NspGtkEntryCompletion*) 0

NspGtkEntryCompletion *gtkentrycompletion_create(char *name,NspTypeBase *type);

/* from GtkEntryCompletionObj.c */

extern NspGtkEntryCompletion *gtkentrycompletion_object (NspObject *O); 
extern int IsGtkEntryCompletionObj (Stack stack, int i); 
extern int IsGtkEntryCompletion(NspObject *O);
extern NspGtkEntryCompletion *GetGtkEntryCompletionCopy (Stack stack, int i); 
extern NspGtkEntryCompletion *GetGtkEntryCompletion (Stack stack, int i); 

#endif 

#ifdef GtkEntryCompletion_Private 
static int init_gtkentrycompletion(NspGtkEntryCompletion *o,NspTypeGtkEntryCompletion *type);
static char *gtkentrycompletion_type_as_string(void);
static char *gtkentrycompletion_type_short_string(NspObject *v);
static AttrTab gtkentrycompletion_attrs[];
/* static int int_gtkentrycompletion_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkentrycompletion_get_methods(void); 
#endif /* GtkEntryCompletion_Private */
