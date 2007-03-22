/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkEntry
#define INC_NSP_GtkEntry

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkEntry inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkEntry ;
typedef NspTypeGtkWidget NspTypeGtkEntry ;

extern int nsp_type_gtkentry_id;
extern NspTypeGtkEntry *nsp_type_gtkentry;

/* type instances for gtkwidget */

NspTypeGtkEntry *new_type_gtkentry(type_mode mode);

/* instance for GtkEntry */

NspGtkEntry *new_gtkentry();

/*
* Object methods redefined for gtkentry 
*/

#define NULLGTKENTRY (NspGtkEntry*) 0

NspGtkEntry *gtkentry_create(char *name,NspTypeBase *type);

/* from GtkEntryObj.c */

extern NspGtkEntry *gtkentry_object (NspObject *O); 
extern int IsGtkEntryObj (Stack stack, int i); 
extern int IsGtkEntry(NspObject *O);
extern NspGtkEntry *GetGtkEntryCopy (Stack stack, int i); 
extern NspGtkEntry *GetGtkEntry (Stack stack, int i); 

#endif 

#ifdef GtkEntry_Private 
static int init_gtkentry(NspGtkEntry *o,NspTypeGtkEntry *type);
static char *gtkentry_type_as_string(void);
static char *gtkentry_type_short_string(NspObject *v);
static AttrTab gtkentry_attrs[];
/* static int int_gtkentry_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkentry_get_methods(void); 
#endif /* GtkEntry_Private */
