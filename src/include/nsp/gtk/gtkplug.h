/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkPlug
#define INC_NSP_GtkPlug

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwindow.h"

/*
* NspGtkPlug inherits from NspGtkWindow
* just change some type attributes 
*/

typedef NspGtkWindow NspGtkPlug ;
typedef NspTypeGtkWindow NspTypeGtkPlug ;

extern int nsp_type_gtkplug_id;
extern NspTypeGtkPlug *nsp_type_gtkplug;

/* type instances for gtkwindow */

NspTypeGtkPlug *new_type_gtkplug(type_mode mode);

/* instance for GtkPlug */

NspGtkPlug *new_gtkplug();

/*
* Object methods redefined for gtkplug 
*/

#ifdef GtkPlug_Private 
static int init_gtkplug(NspGtkPlug *o,NspTypeGtkPlug *type);
static char *gtkplug_type_as_string(void);
static char *gtkplug_type_short_string(void);
static AttrTab gtkplug_attrs[];
/* static int int_gtkplug_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkplug_get_methods(void); 
#endif /* GtkPlug_Private */

#define NULLGTKPLUG (NspGtkPlug*) 0

NspGtkPlug *gtkplug_create(char *name,NspTypeBase *type);

/* from GtkPlugObj.c */

extern NspGtkPlug *gtkplug_object (NspObject *O); 
extern int IsGtkPlugObj (Stack stack, int i); 
extern int IsGtkPlug(NspObject *O);
extern NspGtkPlug *GetGtkPlugCopy (Stack stack, int i); 
extern NspGtkPlug *GetGtkPlug (Stack stack, int i); 

#endif 
