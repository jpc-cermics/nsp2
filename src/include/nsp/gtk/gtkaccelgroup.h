/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkAccelGroup
#define INC_NSP_GtkAccelGroup

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkAccelGroup inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkAccelGroup ;
typedef NspTypeGObject NspTypeGtkAccelGroup ;

extern int nsp_type_gtkaccelgroup_id;
extern NspTypeGtkAccelGroup *nsp_type_gtkaccelgroup;

/* type instances for gobject */

NspTypeGtkAccelGroup *new_type_gtkaccelgroup(type_mode mode);

/* instance for GtkAccelGroup */

NspGtkAccelGroup *new_gtkaccelgroup();

/*
* Object methods redefined for gtkaccelgroup 
*/

#ifdef GtkAccelGroup_Private 
static int init_gtkaccelgroup(NspGtkAccelGroup *o,NspTypeGtkAccelGroup *type);
static char *gtkaccelgroup_type_as_string(void);
static char *gtkaccelgroup_type_short_string(void);
static AttrTab gtkaccelgroup_attrs[];
/* static int int_gtkaccelgroup_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkaccelgroup_get_methods(void); 
#endif /* GtkAccelGroup_Private */

#define NULLGTKACCELGROUP (NspGtkAccelGroup*) 0

NspGtkAccelGroup *gtkaccelgroup_create(char *name,NspTypeBase *type);

/* from GtkAccelGroupObj.c */

extern NspGtkAccelGroup *gtkaccelgroup_object (NspObject *O); 
extern int IsGtkAccelGroupObj (Stack stack, int i); 
extern int IsGtkAccelGroup(NspObject *O);
extern NspGtkAccelGroup *GetGtkAccelGroupCopy (Stack stack, int i); 
extern NspGtkAccelGroup *GetGtkAccelGroup (Stack stack, int i); 

#endif 
