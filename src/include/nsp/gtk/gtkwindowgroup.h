/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkWindowGroup
#define INC_NSP_GtkWindowGroup

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkWindowGroup inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkWindowGroup ;
typedef NspTypeGObject NspTypeGtkWindowGroup ;

extern int nsp_type_gtkwindowgroup_id;
extern NspTypeGtkWindowGroup *nsp_type_gtkwindowgroup;

/* type instances for gobject */

NspTypeGtkWindowGroup *new_type_gtkwindowgroup(type_mode mode);

/* instance for GtkWindowGroup */

NspGtkWindowGroup *new_gtkwindowgroup();

/*
* Object methods redefined for gtkwindowgroup 
*/

#define NULLGTKWINDOWGROUP (NspGtkWindowGroup*) 0

NspGtkWindowGroup *gtkwindowgroup_create(char *name,NspTypeBase *type);

/* from GtkWindowGroupObj.c */

extern NspGtkWindowGroup *gtkwindowgroup_object (NspObject *O); 
extern int IsGtkWindowGroupObj (Stack stack, int i); 
extern int IsGtkWindowGroup(NspObject *O);
extern NspGtkWindowGroup *GetGtkWindowGroupCopy (Stack stack, int i); 
extern NspGtkWindowGroup *GetGtkWindowGroup (Stack stack, int i); 

#endif 

#ifdef GtkWindowGroup_Private 
static int init_gtkwindowgroup(NspGtkWindowGroup *o,NspTypeGtkWindowGroup *type);
static char *gtkwindowgroup_type_as_string(void);
static char *gtkwindowgroup_type_short_string(NspObject *v);
static AttrTab gtkwindowgroup_attrs[];
/* static int int_gtkwindowgroup_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkwindowgroup_get_methods(void); 
#endif /* GtkWindowGroup_Private */
