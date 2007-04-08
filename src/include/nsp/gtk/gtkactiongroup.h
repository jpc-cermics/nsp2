/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkActionGroup
#define INC_NSP_GtkActionGroup

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkActionGroup inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkActionGroup ;
typedef NspTypeGObject NspTypeGtkActionGroup ;

extern int nsp_type_gtkactiongroup_id;
extern NspTypeGtkActionGroup *nsp_type_gtkactiongroup;

/* type instances for gobject */

NspTypeGtkActionGroup *new_type_gtkactiongroup(type_mode mode);

/* instance for GtkActionGroup */

NspGtkActionGroup *new_gtkactiongroup();

/*
* Object methods redefined for gtkactiongroup 
*/

#define NULLGTKACTIONGROUP (NspGtkActionGroup*) 0

NspGtkActionGroup *gtkactiongroup_create(char *name,NspTypeBase *type);

/* from GtkActionGroupObj.c */

extern NspGtkActionGroup *gtkactiongroup_object (NspObject *O); 
extern int IsGtkActionGroupObj (Stack stack, int i); 
extern int IsGtkActionGroup(NspObject *O);
extern NspGtkActionGroup *GetGtkActionGroupCopy (Stack stack, int i); 
extern NspGtkActionGroup *GetGtkActionGroup (Stack stack, int i); 

#endif 

#ifdef GtkActionGroup_Private 
static int init_gtkactiongroup(NspGtkActionGroup *o,NspTypeGtkActionGroup *type);
static char *gtkactiongroup_type_as_string(void);
static char *gtkactiongroup_type_short_string(NspObject *v);
static AttrTab gtkactiongroup_attrs[];
/* static int int_gtkactiongroup_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkactiongroup_get_methods(void); 
#endif /* GtkActionGroup_Private */
