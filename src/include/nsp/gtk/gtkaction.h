/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkAction
#define INC_NSP_GtkAction

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkAction inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkAction ;
typedef NspTypeGObject NspTypeGtkAction ;

extern int nsp_type_gtkaction_id;
extern NspTypeGtkAction *nsp_type_gtkaction;

/* type instances for gobject */

NspTypeGtkAction *new_type_gtkaction(type_mode mode);

/* instance for GtkAction */

NspGtkAction *new_gtkaction();

/*
* Object methods redefined for gtkaction 
*/

#define NULLGTKACTION (NspGtkAction*) 0

NspGtkAction *gtkaction_create(char *name,NspTypeBase *type);

/* from GtkActionObj.c */

extern NspGtkAction *gtkaction_object (NspObject *O); 
extern int IsGtkActionObj (Stack stack, int i); 
extern int IsGtkAction(NspObject *O);
extern NspGtkAction *GetGtkActionCopy (Stack stack, int i); 
extern NspGtkAction *GetGtkAction (Stack stack, int i); 

#endif 

#ifdef GtkAction_Private 
static int init_gtkaction(NspGtkAction *o,NspTypeGtkAction *type);
static char *gtkaction_type_as_string(void);
static char *gtkaction_type_short_string(NspObject *v);
static AttrTab gtkaction_attrs[];
/* static int int_gtkaction_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkaction_get_methods(void); 
#endif /* GtkAction_Private */
