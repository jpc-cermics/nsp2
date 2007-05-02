/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkUIManager
#define INC_NSP_GtkUIManager

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkUIManager inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkUIManager ;
typedef NspTypeGObject NspTypeGtkUIManager ;

extern int nsp_type_gtkuimanager_id;
extern NspTypeGtkUIManager *nsp_type_gtkuimanager;

/* type instances for gobject */

NspTypeGtkUIManager *new_type_gtkuimanager(type_mode mode);

/* instance for GtkUIManager */

NspGtkUIManager *new_gtkuimanager();

/*
* Object methods redefined for gtkuimanager 
*/

#define NULLGTKUIMANAGER (NspGtkUIManager*) 0

NspGtkUIManager *gtkuimanager_create(char *name,NspTypeBase *type);

/* from GtkUIManagerObj.c */

extern NspGtkUIManager *gtkuimanager_object (NspObject *O); 
extern int IsGtkUIManagerObj (Stack stack, int i); 
extern int IsGtkUIManager(NspObject *O);
extern NspGtkUIManager *GetGtkUIManagerCopy (Stack stack, int i); 
extern NspGtkUIManager *GetGtkUIManager (Stack stack, int i); 

#endif 

#ifdef GtkUIManager_Private 
static int init_gtkuimanager(NspGtkUIManager *o,NspTypeGtkUIManager *type);
static char *gtkuimanager_type_as_string(void);
static char *gtkuimanager_type_short_string(NspObject *v);
static AttrTab gtkuimanager_attrs[];
/* static int int_gtkuimanager_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkuimanager_get_methods(void); 
#endif /* GtkUIManager_Private */
