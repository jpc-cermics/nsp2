/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkDisplayManager
#define INC_NSP_GdkDisplayManager

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkDisplayManager inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkDisplayManager ;
typedef NspTypeGObject NspTypeGdkDisplayManager ;

extern int nsp_type_gdkdisplaymanager_id;
extern NspTypeGdkDisplayManager *nsp_type_gdkdisplaymanager;

/* type instances for gobject */

NspTypeGdkDisplayManager *new_type_gdkdisplaymanager(type_mode mode);

/* instance for GdkDisplayManager */

NspGdkDisplayManager *new_gdkdisplaymanager();

/*
* Object methods redefined for gdkdisplaymanager 
*/

#define NULLGDKDISPLAYMANAGER (NspGdkDisplayManager*) 0

NspGdkDisplayManager *gdkdisplaymanager_create(char *name,NspTypeBase *type);

/* from GdkDisplayManagerObj.c */

extern NspGdkDisplayManager *gdkdisplaymanager_object (NspObject *O); 
extern int IsGdkDisplayManagerObj (Stack stack, int i); 
extern int IsGdkDisplayManager(NspObject *O);
extern NspGdkDisplayManager *GetGdkDisplayManagerCopy (Stack stack, int i); 
extern NspGdkDisplayManager *GetGdkDisplayManager (Stack stack, int i); 

#endif 

#ifdef GdkDisplayManager_Private 
static int init_gdkdisplaymanager(NspGdkDisplayManager *o,NspTypeGdkDisplayManager *type);
static char *gdkdisplaymanager_type_as_string(void);
static char *gdkdisplaymanager_type_short_string(NspObject *v);
static AttrTab gdkdisplaymanager_attrs[];
/* static int int_gdkdisplaymanager_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkdisplaymanager_get_methods(void); 
#endif /* GdkDisplayManager_Private */
