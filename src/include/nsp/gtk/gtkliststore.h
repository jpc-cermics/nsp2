/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkListStore
#define INC_NSP_GtkListStore

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkListStore inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkListStore ;
typedef NspTypeGObject NspTypeGtkListStore ;

extern int nsp_type_gtkliststore_id;
extern NspTypeGtkListStore *nsp_type_gtkliststore;

/* type instances for gobject */

NspTypeGtkListStore *new_type_gtkliststore(type_mode mode);

/* instance for GtkListStore */

NspGtkListStore *new_gtkliststore();

/*
* Object methods redefined for gtkliststore 
*/

#ifdef GtkListStore_Private 
static int init_gtkliststore(NspGtkListStore *o,NspTypeGtkListStore *type);
static char *gtkliststore_type_as_string(void);
static char *gtkliststore_type_short_string(void);
static AttrTab gtkliststore_attrs[];
/* static int int_gtkliststore_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkliststore_get_methods(void); 
#endif /* GtkListStore_Private */

#define NULLGTKLISTSTORE (NspGtkListStore*) 0

NspGtkListStore *gtkliststore_create(char *name,NspTypeBase *type);

/* from GtkListStoreObj.c */

extern NspGtkListStore *gtkliststore_object (NspObject *O); 
extern int IsGtkListStoreObj (Stack stack, int i); 
extern int IsGtkListStore(NspObject *O);
extern NspGtkListStore *GetGtkListStoreCopy (Stack stack, int i); 
extern NspGtkListStore *GetGtkListStore (Stack stack, int i); 

#endif 
