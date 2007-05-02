/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeStore
#define INC_NSP_GtkTreeStore

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTreeStore inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTreeStore ;
typedef NspTypeGObject NspTypeGtkTreeStore ;

extern int nsp_type_gtktreestore_id;
extern NspTypeGtkTreeStore *nsp_type_gtktreestore;

/* type instances for gobject */

NspTypeGtkTreeStore *new_type_gtktreestore(type_mode mode);

/* instance for GtkTreeStore */

NspGtkTreeStore *new_gtktreestore();

/*
* Object methods redefined for gtktreestore 
*/

#define NULLGTKTREESTORE (NspGtkTreeStore*) 0

NspGtkTreeStore *gtktreestore_create(char *name,NspTypeBase *type);

/* from GtkTreeStoreObj.c */

extern NspGtkTreeStore *gtktreestore_object (NspObject *O); 
extern int IsGtkTreeStoreObj (Stack stack, int i); 
extern int IsGtkTreeStore(NspObject *O);
extern NspGtkTreeStore *GetGtkTreeStoreCopy (Stack stack, int i); 
extern NspGtkTreeStore *GetGtkTreeStore (Stack stack, int i); 

#endif 

#ifdef GtkTreeStore_Private 
static int init_gtktreestore(NspGtkTreeStore *o,NspTypeGtkTreeStore *type);
static char *gtktreestore_type_as_string(void);
static char *gtktreestore_type_short_string(NspObject *v);
static AttrTab gtktreestore_attrs[];
/* static int int_gtktreestore_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreestore_get_methods(void); 
#endif /* GtkTreeStore_Private */
