/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeSortable
#define INC_NSP_GtkTreeSortable

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTreeSortable inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTreeSortable ;
typedef NspTypeGObject NspTypeGtkTreeSortable ;

extern int nsp_type_gtktreesortable_id;
extern NspTypeGtkTreeSortable *nsp_type_gtktreesortable;

/* type instances for gobject */

NspTypeGtkTreeSortable *new_type_gtktreesortable(type_mode mode);

/* instance for GtkTreeSortable */

NspGtkTreeSortable *new_gtktreesortable();

/*
* Object methods redefined for gtktreesortable 
*/

#ifdef GtkTreeSortable_Private 
static int init_gtktreesortable(NspGtkTreeSortable *o,NspTypeGtkTreeSortable *type);
static char *gtktreesortable_type_as_string(void);
static char *gtktreesortable_type_short_string(void);
static AttrTab gtktreesortable_attrs[];
/* static int int_gtktreesortable_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreesortable_get_methods(void); 
#endif /* GtkTreeSortable_Private */

#define NULLGTKTREESORTABLE (NspGtkTreeSortable*) 0

NspGtkTreeSortable *gtktreesortable_create(char *name,NspTypeBase *type);

/* from GtkTreeSortableObj.c */

extern NspGtkTreeSortable *gtktreesortable_object (NspObject *O); 
extern int IsGtkTreeSortableObj (Stack stack, int i); 
extern int IsGtkTreeSortable(NspObject *O);
extern NspGtkTreeSortable *GetGtkTreeSortableCopy (Stack stack, int i); 
extern NspGtkTreeSortable *GetGtkTreeSortable (Stack stack, int i); 

#endif 
