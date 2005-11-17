/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeDragDest
#define INC_NSP_GtkTreeDragDest

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTreeDragDest inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTreeDragDest ;
typedef NspTypeGObject NspTypeGtkTreeDragDest ;

extern int nsp_type_gtktreedragdest_id;
extern NspTypeGtkTreeDragDest *nsp_type_gtktreedragdest;

/* type instances for gobject */

NspTypeGtkTreeDragDest *new_type_gtktreedragdest(type_mode mode);

/* instance for GtkTreeDragDest */

NspGtkTreeDragDest *new_gtktreedragdest();

/*
* Object methods redefined for gtktreedragdest 
*/

#define NULLGTKTREEDRAGDEST (NspGtkTreeDragDest*) 0

NspGtkTreeDragDest *gtktreedragdest_create(char *name,NspTypeBase *type);

/* from GtkTreeDragDestObj.c */

extern NspGtkTreeDragDest *gtktreedragdest_object (NspObject *O); 
extern int IsGtkTreeDragDestObj (Stack stack, int i); 
extern int IsGtkTreeDragDest(NspObject *O);
extern NspGtkTreeDragDest *GetGtkTreeDragDestCopy (Stack stack, int i); 
extern NspGtkTreeDragDest *GetGtkTreeDragDest (Stack stack, int i); 

#endif 

#ifdef GtkTreeDragDest_Private 
static int init_gtktreedragdest(NspGtkTreeDragDest *o,NspTypeGtkTreeDragDest *type);
static char *gtktreedragdest_type_as_string(void);
static char *gtktreedragdest_type_short_string(void);
static AttrTab gtktreedragdest_attrs[];
/* static int int_gtktreedragdest_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreedragdest_get_methods(void); 
#endif /* GtkTreeDragDest_Private */
