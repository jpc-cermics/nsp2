/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeDragSource
#define INC_NSP_GtkTreeDragSource

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTreeDragSource inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTreeDragSource ;
typedef NspTypeGObject NspTypeGtkTreeDragSource ;

extern int nsp_type_gtktreedragsource_id;
extern NspTypeGtkTreeDragSource *nsp_type_gtktreedragsource;

/* type instances for gobject */

NspTypeGtkTreeDragSource *new_type_gtktreedragsource(type_mode mode);

/* instance for GtkTreeDragSource */

NspGtkTreeDragSource *new_gtktreedragsource();

/*
* Object methods redefined for gtktreedragsource 
*/

#define NULLGTKTREEDRAGSOURCE (NspGtkTreeDragSource*) 0

NspGtkTreeDragSource *gtktreedragsource_create(char *name,NspTypeBase *type);

/* from GtkTreeDragSourceObj.c */

extern NspGtkTreeDragSource *gtktreedragsource_object (NspObject *O); 
extern int IsGtkTreeDragSourceObj (Stack stack, int i); 
extern int IsGtkTreeDragSource(NspObject *O);
extern NspGtkTreeDragSource *GetGtkTreeDragSourceCopy (Stack stack, int i); 
extern NspGtkTreeDragSource *GetGtkTreeDragSource (Stack stack, int i); 

#endif 

#ifdef GtkTreeDragSource_Private 
static int init_gtktreedragsource(NspGtkTreeDragSource *o,NspTypeGtkTreeDragSource *type);
static char *gtktreedragsource_type_as_string(void);
static char *gtktreedragsource_type_short_string(NspObject *v);
static AttrTab gtktreedragsource_attrs[];
/* static int int_gtktreedragsource_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreedragsource_get_methods(void); 
#endif /* GtkTreeDragSource_Private */
