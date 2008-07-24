/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkAccelMap
#define INC_NSP_GtkAccelMap

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkAccelMap inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkAccelMap ;
typedef NspTypeGObject NspTypeGtkAccelMap ;

extern int nsp_type_gtkaccelmap_id;
extern NspTypeGtkAccelMap *nsp_type_gtkaccelmap;

/* type instances for gobject */

NspTypeGtkAccelMap *new_type_gtkaccelmap(type_mode mode);

/* instance for GtkAccelMap */

NspGtkAccelMap *new_gtkaccelmap();

/*
* Object methods redefined for gtkaccelmap 
*/

#define NULLGTKACCELMAP (NspGtkAccelMap*) 0

NspGtkAccelMap *gtkaccelmap_create(char *name,NspTypeBase *type);

/* from GtkAccelMapObj.c */

extern NspGtkAccelMap *gtkaccelmap_object (NspObject *O); 
extern int IsGtkAccelMapObj (Stack stack, int i); 
extern int IsGtkAccelMap(NspObject *O);
extern NspGtkAccelMap *GetGtkAccelMapCopy (Stack stack, int i); 
extern NspGtkAccelMap *GetGtkAccelMap (Stack stack, int i); 

#endif 

#ifdef GtkAccelMap_Private 
static int init_gtkaccelmap(NspGtkAccelMap *o,NspTypeGtkAccelMap *type);
static char *gtkaccelmap_type_as_string(void);
static char *gtkaccelmap_type_short_string(NspObject *v);
static AttrTab gtkaccelmap_attrs[];
/* static int int_gtkaccelmap_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkaccelmap_get_methods(void); 
#endif /* GtkAccelMap_Private */
