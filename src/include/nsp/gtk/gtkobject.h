/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkObject
#define INC_NSP_GtkObject

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkObject inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkObject ;
typedef NspTypeGObject NspTypeGtkObject ;

extern int nsp_type_gtkobject_id;
extern NspTypeGtkObject *nsp_type_gtkobject;

/* type instances for gobject */

NspTypeGtkObject *new_type_gtkobject(type_mode mode);

/* instance for GtkObject */

NspGtkObject *new_gtkobject();

/*
* Object methods redefined for gtkobject 
*/

#ifdef GtkObject_Private 
static int init_gtkobject(NspGtkObject *o,NspTypeGtkObject *type);
static char *gtkobject_type_as_string(void);
static char *gtkobject_type_short_string(void);
static AttrTab gtkobject_attrs[];
/* static int int_gtkobject_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkobject_get_methods(void); 
#endif /* GtkObject_Private */

#define NULLGTKOBJECT (NspGtkObject*) 0

NspGtkObject *gtkobject_create(char *name,NspTypeBase *type);

/* from GtkObjectObj.c */

extern NspGtkObject *gtkobject_object (NspObject *O); 
extern int IsGtkObjectObj (Stack stack, int i); 
extern int IsGtkObject(NspObject *O);
extern NspGtkObject *GetGtkObjectCopy (Stack stack, int i); 
extern NspGtkObject *GetGtkObject (Stack stack, int i); 

#endif 
