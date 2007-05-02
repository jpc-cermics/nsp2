/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkVSeparator
#define INC_NSP_GtkVSeparator

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkseparator.h"

/*
* NspGtkVSeparator inherits from NspGtkSeparator
* just change some type attributes 
*/

typedef NspGtkSeparator NspGtkVSeparator ;
typedef NspTypeGtkSeparator NspTypeGtkVSeparator ;

extern int nsp_type_gtkvseparator_id;
extern NspTypeGtkVSeparator *nsp_type_gtkvseparator;

/* type instances for gtkseparator */

NspTypeGtkVSeparator *new_type_gtkvseparator(type_mode mode);

/* instance for GtkVSeparator */

NspGtkVSeparator *new_gtkvseparator();

/*
* Object methods redefined for gtkvseparator 
*/

#define NULLGTKVSEPARATOR (NspGtkVSeparator*) 0

NspGtkVSeparator *gtkvseparator_create(char *name,NspTypeBase *type);

/* from GtkVSeparatorObj.c */

extern NspGtkVSeparator *gtkvseparator_object (NspObject *O); 
extern int IsGtkVSeparatorObj (Stack stack, int i); 
extern int IsGtkVSeparator(NspObject *O);
extern NspGtkVSeparator *GetGtkVSeparatorCopy (Stack stack, int i); 
extern NspGtkVSeparator *GetGtkVSeparator (Stack stack, int i); 

#endif 

#ifdef GtkVSeparator_Private 
static int init_gtkvseparator(NspGtkVSeparator *o,NspTypeGtkVSeparator *type);
static char *gtkvseparator_type_as_string(void);
static char *gtkvseparator_type_short_string(NspObject *v);
static AttrTab gtkvseparator_attrs[];
/* static int int_gtkvseparator_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkvseparator_get_methods(void); 
#endif /* GtkVSeparator_Private */
