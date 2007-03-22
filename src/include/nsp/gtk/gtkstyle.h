/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkStyle
#define INC_NSP_GtkStyle

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkStyle inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkStyle ;
typedef NspTypeGObject NspTypeGtkStyle ;

extern int nsp_type_gtkstyle_id;
extern NspTypeGtkStyle *nsp_type_gtkstyle;

/* type instances for gobject */

NspTypeGtkStyle *new_type_gtkstyle(type_mode mode);

/* instance for GtkStyle */

NspGtkStyle *new_gtkstyle();

/*
* Object methods redefined for gtkstyle 
*/

#define NULLGTKSTYLE (NspGtkStyle*) 0

NspGtkStyle *gtkstyle_create(char *name,NspTypeBase *type);

/* from GtkStyleObj.c */

extern NspGtkStyle *gtkstyle_object (NspObject *O); 
extern int IsGtkStyleObj (Stack stack, int i); 
extern int IsGtkStyle(NspObject *O);
extern NspGtkStyle *GetGtkStyleCopy (Stack stack, int i); 
extern NspGtkStyle *GetGtkStyle (Stack stack, int i); 

#endif 

#ifdef GtkStyle_Private 
static int init_gtkstyle(NspGtkStyle *o,NspTypeGtkStyle *type);
static char *gtkstyle_type_as_string(void);
static char *gtkstyle_type_short_string(NspObject *v);
static AttrTab gtkstyle_attrs[];
/* static int int_gtkstyle_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkstyle_get_methods(void); 
#endif /* GtkStyle_Private */
