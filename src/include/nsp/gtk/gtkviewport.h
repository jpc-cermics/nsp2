/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkViewport
#define INC_NSP_GtkViewport

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkViewport inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkViewport ;
typedef NspTypeGtkBin NspTypeGtkViewport ;

extern int nsp_type_gtkviewport_id;
extern NspTypeGtkViewport *nsp_type_gtkviewport;

/* type instances for gtkbin */

NspTypeGtkViewport *new_type_gtkviewport(type_mode mode);

/* instance for GtkViewport */

NspGtkViewport *new_gtkviewport();

/*
* Object methods redefined for gtkviewport 
*/

#define NULLGTKVIEWPORT (NspGtkViewport*) 0

NspGtkViewport *gtkviewport_create(char *name,NspTypeBase *type);

/* from GtkViewportObj.c */

extern NspGtkViewport *gtkviewport_object (NspObject *O); 
extern int IsGtkViewportObj (Stack stack, int i); 
extern int IsGtkViewport(NspObject *O);
extern NspGtkViewport *GetGtkViewportCopy (Stack stack, int i); 
extern NspGtkViewport *GetGtkViewport (Stack stack, int i); 

#endif 

#ifdef GtkViewport_Private 
static int init_gtkviewport(NspGtkViewport *o,NspTypeGtkViewport *type);
static char *gtkviewport_type_as_string(void);
static char *gtkviewport_type_short_string(NspObject *v);
static AttrTab gtkviewport_attrs[];
/* static int int_gtkviewport_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkviewport_get_methods(void); 
#endif /* GtkViewport_Private */
