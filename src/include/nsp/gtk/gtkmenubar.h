/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkMenuBar
#define INC_NSP_GtkMenuBar

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkmenushell.h"

/*
* NspGtkMenuBar inherits from NspGtkMenuShell
* just change some type attributes 
*/

typedef NspGtkMenuShell NspGtkMenuBar ;
typedef NspTypeGtkMenuShell NspTypeGtkMenuBar ;

extern int nsp_type_gtkmenubar_id;
extern NspTypeGtkMenuBar *nsp_type_gtkmenubar;

/* type instances for gtkmenushell */

NspTypeGtkMenuBar *new_type_gtkmenubar(type_mode mode);

/* instance for GtkMenuBar */

NspGtkMenuBar *new_gtkmenubar();

/*
* Object methods redefined for gtkmenubar 
*/

#define NULLGTKMENUBAR (NspGtkMenuBar*) 0

NspGtkMenuBar *gtkmenubar_create(char *name,NspTypeBase *type);

/* from GtkMenuBarObj.c */

extern NspGtkMenuBar *gtkmenubar_object (NspObject *O); 
extern int IsGtkMenuBarObj (Stack stack, int i); 
extern int IsGtkMenuBar(NspObject *O);
extern NspGtkMenuBar *GetGtkMenuBarCopy (Stack stack, int i); 
extern NspGtkMenuBar *GetGtkMenuBar (Stack stack, int i); 

#endif 

#ifdef GtkMenuBar_Private 
static int init_gtkmenubar(NspGtkMenuBar *o,NspTypeGtkMenuBar *type);
static char *gtkmenubar_type_as_string(void);
static char *gtkmenubar_type_short_string(NspObject *v);
static AttrTab gtkmenubar_attrs[];
/* static int int_gtkmenubar_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkmenubar_get_methods(void); 
#endif /* GtkMenuBar_Private */
