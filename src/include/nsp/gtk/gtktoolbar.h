/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkToolbar
#define INC_NSP_GtkToolbar

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkToolbar inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkToolbar ;
typedef NspTypeGtkContainer NspTypeGtkToolbar ;

extern int nsp_type_gtktoolbar_id;
extern NspTypeGtkToolbar *nsp_type_gtktoolbar;

/* type instances for gtkcontainer */

NspTypeGtkToolbar *new_type_gtktoolbar(type_mode mode);

/* instance for GtkToolbar */

NspGtkToolbar *new_gtktoolbar();

/*
* Object methods redefined for gtktoolbar 
*/

#define NULLGTKTOOLBAR (NspGtkToolbar*) 0

NspGtkToolbar *gtktoolbar_create(char *name,NspTypeBase *type);

/* from GtkToolbarObj.c */

extern NspGtkToolbar *gtktoolbar_object (NspObject *O); 
extern int IsGtkToolbarObj (Stack stack, int i); 
extern int IsGtkToolbar(NspObject *O);
extern NspGtkToolbar *GetGtkToolbarCopy (Stack stack, int i); 
extern NspGtkToolbar *GetGtkToolbar (Stack stack, int i); 

#endif 

#ifdef GtkToolbar_Private 
static int init_gtktoolbar(NspGtkToolbar *o,NspTypeGtkToolbar *type);
static char *gtktoolbar_type_as_string(void);
static char *gtktoolbar_type_short_string(void);
static AttrTab gtktoolbar_attrs[];
/* static int int_gtktoolbar_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktoolbar_get_methods(void); 
#endif /* GtkToolbar_Private */
