/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkHScrollbar
#define INC_NSP_GtkHScrollbar

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkscrollbar.h"

/*
* NspGtkHScrollbar inherits from NspGtkScrollbar
* just change some type attributes 
*/

typedef NspGtkScrollbar NspGtkHScrollbar ;
typedef NspTypeGtkScrollbar NspTypeGtkHScrollbar ;

extern int nsp_type_gtkhscrollbar_id;
extern NspTypeGtkHScrollbar *nsp_type_gtkhscrollbar;

/* type instances for gtkscrollbar */

NspTypeGtkHScrollbar *new_type_gtkhscrollbar(type_mode mode);

/* instance for GtkHScrollbar */

NspGtkHScrollbar *new_gtkhscrollbar();

/*
* Object methods redefined for gtkhscrollbar 
*/

#define NULLGTKHSCROLLBAR (NspGtkHScrollbar*) 0

NspGtkHScrollbar *gtkhscrollbar_create(char *name,NspTypeBase *type);

/* from GtkHScrollbarObj.c */

extern NspGtkHScrollbar *gtkhscrollbar_object (NspObject *O); 
extern int IsGtkHScrollbarObj (Stack stack, int i); 
extern int IsGtkHScrollbar(NspObject *O);
extern NspGtkHScrollbar *GetGtkHScrollbarCopy (Stack stack, int i); 
extern NspGtkHScrollbar *GetGtkHScrollbar (Stack stack, int i); 

#endif 

#ifdef GtkHScrollbar_Private 
static int init_gtkhscrollbar(NspGtkHScrollbar *o,NspTypeGtkHScrollbar *type);
static char *gtkhscrollbar_type_as_string(void);
static char *gtkhscrollbar_type_short_string(NspObject *v);
static AttrTab gtkhscrollbar_attrs[];
/* static int int_gtkhscrollbar_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkhscrollbar_get_methods(void); 
#endif /* GtkHScrollbar_Private */
