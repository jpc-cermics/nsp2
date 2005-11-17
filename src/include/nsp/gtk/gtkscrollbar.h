/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkScrollbar
#define INC_NSP_GtkScrollbar

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkrange.h"

/*
* NspGtkScrollbar inherits from NspGtkRange
* just change some type attributes 
*/

typedef NspGtkRange NspGtkScrollbar ;
typedef NspTypeGtkRange NspTypeGtkScrollbar ;

extern int nsp_type_gtkscrollbar_id;
extern NspTypeGtkScrollbar *nsp_type_gtkscrollbar;

/* type instances for gtkrange */

NspTypeGtkScrollbar *new_type_gtkscrollbar(type_mode mode);

/* instance for GtkScrollbar */

NspGtkScrollbar *new_gtkscrollbar();

/*
* Object methods redefined for gtkscrollbar 
*/

#define NULLGTKSCROLLBAR (NspGtkScrollbar*) 0

NspGtkScrollbar *gtkscrollbar_create(char *name,NspTypeBase *type);

/* from GtkScrollbarObj.c */

extern NspGtkScrollbar *gtkscrollbar_object (NspObject *O); 
extern int IsGtkScrollbarObj (Stack stack, int i); 
extern int IsGtkScrollbar(NspObject *O);
extern NspGtkScrollbar *GetGtkScrollbarCopy (Stack stack, int i); 
extern NspGtkScrollbar *GetGtkScrollbar (Stack stack, int i); 

#endif 

#ifdef GtkScrollbar_Private 
static int init_gtkscrollbar(NspGtkScrollbar *o,NspTypeGtkScrollbar *type);
static char *gtkscrollbar_type_as_string(void);
static char *gtkscrollbar_type_short_string(void);
static AttrTab gtkscrollbar_attrs[];
/* static int int_gtkscrollbar_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkscrollbar_get_methods(void); 
#endif /* GtkScrollbar_Private */
