/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkVScrollbar
#define INC_NSP_GtkVScrollbar

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkscrollbar.h"

/*
* NspGtkVScrollbar inherits from NspGtkScrollbar
* just change some type attributes 
*/

typedef NspGtkScrollbar NspGtkVScrollbar ;
typedef NspTypeGtkScrollbar NspTypeGtkVScrollbar ;

extern int nsp_type_gtkvscrollbar_id;
extern NspTypeGtkVScrollbar *nsp_type_gtkvscrollbar;

/* type instances for gtkscrollbar */

NspTypeGtkVScrollbar *new_type_gtkvscrollbar(type_mode mode);

/* instance for GtkVScrollbar */

NspGtkVScrollbar *new_gtkvscrollbar();

/*
* Object methods redefined for gtkvscrollbar 
*/

#define NULLGTKVSCROLLBAR (NspGtkVScrollbar*) 0

NspGtkVScrollbar *gtkvscrollbar_create(char *name,NspTypeBase *type);

/* from GtkVScrollbarObj.c */

extern NspGtkVScrollbar *gtkvscrollbar_object (NspObject *O); 
extern int IsGtkVScrollbarObj (Stack stack, int i); 
extern int IsGtkVScrollbar(NspObject *O);
extern NspGtkVScrollbar *GetGtkVScrollbarCopy (Stack stack, int i); 
extern NspGtkVScrollbar *GetGtkVScrollbar (Stack stack, int i); 

#endif 

#ifdef GtkVScrollbar_Private 
static int init_gtkvscrollbar(NspGtkVScrollbar *o,NspTypeGtkVScrollbar *type);
static char *gtkvscrollbar_type_as_string(void);
static char *gtkvscrollbar_type_short_string(NspObject *v);
static AttrTab gtkvscrollbar_attrs[];
/* static int int_gtkvscrollbar_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkvscrollbar_get_methods(void); 
#endif /* GtkVScrollbar_Private */
