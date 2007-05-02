/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCellLayout
#define INC_NSP_GtkCellLayout

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkCellLayout inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkCellLayout ;
typedef NspTypeGObject NspTypeGtkCellLayout ;

extern int nsp_type_gtkcelllayout_id;
extern NspTypeGtkCellLayout *nsp_type_gtkcelllayout;

/* type instances for gobject */

NspTypeGtkCellLayout *new_type_gtkcelllayout(type_mode mode);

/* instance for GtkCellLayout */

NspGtkCellLayout *new_gtkcelllayout();

/*
* Object methods redefined for gtkcelllayout 
*/

#define NULLGTKCELLLAYOUT (NspGtkCellLayout*) 0

NspGtkCellLayout *gtkcelllayout_create(char *name,NspTypeBase *type);

/* from GtkCellLayoutObj.c */

extern NspGtkCellLayout *gtkcelllayout_object (NspObject *O); 
extern int IsGtkCellLayoutObj (Stack stack, int i); 
extern int IsGtkCellLayout(NspObject *O);
extern NspGtkCellLayout *GetGtkCellLayoutCopy (Stack stack, int i); 
extern NspGtkCellLayout *GetGtkCellLayout (Stack stack, int i); 

#endif 

#ifdef GtkCellLayout_Private 
static int init_gtkcelllayout(NspGtkCellLayout *o,NspTypeGtkCellLayout *type);
static char *gtkcelllayout_type_as_string(void);
static char *gtkcelllayout_type_short_string(NspObject *v);
static AttrTab gtkcelllayout_attrs[];
/* static int int_gtkcelllayout_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcelllayout_get_methods(void); 
#endif /* GtkCellLayout_Private */
