/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCellEditable
#define INC_NSP_GtkCellEditable

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkCellEditable inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkCellEditable ;
typedef NspTypeGObject NspTypeGtkCellEditable ;

extern int nsp_type_gtkcelleditable_id;
extern NspTypeGtkCellEditable *nsp_type_gtkcelleditable;

/* type instances for gobject */

NspTypeGtkCellEditable *new_type_gtkcelleditable(type_mode mode);

/* instance for GtkCellEditable */

NspGtkCellEditable *new_gtkcelleditable();

/*
* Object methods redefined for gtkcelleditable 
*/

#ifdef GtkCellEditable_Private 
static int init_gtkcelleditable(NspGtkCellEditable *o,NspTypeGtkCellEditable *type);
static char *gtkcelleditable_type_as_string(void);
static char *gtkcelleditable_type_short_string(void);
static AttrTab gtkcelleditable_attrs[];
/* static int int_gtkcelleditable_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcelleditable_get_methods(void); 
#endif /* GtkCellEditable_Private */

#define NULLGTKCELLEDITABLE (NspGtkCellEditable*) 0

NspGtkCellEditable *gtkcelleditable_create(char *name,NspTypeBase *type);

/* from GtkCellEditableObj.c */

extern NspGtkCellEditable *gtkcelleditable_object (NspObject *O); 
extern int IsGtkCellEditableObj (Stack stack, int i); 
extern int IsGtkCellEditable(NspObject *O);
extern NspGtkCellEditable *GetGtkCellEditableCopy (Stack stack, int i); 
extern NspGtkCellEditable *GetGtkCellEditable (Stack stack, int i); 

#endif 
