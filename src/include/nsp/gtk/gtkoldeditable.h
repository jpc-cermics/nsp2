/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkOldEditable
#define INC_NSP_GtkOldEditable

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkOldEditable inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkOldEditable ;
typedef NspTypeGtkWidget NspTypeGtkOldEditable ;

extern int nsp_type_gtkoldeditable_id;
extern NspTypeGtkOldEditable *nsp_type_gtkoldeditable;

/* type instances for gtkwidget */

NspTypeGtkOldEditable *new_type_gtkoldeditable(type_mode mode);

/* instance for GtkOldEditable */

NspGtkOldEditable *new_gtkoldeditable();

/*
* Object methods redefined for gtkoldeditable 
*/

#ifdef GtkOldEditable_Private 
static int init_gtkoldeditable(NspGtkOldEditable *o,NspTypeGtkOldEditable *type);
static char *gtkoldeditable_type_as_string(void);
static char *gtkoldeditable_type_short_string(void);
static AttrTab gtkoldeditable_attrs[];
/* static int int_gtkoldeditable_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkoldeditable_get_methods(void); 
#endif /* GtkOldEditable_Private */

#define NULLGTKOLDEDITABLE (NspGtkOldEditable*) 0

NspGtkOldEditable *gtkoldeditable_create(char *name,NspTypeBase *type);

/* from GtkOldEditableObj.c */

extern NspGtkOldEditable *gtkoldeditable_object (NspObject *O); 
extern int IsGtkOldEditableObj (Stack stack, int i); 
extern int IsGtkOldEditable(NspObject *O);
extern NspGtkOldEditable *GetGtkOldEditableCopy (Stack stack, int i); 
extern NspGtkOldEditable *GetGtkOldEditable (Stack stack, int i); 

#endif 
