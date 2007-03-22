/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkSpinButton
#define INC_NSP_GtkSpinButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkentry.h"

/*
* NspGtkSpinButton inherits from NspGtkEntry
* just change some type attributes 
*/

typedef NspGtkEntry NspGtkSpinButton ;
typedef NspTypeGtkEntry NspTypeGtkSpinButton ;

extern int nsp_type_gtkspinbutton_id;
extern NspTypeGtkSpinButton *nsp_type_gtkspinbutton;

/* type instances for gtkentry */

NspTypeGtkSpinButton *new_type_gtkspinbutton(type_mode mode);

/* instance for GtkSpinButton */

NspGtkSpinButton *new_gtkspinbutton();

/*
* Object methods redefined for gtkspinbutton 
*/

#define NULLGTKSPINBUTTON (NspGtkSpinButton*) 0

NspGtkSpinButton *gtkspinbutton_create(char *name,NspTypeBase *type);

/* from GtkSpinButtonObj.c */

extern NspGtkSpinButton *gtkspinbutton_object (NspObject *O); 
extern int IsGtkSpinButtonObj (Stack stack, int i); 
extern int IsGtkSpinButton(NspObject *O);
extern NspGtkSpinButton *GetGtkSpinButtonCopy (Stack stack, int i); 
extern NspGtkSpinButton *GetGtkSpinButton (Stack stack, int i); 

#endif 

#ifdef GtkSpinButton_Private 
static int init_gtkspinbutton(NspGtkSpinButton *o,NspTypeGtkSpinButton *type);
static char *gtkspinbutton_type_as_string(void);
static char *gtkspinbutton_type_short_string(NspObject *v);
static AttrTab gtkspinbutton_attrs[];
/* static int int_gtkspinbutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkspinbutton_get_methods(void); 
#endif /* GtkSpinButton_Private */
