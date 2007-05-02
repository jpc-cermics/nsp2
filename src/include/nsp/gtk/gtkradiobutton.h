/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkRadioButton
#define INC_NSP_GtkRadioButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcheckbutton.h"

/*
* NspGtkRadioButton inherits from NspGtkCheckButton
* just change some type attributes 
*/

typedef NspGtkCheckButton NspGtkRadioButton ;
typedef NspTypeGtkCheckButton NspTypeGtkRadioButton ;

extern int nsp_type_gtkradiobutton_id;
extern NspTypeGtkRadioButton *nsp_type_gtkradiobutton;

/* type instances for gtkcheckbutton */

NspTypeGtkRadioButton *new_type_gtkradiobutton(type_mode mode);

/* instance for GtkRadioButton */

NspGtkRadioButton *new_gtkradiobutton();

/*
* Object methods redefined for gtkradiobutton 
*/

#define NULLGTKRADIOBUTTON (NspGtkRadioButton*) 0

NspGtkRadioButton *gtkradiobutton_create(char *name,NspTypeBase *type);

/* from GtkRadioButtonObj.c */

extern NspGtkRadioButton *gtkradiobutton_object (NspObject *O); 
extern int IsGtkRadioButtonObj (Stack stack, int i); 
extern int IsGtkRadioButton(NspObject *O);
extern NspGtkRadioButton *GetGtkRadioButtonCopy (Stack stack, int i); 
extern NspGtkRadioButton *GetGtkRadioButton (Stack stack, int i); 

#endif 

#ifdef GtkRadioButton_Private 
static int init_gtkradiobutton(NspGtkRadioButton *o,NspTypeGtkRadioButton *type);
static char *gtkradiobutton_type_as_string(void);
static char *gtkradiobutton_type_short_string(NspObject *v);
static AttrTab gtkradiobutton_attrs[];
/* static int int_gtkradiobutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkradiobutton_get_methods(void); 
#endif /* GtkRadioButton_Private */
