/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkRadioToolButton
#define INC_NSP_GtkRadioToolButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtktoggletoolbutton.h"

/*
* NspGtkRadioToolButton inherits from NspGtkToggleToolButton
* just change some type attributes 
*/

typedef NspGtkToggleToolButton NspGtkRadioToolButton ;
typedef NspTypeGtkToggleToolButton NspTypeGtkRadioToolButton ;

extern int nsp_type_gtkradiotoolbutton_id;
extern NspTypeGtkRadioToolButton *nsp_type_gtkradiotoolbutton;

/* type instances for gtktoggletoolbutton */

NspTypeGtkRadioToolButton *new_type_gtkradiotoolbutton(type_mode mode);

/* instance for GtkRadioToolButton */

NspGtkRadioToolButton *new_gtkradiotoolbutton();

/*
* Object methods redefined for gtkradiotoolbutton 
*/

#define NULLGTKRADIOTOOLBUTTON (NspGtkRadioToolButton*) 0

NspGtkRadioToolButton *gtkradiotoolbutton_create(char *name,NspTypeBase *type);

/* from GtkRadioToolButtonObj.c */

extern NspGtkRadioToolButton *gtkradiotoolbutton_object (NspObject *O); 
extern int IsGtkRadioToolButtonObj (Stack stack, int i); 
extern int IsGtkRadioToolButton(NspObject *O);
extern NspGtkRadioToolButton *GetGtkRadioToolButtonCopy (Stack stack, int i); 
extern NspGtkRadioToolButton *GetGtkRadioToolButton (Stack stack, int i); 

#endif 

#ifdef GtkRadioToolButton_Private 
static int init_gtkradiotoolbutton(NspGtkRadioToolButton *o,NspTypeGtkRadioToolButton *type);
static char *gtkradiotoolbutton_type_as_string(void);
static char *gtkradiotoolbutton_type_short_string(NspObject *v);
static AttrTab gtkradiotoolbutton_attrs[];
/* static int int_gtkradiotoolbutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkradiotoolbutton_get_methods(void); 
#endif /* GtkRadioToolButton_Private */
