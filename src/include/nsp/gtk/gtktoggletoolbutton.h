/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkToggleToolButton
#define INC_NSP_GtkToggleToolButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtktoolbutton.h"

/*
* NspGtkToggleToolButton inherits from NspGtkToolButton
* just change some type attributes 
*/

typedef NspGtkToolButton NspGtkToggleToolButton ;
typedef NspTypeGtkToolButton NspTypeGtkToggleToolButton ;

extern int nsp_type_gtktoggletoolbutton_id;
extern NspTypeGtkToggleToolButton *nsp_type_gtktoggletoolbutton;

/* type instances for gtktoolbutton */

NspTypeGtkToggleToolButton *new_type_gtktoggletoolbutton(type_mode mode);

/* instance for GtkToggleToolButton */

NspGtkToggleToolButton *new_gtktoggletoolbutton();

/*
* Object methods redefined for gtktoggletoolbutton 
*/

#define NULLGTKTOGGLETOOLBUTTON (NspGtkToggleToolButton*) 0

NspGtkToggleToolButton *gtktoggletoolbutton_create(char *name,NspTypeBase *type);

/* from GtkToggleToolButtonObj.c */

extern NspGtkToggleToolButton *gtktoggletoolbutton_object (NspObject *O); 
extern int IsGtkToggleToolButtonObj (Stack stack, int i); 
extern int IsGtkToggleToolButton(NspObject *O);
extern NspGtkToggleToolButton *GetGtkToggleToolButtonCopy (Stack stack, int i); 
extern NspGtkToggleToolButton *GetGtkToggleToolButton (Stack stack, int i); 

#endif 

#ifdef GtkToggleToolButton_Private 
static int init_gtktoggletoolbutton(NspGtkToggleToolButton *o,NspTypeGtkToggleToolButton *type);
static char *gtktoggletoolbutton_type_as_string(void);
static char *gtktoggletoolbutton_type_short_string(NspObject *v);
static AttrTab gtktoggletoolbutton_attrs[];
/* static int int_gtktoggletoolbutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktoggletoolbutton_get_methods(void); 
#endif /* GtkToggleToolButton_Private */
