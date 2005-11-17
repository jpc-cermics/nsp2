/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCheckButton
#define INC_NSP_GtkCheckButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtktogglebutton.h"

/*
* NspGtkCheckButton inherits from NspGtkToggleButton
* just change some type attributes 
*/

typedef NspGtkToggleButton NspGtkCheckButton ;
typedef NspTypeGtkToggleButton NspTypeGtkCheckButton ;

extern int nsp_type_gtkcheckbutton_id;
extern NspTypeGtkCheckButton *nsp_type_gtkcheckbutton;

/* type instances for gtktogglebutton */

NspTypeGtkCheckButton *new_type_gtkcheckbutton(type_mode mode);

/* instance for GtkCheckButton */

NspGtkCheckButton *new_gtkcheckbutton();

/*
* Object methods redefined for gtkcheckbutton 
*/

#define NULLGTKCHECKBUTTON (NspGtkCheckButton*) 0

NspGtkCheckButton *gtkcheckbutton_create(char *name,NspTypeBase *type);

/* from GtkCheckButtonObj.c */

extern NspGtkCheckButton *gtkcheckbutton_object (NspObject *O); 
extern int IsGtkCheckButtonObj (Stack stack, int i); 
extern int IsGtkCheckButton(NspObject *O);
extern NspGtkCheckButton *GetGtkCheckButtonCopy (Stack stack, int i); 
extern NspGtkCheckButton *GetGtkCheckButton (Stack stack, int i); 

#endif 

#ifdef GtkCheckButton_Private 
static int init_gtkcheckbutton(NspGtkCheckButton *o,NspTypeGtkCheckButton *type);
static char *gtkcheckbutton_type_as_string(void);
static char *gtkcheckbutton_type_short_string(void);
static AttrTab gtkcheckbutton_attrs[];
/* static int int_gtkcheckbutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcheckbutton_get_methods(void); 
#endif /* GtkCheckButton_Private */
