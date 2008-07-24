/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkMenuToolButton
#define INC_NSP_GtkMenuToolButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtktoolbutton.h"

/*
* NspGtkMenuToolButton inherits from NspGtkToolButton
* just change some type attributes 
*/

typedef NspGtkToolButton NspGtkMenuToolButton ;
typedef NspTypeGtkToolButton NspTypeGtkMenuToolButton ;

extern int nsp_type_gtkmenutoolbutton_id;
extern NspTypeGtkMenuToolButton *nsp_type_gtkmenutoolbutton;

/* type instances for gtktoolbutton */

NspTypeGtkMenuToolButton *new_type_gtkmenutoolbutton(type_mode mode);

/* instance for GtkMenuToolButton */

NspGtkMenuToolButton *new_gtkmenutoolbutton();

/*
* Object methods redefined for gtkmenutoolbutton 
*/

#define NULLGTKMENUTOOLBUTTON (NspGtkMenuToolButton*) 0

NspGtkMenuToolButton *gtkmenutoolbutton_create(char *name,NspTypeBase *type);

/* from GtkMenuToolButtonObj.c */

extern NspGtkMenuToolButton *gtkmenutoolbutton_object (NspObject *O); 
extern int IsGtkMenuToolButtonObj (Stack stack, int i); 
extern int IsGtkMenuToolButton(NspObject *O);
extern NspGtkMenuToolButton *GetGtkMenuToolButtonCopy (Stack stack, int i); 
extern NspGtkMenuToolButton *GetGtkMenuToolButton (Stack stack, int i); 

#endif 

#ifdef GtkMenuToolButton_Private 
static int init_gtkmenutoolbutton(NspGtkMenuToolButton *o,NspTypeGtkMenuToolButton *type);
static char *gtkmenutoolbutton_type_as_string(void);
static char *gtkmenutoolbutton_type_short_string(NspObject *v);
static AttrTab gtkmenutoolbutton_attrs[];
/* static int int_gtkmenutoolbutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkmenutoolbutton_get_methods(void); 
#endif /* GtkMenuToolButton_Private */
