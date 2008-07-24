/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkColorButton
#define INC_NSP_GtkColorButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbutton.h"

/*
* NspGtkColorButton inherits from NspGtkButton
* just change some type attributes 
*/

typedef NspGtkButton NspGtkColorButton ;
typedef NspTypeGtkButton NspTypeGtkColorButton ;

extern int nsp_type_gtkcolorbutton_id;
extern NspTypeGtkColorButton *nsp_type_gtkcolorbutton;

/* type instances for gtkbutton */

NspTypeGtkColorButton *new_type_gtkcolorbutton(type_mode mode);

/* instance for GtkColorButton */

NspGtkColorButton *new_gtkcolorbutton();

/*
* Object methods redefined for gtkcolorbutton 
*/

#define NULLGTKCOLORBUTTON (NspGtkColorButton*) 0

NspGtkColorButton *gtkcolorbutton_create(char *name,NspTypeBase *type);

/* from GtkColorButtonObj.c */

extern NspGtkColorButton *gtkcolorbutton_object (NspObject *O); 
extern int IsGtkColorButtonObj (Stack stack, int i); 
extern int IsGtkColorButton(NspObject *O);
extern NspGtkColorButton *GetGtkColorButtonCopy (Stack stack, int i); 
extern NspGtkColorButton *GetGtkColorButton (Stack stack, int i); 

#endif 

#ifdef GtkColorButton_Private 
static int init_gtkcolorbutton(NspGtkColorButton *o,NspTypeGtkColorButton *type);
static char *gtkcolorbutton_type_as_string(void);
static char *gtkcolorbutton_type_short_string(NspObject *v);
static AttrTab gtkcolorbutton_attrs[];
/* static int int_gtkcolorbutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcolorbutton_get_methods(void); 
#endif /* GtkColorButton_Private */
