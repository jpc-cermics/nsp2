/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFontButton
#define INC_NSP_GtkFontButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbutton.h"

/*
* NspGtkFontButton inherits from NspGtkButton
* just change some type attributes 
*/

typedef NspGtkButton NspGtkFontButton ;
typedef NspTypeGtkButton NspTypeGtkFontButton ;

extern int nsp_type_gtkfontbutton_id;
extern NspTypeGtkFontButton *nsp_type_gtkfontbutton;

/* type instances for gtkbutton */

NspTypeGtkFontButton *new_type_gtkfontbutton(type_mode mode);

/* instance for GtkFontButton */

NspGtkFontButton *new_gtkfontbutton();

/*
* Object methods redefined for gtkfontbutton 
*/

#define NULLGTKFONTBUTTON (NspGtkFontButton*) 0

NspGtkFontButton *gtkfontbutton_create(char *name,NspTypeBase *type);

/* from GtkFontButtonObj.c */

extern NspGtkFontButton *gtkfontbutton_object (NspObject *O); 
extern int IsGtkFontButtonObj (Stack stack, int i); 
extern int IsGtkFontButton(NspObject *O);
extern NspGtkFontButton *GetGtkFontButtonCopy (Stack stack, int i); 
extern NspGtkFontButton *GetGtkFontButton (Stack stack, int i); 

#endif 

#ifdef GtkFontButton_Private 
static int init_gtkfontbutton(NspGtkFontButton *o,NspTypeGtkFontButton *type);
static char *gtkfontbutton_type_as_string(void);
static char *gtkfontbutton_type_short_string(NspObject *v);
static AttrTab gtkfontbutton_attrs[];
/* static int int_gtkfontbutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkfontbutton_get_methods(void); 
#endif /* GtkFontButton_Private */
