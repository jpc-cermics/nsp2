/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFileChooserButton
#define INC_NSP_GtkFileChooserButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkhbox.h"

/*
* NspGtkFileChooserButton inherits from NspGtkHBox
* just change some type attributes 
*/

typedef NspGtkHBox NspGtkFileChooserButton ;
typedef NspTypeGtkHBox NspTypeGtkFileChooserButton ;

extern int nsp_type_gtkfilechooserbutton_id;
extern NspTypeGtkFileChooserButton *nsp_type_gtkfilechooserbutton;

/* type instances for gtkhbox */

NspTypeGtkFileChooserButton *new_type_gtkfilechooserbutton(type_mode mode);

/* instance for GtkFileChooserButton */

NspGtkFileChooserButton *new_gtkfilechooserbutton();

/*
* Object methods redefined for gtkfilechooserbutton 
*/

#define NULLGTKFILECHOOSERBUTTON (NspGtkFileChooserButton*) 0

NspGtkFileChooserButton *gtkfilechooserbutton_create(char *name,NspTypeBase *type);

/* from GtkFileChooserButtonObj.c */

extern NspGtkFileChooserButton *gtkfilechooserbutton_object (NspObject *O); 
extern int IsGtkFileChooserButtonObj (Stack stack, int i); 
extern int IsGtkFileChooserButton(NspObject *O);
extern NspGtkFileChooserButton *GetGtkFileChooserButtonCopy (Stack stack, int i); 
extern NspGtkFileChooserButton *GetGtkFileChooserButton (Stack stack, int i); 

#endif 

#ifdef GtkFileChooserButton_Private 
static int init_gtkfilechooserbutton(NspGtkFileChooserButton *o,NspTypeGtkFileChooserButton *type);
static char *gtkfilechooserbutton_type_as_string(void);
static char *gtkfilechooserbutton_type_short_string(NspObject *v);
static AttrTab gtkfilechooserbutton_attrs[];
/* static int int_gtkfilechooserbutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkfilechooserbutton_get_methods(void); 
#endif /* GtkFileChooserButton_Private */
