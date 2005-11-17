/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkToggleButton
#define INC_NSP_GtkToggleButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbutton.h"

/*
* NspGtkToggleButton inherits from NspGtkButton
* just change some type attributes 
*/

typedef NspGtkButton NspGtkToggleButton ;
typedef NspTypeGtkButton NspTypeGtkToggleButton ;

extern int nsp_type_gtktogglebutton_id;
extern NspTypeGtkToggleButton *nsp_type_gtktogglebutton;

/* type instances for gtkbutton */

NspTypeGtkToggleButton *new_type_gtktogglebutton(type_mode mode);

/* instance for GtkToggleButton */

NspGtkToggleButton *new_gtktogglebutton();

/*
* Object methods redefined for gtktogglebutton 
*/

#define NULLGTKTOGGLEBUTTON (NspGtkToggleButton*) 0

NspGtkToggleButton *gtktogglebutton_create(char *name,NspTypeBase *type);

/* from GtkToggleButtonObj.c */

extern NspGtkToggleButton *gtktogglebutton_object (NspObject *O); 
extern int IsGtkToggleButtonObj (Stack stack, int i); 
extern int IsGtkToggleButton(NspObject *O);
extern NspGtkToggleButton *GetGtkToggleButtonCopy (Stack stack, int i); 
extern NspGtkToggleButton *GetGtkToggleButton (Stack stack, int i); 

#endif 

#ifdef GtkToggleButton_Private 
static int init_gtktogglebutton(NspGtkToggleButton *o,NspTypeGtkToggleButton *type);
static char *gtktogglebutton_type_as_string(void);
static char *gtktogglebutton_type_short_string(void);
static AttrTab gtktogglebutton_attrs[];
/* static int int_gtktogglebutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktogglebutton_get_methods(void); 
#endif /* GtkToggleButton_Private */
