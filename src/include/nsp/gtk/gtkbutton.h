/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkButton
#define INC_NSP_GtkButton

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkButton inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkButton ;
typedef NspTypeGtkBin NspTypeGtkButton ;

extern int nsp_type_gtkbutton_id;
extern NspTypeGtkButton *nsp_type_gtkbutton;

/* type instances for gtkbin */

NspTypeGtkButton *new_type_gtkbutton(type_mode mode);

/* instance for GtkButton */

NspGtkButton *new_gtkbutton();

/*
* Object methods redefined for gtkbutton 
*/

#define NULLGTKBUTTON (NspGtkButton*) 0

NspGtkButton *gtkbutton_create(char *name,NspTypeBase *type);

/* from GtkButtonObj.c */

extern NspGtkButton *gtkbutton_object (NspObject *O); 
extern int IsGtkButtonObj (Stack stack, int i); 
extern int IsGtkButton(NspObject *O);
extern NspGtkButton *GetGtkButtonCopy (Stack stack, int i); 
extern NspGtkButton *GetGtkButton (Stack stack, int i); 

#endif 

#ifdef GtkButton_Private 
static int init_gtkbutton(NspGtkButton *o,NspTypeGtkButton *type);
static char *gtkbutton_type_as_string(void);
static char *gtkbutton_type_short_string(NspObject *v);
static AttrTab gtkbutton_attrs[];
/* static int int_gtkbutton_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkbutton_get_methods(void); 
#endif /* GtkButton_Private */
