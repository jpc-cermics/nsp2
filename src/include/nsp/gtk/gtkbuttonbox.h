/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkButtonBox
#define INC_NSP_GtkButtonBox

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbox.h"

/*
* NspGtkButtonBox inherits from NspGtkBox
* just change some type attributes 
*/

typedef NspGtkBox NspGtkButtonBox ;
typedef NspTypeGtkBox NspTypeGtkButtonBox ;

extern int nsp_type_gtkbuttonbox_id;
extern NspTypeGtkButtonBox *nsp_type_gtkbuttonbox;

/* type instances for gtkbox */

NspTypeGtkButtonBox *new_type_gtkbuttonbox(type_mode mode);

/* instance for GtkButtonBox */

NspGtkButtonBox *new_gtkbuttonbox();

/*
* Object methods redefined for gtkbuttonbox 
*/

#ifdef GtkButtonBox_Private 
static int init_gtkbuttonbox(NspGtkButtonBox *o,NspTypeGtkButtonBox *type);
static char *gtkbuttonbox_type_as_string(void);
static char *gtkbuttonbox_type_short_string(void);
static AttrTab gtkbuttonbox_attrs[];
/* static int int_gtkbuttonbox_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkbuttonbox_get_methods(void); 
#endif /* GtkButtonBox_Private */

#define NULLGTKBUTTONBOX (NspGtkButtonBox*) 0

NspGtkButtonBox *gtkbuttonbox_create(char *name,NspTypeBase *type);

/* from GtkButtonBoxObj.c */

extern NspGtkButtonBox *gtkbuttonbox_object (NspObject *O); 
extern int IsGtkButtonBoxObj (Stack stack, int i); 
extern int IsGtkButtonBox(NspObject *O);
extern NspGtkButtonBox *GetGtkButtonBoxCopy (Stack stack, int i); 
extern NspGtkButtonBox *GetGtkButtonBox (Stack stack, int i); 

#endif 
