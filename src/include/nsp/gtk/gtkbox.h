/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkBox
#define INC_NSP_GtkBox

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkBox inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkBox ;
typedef NspTypeGtkContainer NspTypeGtkBox ;

extern int nsp_type_gtkbox_id;
extern NspTypeGtkBox *nsp_type_gtkbox;

/* type instances for gtkcontainer */

NspTypeGtkBox *new_type_gtkbox(type_mode mode);

/* instance for GtkBox */

NspGtkBox *new_gtkbox();

/*
* Object methods redefined for gtkbox 
*/

#ifdef GtkBox_Private 
static int init_gtkbox(NspGtkBox *o,NspTypeGtkBox *type);
static char *gtkbox_type_as_string(void);
static char *gtkbox_type_short_string(void);
static AttrTab gtkbox_attrs[];
/* static int int_gtkbox_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkbox_get_methods(void); 
#endif /* GtkBox_Private */

#define NULLGTKBOX (NspGtkBox*) 0

NspGtkBox *gtkbox_create(char *name,NspTypeBase *type);

/* from GtkBoxObj.c */

extern NspGtkBox *gtkbox_object (NspObject *O); 
extern int IsGtkBoxObj (Stack stack, int i); 
extern int IsGtkBox(NspObject *O);
extern NspGtkBox *GetGtkBoxCopy (Stack stack, int i); 
extern NspGtkBox *GetGtkBox (Stack stack, int i); 

#endif 
