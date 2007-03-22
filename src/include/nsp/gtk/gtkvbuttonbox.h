/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkVButtonBox
#define INC_NSP_GtkVButtonBox

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbuttonbox.h"

/*
* NspGtkVButtonBox inherits from NspGtkButtonBox
* just change some type attributes 
*/

typedef NspGtkButtonBox NspGtkVButtonBox ;
typedef NspTypeGtkButtonBox NspTypeGtkVButtonBox ;

extern int nsp_type_gtkvbuttonbox_id;
extern NspTypeGtkVButtonBox *nsp_type_gtkvbuttonbox;

/* type instances for gtkbuttonbox */

NspTypeGtkVButtonBox *new_type_gtkvbuttonbox(type_mode mode);

/* instance for GtkVButtonBox */

NspGtkVButtonBox *new_gtkvbuttonbox();

/*
* Object methods redefined for gtkvbuttonbox 
*/

#define NULLGTKVBUTTONBOX (NspGtkVButtonBox*) 0

NspGtkVButtonBox *gtkvbuttonbox_create(char *name,NspTypeBase *type);

/* from GtkVButtonBoxObj.c */

extern NspGtkVButtonBox *gtkvbuttonbox_object (NspObject *O); 
extern int IsGtkVButtonBoxObj (Stack stack, int i); 
extern int IsGtkVButtonBox(NspObject *O);
extern NspGtkVButtonBox *GetGtkVButtonBoxCopy (Stack stack, int i); 
extern NspGtkVButtonBox *GetGtkVButtonBox (Stack stack, int i); 

#endif 

#ifdef GtkVButtonBox_Private 
static int init_gtkvbuttonbox(NspGtkVButtonBox *o,NspTypeGtkVButtonBox *type);
static char *gtkvbuttonbox_type_as_string(void);
static char *gtkvbuttonbox_type_short_string(NspObject *v);
static AttrTab gtkvbuttonbox_attrs[];
/* static int int_gtkvbuttonbox_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkvbuttonbox_get_methods(void); 
#endif /* GtkVButtonBox_Private */
