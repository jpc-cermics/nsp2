/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkVBox
#define INC_NSP_GtkVBox

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbox.h"

/*
* NspGtkVBox inherits from NspGtkBox
* just change some type attributes 
*/

typedef NspGtkBox NspGtkVBox ;
typedef NspTypeGtkBox NspTypeGtkVBox ;

extern int nsp_type_gtkvbox_id;
extern NspTypeGtkVBox *nsp_type_gtkvbox;

/* type instances for gtkbox */

NspTypeGtkVBox *new_type_gtkvbox(type_mode mode);

/* instance for GtkVBox */

NspGtkVBox *new_gtkvbox();

/*
* Object methods redefined for gtkvbox 
*/

#ifdef GtkVBox_Private 
static int init_gtkvbox(NspGtkVBox *o,NspTypeGtkVBox *type);
static char *gtkvbox_type_as_string(void);
static char *gtkvbox_type_short_string(void);
static AttrTab gtkvbox_attrs[];
/* static int int_gtkvbox_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkvbox_get_methods(void); 
#endif /* GtkVBox_Private */

#define NULLGTKVBOX (NspGtkVBox*) 0

NspGtkVBox *gtkvbox_create(char *name,NspTypeBase *type);

/* from GtkVBoxObj.c */

extern NspGtkVBox *gtkvbox_object (NspObject *O); 
extern int IsGtkVBoxObj (Stack stack, int i); 
extern int IsGtkVBox(NspObject *O);
extern NspGtkVBox *GetGtkVBoxCopy (Stack stack, int i); 
extern NspGtkVBox *GetGtkVBox (Stack stack, int i); 

#endif 
