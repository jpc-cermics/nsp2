/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkHBox
#define INC_NSP_GtkHBox

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbox.h"

/*
* NspGtkHBox inherits from NspGtkBox
* just change some type attributes 
*/

typedef NspGtkBox NspGtkHBox ;
typedef NspTypeGtkBox NspTypeGtkHBox ;

extern int nsp_type_gtkhbox_id;
extern NspTypeGtkHBox *nsp_type_gtkhbox;

/* type instances for gtkbox */

NspTypeGtkHBox *new_type_gtkhbox(type_mode mode);

/* instance for GtkHBox */

NspGtkHBox *new_gtkhbox();

/*
* Object methods redefined for gtkhbox 
*/

#define NULLGTKHBOX (NspGtkHBox*) 0

NspGtkHBox *gtkhbox_create(char *name,NspTypeBase *type);

/* from GtkHBoxObj.c */

extern NspGtkHBox *gtkhbox_object (NspObject *O); 
extern int IsGtkHBoxObj (Stack stack, int i); 
extern int IsGtkHBox(NspObject *O);
extern NspGtkHBox *GetGtkHBoxCopy (Stack stack, int i); 
extern NspGtkHBox *GetGtkHBox (Stack stack, int i); 

#endif 

#ifdef GtkHBox_Private 
static int init_gtkhbox(NspGtkHBox *o,NspTypeGtkHBox *type);
static char *gtkhbox_type_as_string(void);
static char *gtkhbox_type_short_string(NspObject *v);
static AttrTab gtkhbox_attrs[];
/* static int int_gtkhbox_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkhbox_get_methods(void); 
#endif /* GtkHBox_Private */
