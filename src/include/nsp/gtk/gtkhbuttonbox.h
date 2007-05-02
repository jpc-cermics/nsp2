/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkHButtonBox
#define INC_NSP_GtkHButtonBox

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbuttonbox.h"

/*
* NspGtkHButtonBox inherits from NspGtkButtonBox
* just change some type attributes 
*/

typedef NspGtkButtonBox NspGtkHButtonBox ;
typedef NspTypeGtkButtonBox NspTypeGtkHButtonBox ;

extern int nsp_type_gtkhbuttonbox_id;
extern NspTypeGtkHButtonBox *nsp_type_gtkhbuttonbox;

/* type instances for gtkbuttonbox */

NspTypeGtkHButtonBox *new_type_gtkhbuttonbox(type_mode mode);

/* instance for GtkHButtonBox */

NspGtkHButtonBox *new_gtkhbuttonbox();

/*
* Object methods redefined for gtkhbuttonbox 
*/

#define NULLGTKHBUTTONBOX (NspGtkHButtonBox*) 0

NspGtkHButtonBox *gtkhbuttonbox_create(char *name,NspTypeBase *type);

/* from GtkHButtonBoxObj.c */

extern NspGtkHButtonBox *gtkhbuttonbox_object (NspObject *O); 
extern int IsGtkHButtonBoxObj (Stack stack, int i); 
extern int IsGtkHButtonBox(NspObject *O);
extern NspGtkHButtonBox *GetGtkHButtonBoxCopy (Stack stack, int i); 
extern NspGtkHButtonBox *GetGtkHButtonBox (Stack stack, int i); 

#endif 

#ifdef GtkHButtonBox_Private 
static int init_gtkhbuttonbox(NspGtkHButtonBox *o,NspTypeGtkHButtonBox *type);
static char *gtkhbuttonbox_type_as_string(void);
static char *gtkhbuttonbox_type_short_string(NspObject *v);
static AttrTab gtkhbuttonbox_attrs[];
/* static int int_gtkhbuttonbox_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkhbuttonbox_get_methods(void); 
#endif /* GtkHButtonBox_Private */
