/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkAccelLabel
#define INC_NSP_GtkAccelLabel

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtklabel.h"

/*
* NspGtkAccelLabel inherits from NspGtkLabel
* just change some type attributes 
*/

typedef NspGtkLabel NspGtkAccelLabel ;
typedef NspTypeGtkLabel NspTypeGtkAccelLabel ;

extern int nsp_type_gtkaccellabel_id;
extern NspTypeGtkAccelLabel *nsp_type_gtkaccellabel;

/* type instances for gtklabel */

NspTypeGtkAccelLabel *new_type_gtkaccellabel(type_mode mode);

/* instance for GtkAccelLabel */

NspGtkAccelLabel *new_gtkaccellabel();

/*
* Object methods redefined for gtkaccellabel 
*/

#define NULLGTKACCELLABEL (NspGtkAccelLabel*) 0

NspGtkAccelLabel *gtkaccellabel_create(char *name,NspTypeBase *type);

/* from GtkAccelLabelObj.c */

extern NspGtkAccelLabel *gtkaccellabel_object (NspObject *O); 
extern int IsGtkAccelLabelObj (Stack stack, int i); 
extern int IsGtkAccelLabel(NspObject *O);
extern NspGtkAccelLabel *GetGtkAccelLabelCopy (Stack stack, int i); 
extern NspGtkAccelLabel *GetGtkAccelLabel (Stack stack, int i); 

#endif 

#ifdef GtkAccelLabel_Private 
static int init_gtkaccellabel(NspGtkAccelLabel *o,NspTypeGtkAccelLabel *type);
static char *gtkaccellabel_type_as_string(void);
static char *gtkaccellabel_type_short_string(NspObject *v);
static AttrTab gtkaccellabel_attrs[];
/* static int int_gtkaccellabel_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkaccellabel_get_methods(void); 
#endif /* GtkAccelLabel_Private */
