/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTooltips
#define INC_NSP_GtkTooltips

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkobject.h"

/*
* NspGtkTooltips inherits from NspGtkObject
* just change some type attributes 
*/

typedef NspGtkObject NspGtkTooltips ;
typedef NspTypeGtkObject NspTypeGtkTooltips ;

extern int nsp_type_gtktooltips_id;
extern NspTypeGtkTooltips *nsp_type_gtktooltips;

/* type instances for gtkobject */

NspTypeGtkTooltips *new_type_gtktooltips(type_mode mode);

/* instance for GtkTooltips */

NspGtkTooltips *new_gtktooltips();

/*
* Object methods redefined for gtktooltips 
*/

#define NULLGTKTOOLTIPS (NspGtkTooltips*) 0

NspGtkTooltips *gtktooltips_create(char *name,NspTypeBase *type);

/* from GtkTooltipsObj.c */

extern NspGtkTooltips *gtktooltips_object (NspObject *O); 
extern int IsGtkTooltipsObj (Stack stack, int i); 
extern int IsGtkTooltips(NspObject *O);
extern NspGtkTooltips *GetGtkTooltipsCopy (Stack stack, int i); 
extern NspGtkTooltips *GetGtkTooltips (Stack stack, int i); 

#endif 

#ifdef GtkTooltips_Private 
static int init_gtktooltips(NspGtkTooltips *o,NspTypeGtkTooltips *type);
static char *gtktooltips_type_as_string(void);
static char *gtktooltips_type_short_string(NspObject *v);
static AttrTab gtktooltips_attrs[];
/* static int int_gtktooltips_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktooltips_get_methods(void); 
#endif /* GtkTooltips_Private */
