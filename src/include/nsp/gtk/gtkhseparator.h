/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkHSeparator
#define INC_NSP_GtkHSeparator

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkseparator.h"

/*
* NspGtkHSeparator inherits from NspGtkSeparator
* just change some type attributes 
*/

typedef NspGtkSeparator NspGtkHSeparator ;
typedef NspTypeGtkSeparator NspTypeGtkHSeparator ;

extern int nsp_type_gtkhseparator_id;
extern NspTypeGtkHSeparator *nsp_type_gtkhseparator;

/* type instances for gtkseparator */

NspTypeGtkHSeparator *new_type_gtkhseparator(type_mode mode);

/* instance for GtkHSeparator */

NspGtkHSeparator *new_gtkhseparator();

/*
* Object methods redefined for gtkhseparator 
*/

#ifdef GtkHSeparator_Private 
static int init_gtkhseparator(NspGtkHSeparator *o,NspTypeGtkHSeparator *type);
static char *gtkhseparator_type_as_string(void);
static char *gtkhseparator_type_short_string(void);
static AttrTab gtkhseparator_attrs[];
/* static int int_gtkhseparator_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkhseparator_get_methods(void); 
#endif /* GtkHSeparator_Private */

#define NULLGTKHSEPARATOR (NspGtkHSeparator*) 0

NspGtkHSeparator *gtkhseparator_create(char *name,NspTypeBase *type);

/* from GtkHSeparatorObj.c */

extern NspGtkHSeparator *gtkhseparator_object (NspObject *O); 
extern int IsGtkHSeparatorObj (Stack stack, int i); 
extern int IsGtkHSeparator(NspObject *O);
extern NspGtkHSeparator *GetGtkHSeparatorCopy (Stack stack, int i); 
extern NspGtkHSeparator *GetGtkHSeparator (Stack stack, int i); 

#endif 
