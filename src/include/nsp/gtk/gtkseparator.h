/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkSeparator
#define INC_NSP_GtkSeparator

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkSeparator inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkSeparator ;
typedef NspTypeGtkWidget NspTypeGtkSeparator ;

extern int nsp_type_gtkseparator_id;
extern NspTypeGtkSeparator *nsp_type_gtkseparator;

/* type instances for gtkwidget */

NspTypeGtkSeparator *new_type_gtkseparator(type_mode mode);

/* instance for GtkSeparator */

NspGtkSeparator *new_gtkseparator();

/*
* Object methods redefined for gtkseparator 
*/

#ifdef GtkSeparator_Private 
static int init_gtkseparator(NspGtkSeparator *o,NspTypeGtkSeparator *type);
static char *gtkseparator_type_as_string(void);
static char *gtkseparator_type_short_string(void);
static AttrTab gtkseparator_attrs[];
/* static int int_gtkseparator_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkseparator_get_methods(void); 
#endif /* GtkSeparator_Private */

#define NULLGTKSEPARATOR (NspGtkSeparator*) 0

NspGtkSeparator *gtkseparator_create(char *name,NspTypeBase *type);

/* from GtkSeparatorObj.c */

extern NspGtkSeparator *gtkseparator_object (NspObject *O); 
extern int IsGtkSeparatorObj (Stack stack, int i); 
extern int IsGtkSeparator(NspObject *O);
extern NspGtkSeparator *GetGtkSeparatorCopy (Stack stack, int i); 
extern NspGtkSeparator *GetGtkSeparator (Stack stack, int i); 

#endif 
