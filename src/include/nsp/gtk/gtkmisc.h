/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkMisc
#define INC_NSP_GtkMisc

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkMisc inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkMisc ;
typedef NspTypeGtkWidget NspTypeGtkMisc ;

extern int nsp_type_gtkmisc_id;
extern NspTypeGtkMisc *nsp_type_gtkmisc;

/* type instances for gtkwidget */

NspTypeGtkMisc *new_type_gtkmisc(type_mode mode);

/* instance for GtkMisc */

NspGtkMisc *new_gtkmisc();

/*
* Object methods redefined for gtkmisc 
*/

#define NULLGTKMISC (NspGtkMisc*) 0

NspGtkMisc *gtkmisc_create(char *name,NspTypeBase *type);

/* from GtkMiscObj.c */

extern NspGtkMisc *gtkmisc_object (NspObject *O); 
extern int IsGtkMiscObj (Stack stack, int i); 
extern int IsGtkMisc(NspObject *O);
extern NspGtkMisc *GetGtkMiscCopy (Stack stack, int i); 
extern NspGtkMisc *GetGtkMisc (Stack stack, int i); 

#endif 

#ifdef GtkMisc_Private 
static int init_gtkmisc(NspGtkMisc *o,NspTypeGtkMisc *type);
static char *gtkmisc_type_as_string(void);
static char *gtkmisc_type_short_string(void);
static AttrTab gtkmisc_attrs[];
/* static int int_gtkmisc_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkmisc_get_methods(void); 
#endif /* GtkMisc_Private */
