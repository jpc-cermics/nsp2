/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkStatusbar
#define INC_NSP_GtkStatusbar

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkhbox.h"

/*
* NspGtkStatusbar inherits from NspGtkHBox
* just change some type attributes 
*/

typedef NspGtkHBox NspGtkStatusbar ;
typedef NspTypeGtkHBox NspTypeGtkStatusbar ;

extern int nsp_type_gtkstatusbar_id;
extern NspTypeGtkStatusbar *nsp_type_gtkstatusbar;

/* type instances for gtkhbox */

NspTypeGtkStatusbar *new_type_gtkstatusbar(type_mode mode);

/* instance for GtkStatusbar */

NspGtkStatusbar *new_gtkstatusbar();

/*
* Object methods redefined for gtkstatusbar 
*/

#ifdef GtkStatusbar_Private 
static int init_gtkstatusbar(NspGtkStatusbar *o,NspTypeGtkStatusbar *type);
static char *gtkstatusbar_type_as_string(void);
static char *gtkstatusbar_type_short_string(void);
static AttrTab gtkstatusbar_attrs[];
/* static int int_gtkstatusbar_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkstatusbar_get_methods(void); 
#endif /* GtkStatusbar_Private */

#define NULLGTKSTATUSBAR (NspGtkStatusbar*) 0

NspGtkStatusbar *gtkstatusbar_create(char *name,NspTypeBase *type);

/* from GtkStatusbarObj.c */

extern NspGtkStatusbar *gtkstatusbar_object (NspObject *O); 
extern int IsGtkStatusbarObj (Stack stack, int i); 
extern int IsGtkStatusbar(NspObject *O);
extern NspGtkStatusbar *GetGtkStatusbarCopy (Stack stack, int i); 
extern NspGtkStatusbar *GetGtkStatusbar (Stack stack, int i); 

#endif 
