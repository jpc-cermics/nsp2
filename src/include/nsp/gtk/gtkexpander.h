/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkExpander
#define INC_NSP_GtkExpander

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkExpander inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkExpander ;
typedef NspTypeGtkBin NspTypeGtkExpander ;

extern int nsp_type_gtkexpander_id;
extern NspTypeGtkExpander *nsp_type_gtkexpander;

/* type instances for gtkbin */

NspTypeGtkExpander *new_type_gtkexpander(type_mode mode);

/* instance for GtkExpander */

NspGtkExpander *new_gtkexpander();

/*
* Object methods redefined for gtkexpander 
*/

#define NULLGTKEXPANDER (NspGtkExpander*) 0

NspGtkExpander *gtkexpander_create(char *name,NspTypeBase *type);

/* from GtkExpanderObj.c */

extern NspGtkExpander *gtkexpander_object (NspObject *O); 
extern int IsGtkExpanderObj (Stack stack, int i); 
extern int IsGtkExpander(NspObject *O);
extern NspGtkExpander *GetGtkExpanderCopy (Stack stack, int i); 
extern NspGtkExpander *GetGtkExpander (Stack stack, int i); 

#endif 

#ifdef GtkExpander_Private 
static int init_gtkexpander(NspGtkExpander *o,NspTypeGtkExpander *type);
static char *gtkexpander_type_as_string(void);
static char *gtkexpander_type_short_string(NspObject *v);
static AttrTab gtkexpander_attrs[];
/* static int int_gtkexpander_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkexpander_get_methods(void); 
#endif /* GtkExpander_Private */
