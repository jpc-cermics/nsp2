/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkContainer
#define INC_NSP_GtkContainer

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkContainer inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkContainer ;
typedef NspTypeGtkWidget NspTypeGtkContainer ;

extern int nsp_type_gtkcontainer_id;
extern NspTypeGtkContainer *nsp_type_gtkcontainer;

/* type instances for gtkwidget */

NspTypeGtkContainer *new_type_gtkcontainer(type_mode mode);

/* instance for GtkContainer */

NspGtkContainer *new_gtkcontainer();

/*
* Object methods redefined for gtkcontainer 
*/

#ifdef GtkContainer_Private 
static int init_gtkcontainer(NspGtkContainer *o,NspTypeGtkContainer *type);
static char *gtkcontainer_type_as_string(void);
static char *gtkcontainer_type_short_string(void);
static AttrTab gtkcontainer_attrs[];
/* static int int_gtkcontainer_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcontainer_get_methods(void); 
#endif /* GtkContainer_Private */

#define NULLGTKCONTAINER (NspGtkContainer*) 0

NspGtkContainer *gtkcontainer_create(char *name,NspTypeBase *type);

/* from GtkContainerObj.c */

extern NspGtkContainer *gtkcontainer_object (NspObject *O); 
extern int IsGtkContainerObj (Stack stack, int i); 
extern int IsGtkContainer(NspObject *O);
extern NspGtkContainer *GetGtkContainerCopy (Stack stack, int i); 
extern NspGtkContainer *GetGtkContainer (Stack stack, int i); 

#endif 
