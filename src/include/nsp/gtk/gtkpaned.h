/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkPaned
#define INC_NSP_GtkPaned

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkPaned inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkPaned ;
typedef NspTypeGtkContainer NspTypeGtkPaned ;

extern int nsp_type_gtkpaned_id;
extern NspTypeGtkPaned *nsp_type_gtkpaned;

/* type instances for gtkcontainer */

NspTypeGtkPaned *new_type_gtkpaned(type_mode mode);

/* instance for GtkPaned */

NspGtkPaned *new_gtkpaned();

/*
* Object methods redefined for gtkpaned 
*/

#ifdef GtkPaned_Private 
static int init_gtkpaned(NspGtkPaned *o,NspTypeGtkPaned *type);
static char *gtkpaned_type_as_string(void);
static char *gtkpaned_type_short_string(void);
static AttrTab gtkpaned_attrs[];
/* static int int_gtkpaned_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkpaned_get_methods(void); 
#endif /* GtkPaned_Private */

#define NULLGTKPANED (NspGtkPaned*) 0

NspGtkPaned *gtkpaned_create(char *name,NspTypeBase *type);

/* from GtkPanedObj.c */

extern NspGtkPaned *gtkpaned_object (NspObject *O); 
extern int IsGtkPanedObj (Stack stack, int i); 
extern int IsGtkPaned(NspObject *O);
extern NspGtkPaned *GetGtkPanedCopy (Stack stack, int i); 
extern NspGtkPaned *GetGtkPaned (Stack stack, int i); 

#endif 
