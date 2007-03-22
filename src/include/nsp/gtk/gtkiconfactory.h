/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkIconFactory
#define INC_NSP_GtkIconFactory

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkIconFactory inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkIconFactory ;
typedef NspTypeGObject NspTypeGtkIconFactory ;

extern int nsp_type_gtkiconfactory_id;
extern NspTypeGtkIconFactory *nsp_type_gtkiconfactory;

/* type instances for gobject */

NspTypeGtkIconFactory *new_type_gtkiconfactory(type_mode mode);

/* instance for GtkIconFactory */

NspGtkIconFactory *new_gtkiconfactory();

/*
* Object methods redefined for gtkiconfactory 
*/

#define NULLGTKICONFACTORY (NspGtkIconFactory*) 0

NspGtkIconFactory *gtkiconfactory_create(char *name,NspTypeBase *type);

/* from GtkIconFactoryObj.c */

extern NspGtkIconFactory *gtkiconfactory_object (NspObject *O); 
extern int IsGtkIconFactoryObj (Stack stack, int i); 
extern int IsGtkIconFactory(NspObject *O);
extern NspGtkIconFactory *GetGtkIconFactoryCopy (Stack stack, int i); 
extern NspGtkIconFactory *GetGtkIconFactory (Stack stack, int i); 

#endif 

#ifdef GtkIconFactory_Private 
static int init_gtkiconfactory(NspGtkIconFactory *o,NspTypeGtkIconFactory *type);
static char *gtkiconfactory_type_as_string(void);
static char *gtkiconfactory_type_short_string(NspObject *v);
static AttrTab gtkiconfactory_attrs[];
/* static int int_gtkiconfactory_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkiconfactory_get_methods(void); 
#endif /* GtkIconFactory_Private */
