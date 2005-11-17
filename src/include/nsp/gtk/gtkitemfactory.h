/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkItemFactory
#define INC_NSP_GtkItemFactory

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkobject.h"

/*
* NspGtkItemFactory inherits from NspGtkObject
* just change some type attributes 
*/

typedef NspGtkObject NspGtkItemFactory ;
typedef NspTypeGtkObject NspTypeGtkItemFactory ;

extern int nsp_type_gtkitemfactory_id;
extern NspTypeGtkItemFactory *nsp_type_gtkitemfactory;

/* type instances for gtkobject */

NspTypeGtkItemFactory *new_type_gtkitemfactory(type_mode mode);

/* instance for GtkItemFactory */

NspGtkItemFactory *new_gtkitemfactory();

/*
* Object methods redefined for gtkitemfactory 
*/

#define NULLGTKITEMFACTORY (NspGtkItemFactory*) 0

NspGtkItemFactory *gtkitemfactory_create(char *name,NspTypeBase *type);

/* from GtkItemFactoryObj.c */

extern NspGtkItemFactory *gtkitemfactory_object (NspObject *O); 
extern int IsGtkItemFactoryObj (Stack stack, int i); 
extern int IsGtkItemFactory(NspObject *O);
extern NspGtkItemFactory *GetGtkItemFactoryCopy (Stack stack, int i); 
extern NspGtkItemFactory *GetGtkItemFactory (Stack stack, int i); 

#endif 

#ifdef GtkItemFactory_Private 
static int init_gtkitemfactory(NspGtkItemFactory *o,NspTypeGtkItemFactory *type);
static char *gtkitemfactory_type_as_string(void);
static char *gtkitemfactory_type_short_string(void);
static AttrTab gtkitemfactory_attrs[];
/* static int int_gtkitemfactory_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkitemfactory_get_methods(void); 
#endif /* GtkItemFactory_Private */
