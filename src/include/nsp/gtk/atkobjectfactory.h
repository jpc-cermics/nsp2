/* -*- Mode: C -*- */
#ifndef INC_NSP_AtkObjectFactory
#define INC_NSP_AtkObjectFactory

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspAtkObjectFactory inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspAtkObjectFactory ;
typedef NspTypeGObject NspTypeAtkObjectFactory ;

extern int nsp_type_atkobjectfactory_id;
extern NspTypeAtkObjectFactory *nsp_type_atkobjectfactory;

/* type instances for gobject */

NspTypeAtkObjectFactory *new_type_atkobjectfactory(type_mode mode);

/* instance for AtkObjectFactory */

NspAtkObjectFactory *new_atkobjectfactory();

/*
* Object methods redefined for atkobjectfactory 
*/

#ifdef AtkObjectFactory_Private 
static int init_atkobjectfactory(NspAtkObjectFactory *o,NspTypeAtkObjectFactory *type);
static char *atkobjectfactory_type_as_string(void);
static char *atkobjectfactory_type_short_string(void);
static AttrTab atkobjectfactory_attrs[];
/* static int int_atkobjectfactory_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *atkobjectfactory_get_methods(void); 
#endif /* AtkObjectFactory_Private */

#define NULLATKOBJECTFACTORY (NspAtkObjectFactory*) 0

NspAtkObjectFactory *atkobjectfactory_create(char *name,NspTypeBase *type);

/* from AtkObjectFactoryObj.c */

extern NspAtkObjectFactory *atkobjectfactory_object (NspObject *O); 
extern int IsAtkObjectFactoryObj (Stack stack, int i); 
extern int IsAtkObjectFactory(NspObject *O);
extern NspAtkObjectFactory *GetAtkObjectFactoryCopy (Stack stack, int i); 
extern NspAtkObjectFactory *GetAtkObjectFactory (Stack stack, int i); 

#endif 
