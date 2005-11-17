/* -*- Mode: C -*- */
#ifndef INC_NSP_AtkNoOpObjectFactory
#define INC_NSP_AtkNoOpObjectFactory

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/atkobjectfactory.h"

/*
* NspAtkNoOpObjectFactory inherits from NspAtkObjectFactory
* just change some type attributes 
*/

typedef NspAtkObjectFactory NspAtkNoOpObjectFactory ;
typedef NspTypeAtkObjectFactory NspTypeAtkNoOpObjectFactory ;

extern int nsp_type_atknoopobjectfactory_id;
extern NspTypeAtkNoOpObjectFactory *nsp_type_atknoopobjectfactory;

/* type instances for atkobjectfactory */

NspTypeAtkNoOpObjectFactory *new_type_atknoopobjectfactory(type_mode mode);

/* instance for AtkNoOpObjectFactory */

NspAtkNoOpObjectFactory *new_atknoopobjectfactory();

/*
* Object methods redefined for atknoopobjectfactory 
*/

#define NULLATKNOOPOBJECTFACTORY (NspAtkNoOpObjectFactory*) 0

NspAtkNoOpObjectFactory *atknoopobjectfactory_create(char *name,NspTypeBase *type);

/* from AtkNoOpObjectFactoryObj.c */

extern NspAtkNoOpObjectFactory *atknoopobjectfactory_object (NspObject *O); 
extern int IsAtkNoOpObjectFactoryObj (Stack stack, int i); 
extern int IsAtkNoOpObjectFactory(NspObject *O);
extern NspAtkNoOpObjectFactory *GetAtkNoOpObjectFactoryCopy (Stack stack, int i); 
extern NspAtkNoOpObjectFactory *GetAtkNoOpObjectFactory (Stack stack, int i); 

#endif 

#ifdef AtkNoOpObjectFactory_Private 
static int init_atknoopobjectfactory(NspAtkNoOpObjectFactory *o,NspTypeAtkNoOpObjectFactory *type);
static char *atknoopobjectfactory_type_as_string(void);
static char *atknoopobjectfactory_type_short_string(void);
static AttrTab atknoopobjectfactory_attrs[];
/* static int int_atknoopobjectfactory_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *atknoopobjectfactory_get_methods(void); 
#endif /* AtkNoOpObjectFactory_Private */
