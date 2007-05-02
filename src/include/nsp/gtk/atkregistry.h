/* -*- Mode: C -*- */
#ifndef INC_NSP_AtkRegistry
#define INC_NSP_AtkRegistry

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspAtkRegistry inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspAtkRegistry ;
typedef NspTypeGObject NspTypeAtkRegistry ;

extern int nsp_type_atkregistry_id;
extern NspTypeAtkRegistry *nsp_type_atkregistry;

/* type instances for gobject */

NspTypeAtkRegistry *new_type_atkregistry(type_mode mode);

/* instance for AtkRegistry */

NspAtkRegistry *new_atkregistry();

/*
* Object methods redefined for atkregistry 
*/

#define NULLATKREGISTRY (NspAtkRegistry*) 0

NspAtkRegistry *atkregistry_create(char *name,NspTypeBase *type);

/* from AtkRegistryObj.c */

extern NspAtkRegistry *atkregistry_object (NspObject *O); 
extern int IsAtkRegistryObj (Stack stack, int i); 
extern int IsAtkRegistry(NspObject *O);
extern NspAtkRegistry *GetAtkRegistryCopy (Stack stack, int i); 
extern NspAtkRegistry *GetAtkRegistry (Stack stack, int i); 

#endif 

#ifdef AtkRegistry_Private 
static int init_atkregistry(NspAtkRegistry *o,NspTypeAtkRegistry *type);
static char *atkregistry_type_as_string(void);
static char *atkregistry_type_short_string(NspObject *v);
static AttrTab atkregistry_attrs[];
/* static int int_atkregistry_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *atkregistry_get_methods(void); 
#endif /* AtkRegistry_Private */
