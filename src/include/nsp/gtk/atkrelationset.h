/* -*- Mode: C -*- */
#ifndef INC_NSP_AtkRelationSet
#define INC_NSP_AtkRelationSet

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspAtkRelationSet inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspAtkRelationSet ;
typedef NspTypeGObject NspTypeAtkRelationSet ;

extern int nsp_type_atkrelationset_id;
extern NspTypeAtkRelationSet *nsp_type_atkrelationset;

/* type instances for gobject */

NspTypeAtkRelationSet *new_type_atkrelationset(type_mode mode);

/* instance for AtkRelationSet */

NspAtkRelationSet *new_atkrelationset();

/*
* Object methods redefined for atkrelationset 
*/

#ifdef AtkRelationSet_Private 
static int init_atkrelationset(NspAtkRelationSet *o,NspTypeAtkRelationSet *type);
static char *atkrelationset_type_as_string(void);
static char *atkrelationset_type_short_string(void);
static AttrTab atkrelationset_attrs[];
/* static int int_atkrelationset_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *atkrelationset_get_methods(void); 
#endif /* AtkRelationSet_Private */

#define NULLATKRELATIONSET (NspAtkRelationSet*) 0

NspAtkRelationSet *atkrelationset_create(char *name,NspTypeBase *type);

/* from AtkRelationSetObj.c */

extern NspAtkRelationSet *atkrelationset_object (NspObject *O); 
extern int IsAtkRelationSetObj (Stack stack, int i); 
extern int IsAtkRelationSet(NspObject *O);
extern NspAtkRelationSet *GetAtkRelationSetCopy (Stack stack, int i); 
extern NspAtkRelationSet *GetAtkRelationSet (Stack stack, int i); 

#endif 
