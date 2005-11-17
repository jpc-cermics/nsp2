/* -*- Mode: C -*- */
#ifndef INC_NSP_AtkNoOpObject
#define INC_NSP_AtkNoOpObject

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/atkobject.h"

/*
* NspAtkNoOpObject inherits from NspAtkObject
* just change some type attributes 
*/

typedef NspAtkObject NspAtkNoOpObject ;
typedef NspTypeAtkObject NspTypeAtkNoOpObject ;

extern int nsp_type_atknoopobject_id;
extern NspTypeAtkNoOpObject *nsp_type_atknoopobject;

/* type instances for atkobject */

NspTypeAtkNoOpObject *new_type_atknoopobject(type_mode mode);

/* instance for AtkNoOpObject */

NspAtkNoOpObject *new_atknoopobject();

/*
* Object methods redefined for atknoopobject 
*/

#define NULLATKNOOPOBJECT (NspAtkNoOpObject*) 0

NspAtkNoOpObject *atknoopobject_create(char *name,NspTypeBase *type);

/* from AtkNoOpObjectObj.c */

extern NspAtkNoOpObject *atknoopobject_object (NspObject *O); 
extern int IsAtkNoOpObjectObj (Stack stack, int i); 
extern int IsAtkNoOpObject(NspObject *O);
extern NspAtkNoOpObject *GetAtkNoOpObjectCopy (Stack stack, int i); 
extern NspAtkNoOpObject *GetAtkNoOpObject (Stack stack, int i); 

#endif 

#ifdef AtkNoOpObject_Private 
static int init_atknoopobject(NspAtkNoOpObject *o,NspTypeAtkNoOpObject *type);
static char *atknoopobject_type_as_string(void);
static char *atknoopobject_type_short_string(void);
static AttrTab atknoopobject_attrs[];
/* static int int_atknoopobject_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *atknoopobject_get_methods(void); 
#endif /* AtkNoOpObject_Private */
