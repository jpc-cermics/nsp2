/* -*- Mode: C -*- */
#ifndef INC_NSP_AtkObject
#define INC_NSP_AtkObject

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspAtkObject inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspAtkObject ;
typedef NspTypeGObject NspTypeAtkObject ;

extern int nsp_type_atkobject_id;
extern NspTypeAtkObject *nsp_type_atkobject;

/* type instances for gobject */

NspTypeAtkObject *new_type_atkobject(type_mode mode);

/* instance for AtkObject */

NspAtkObject *new_atkobject();

/*
* Object methods redefined for atkobject 
*/

#define NULLATKOBJECT (NspAtkObject*) 0

NspAtkObject *atkobject_create(char *name,NspTypeBase *type);

/* from AtkObjectObj.c */

extern NspAtkObject *atkobject_object (NspObject *O); 
extern int IsAtkObjectObj (Stack stack, int i); 
extern int IsAtkObject(NspObject *O);
extern NspAtkObject *GetAtkObjectCopy (Stack stack, int i); 
extern NspAtkObject *GetAtkObject (Stack stack, int i); 

#endif 

#ifdef AtkObject_Private 
static int init_atkobject(NspAtkObject *o,NspTypeAtkObject *type);
static char *atkobject_type_as_string(void);
static char *atkobject_type_short_string(NspObject *v);
static AttrTab atkobject_attrs[];
/* static int int_atkobject_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *atkobject_get_methods(void); 
#endif /* AtkObject_Private */
