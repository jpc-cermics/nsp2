/* -*- Mode: C -*- */
#ifndef INC_NSP_AtkStateSet
#define INC_NSP_AtkStateSet

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspAtkStateSet inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspAtkStateSet ;
typedef NspTypeGObject NspTypeAtkStateSet ;

extern int nsp_type_atkstateset_id;
extern NspTypeAtkStateSet *nsp_type_atkstateset;

/* type instances for gobject */

NspTypeAtkStateSet *new_type_atkstateset(type_mode mode);

/* instance for AtkStateSet */

NspAtkStateSet *new_atkstateset();

/*
* Object methods redefined for atkstateset 
*/

#define NULLATKSTATESET (NspAtkStateSet*) 0

NspAtkStateSet *atkstateset_create(char *name,NspTypeBase *type);

/* from AtkStateSetObj.c */

extern NspAtkStateSet *atkstateset_object (NspObject *O); 
extern int IsAtkStateSetObj (Stack stack, int i); 
extern int IsAtkStateSet(NspObject *O);
extern NspAtkStateSet *GetAtkStateSetCopy (Stack stack, int i); 
extern NspAtkStateSet *GetAtkStateSet (Stack stack, int i); 

#endif 

#ifdef AtkStateSet_Private 
static int init_atkstateset(NspAtkStateSet *o,NspTypeAtkStateSet *type);
static char *atkstateset_type_as_string(void);
static char *atkstateset_type_short_string(void);
static AttrTab atkstateset_attrs[];
/* static int int_atkstateset_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *atkstateset_get_methods(void); 
#endif /* AtkStateSet_Private */
