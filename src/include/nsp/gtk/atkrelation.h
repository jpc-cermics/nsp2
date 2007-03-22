/* -*- Mode: C -*- */
#ifndef INC_NSP_AtkRelation
#define INC_NSP_AtkRelation

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspAtkRelation inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspAtkRelation ;
typedef NspTypeGObject NspTypeAtkRelation ;

extern int nsp_type_atkrelation_id;
extern NspTypeAtkRelation *nsp_type_atkrelation;

/* type instances for gobject */

NspTypeAtkRelation *new_type_atkrelation(type_mode mode);

/* instance for AtkRelation */

NspAtkRelation *new_atkrelation();

/*
* Object methods redefined for atkrelation 
*/

#define NULLATKRELATION (NspAtkRelation*) 0

NspAtkRelation *atkrelation_create(char *name,NspTypeBase *type);

/* from AtkRelationObj.c */

extern NspAtkRelation *atkrelation_object (NspObject *O); 
extern int IsAtkRelationObj (Stack stack, int i); 
extern int IsAtkRelation(NspObject *O);
extern NspAtkRelation *GetAtkRelationCopy (Stack stack, int i); 
extern NspAtkRelation *GetAtkRelation (Stack stack, int i); 

#endif 

#ifdef AtkRelation_Private 
static int init_atkrelation(NspAtkRelation *o,NspTypeAtkRelation *type);
static char *atkrelation_type_as_string(void);
static char *atkrelation_type_short_string(NspObject *v);
static AttrTab atkrelation_attrs[];
/* static int int_atkrelation_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *atkrelation_get_methods(void); 
#endif /* AtkRelation_Private */
