/* -*- Mode: C -*- */
#ifndef INC_NSP_AtkUtil
#define INC_NSP_AtkUtil

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspAtkUtil inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspAtkUtil ;
typedef NspTypeGObject NspTypeAtkUtil ;

extern int nsp_type_atkutil_id;
extern NspTypeAtkUtil *nsp_type_atkutil;

/* type instances for gobject */

NspTypeAtkUtil *new_type_atkutil(type_mode mode);

/* instance for AtkUtil */

NspAtkUtil *new_atkutil();

/*
* Object methods redefined for atkutil 
*/

#define NULLATKUTIL (NspAtkUtil*) 0

NspAtkUtil *atkutil_create(char *name,NspTypeBase *type);

/* from AtkUtilObj.c */

extern NspAtkUtil *atkutil_object (NspObject *O); 
extern int IsAtkUtilObj (Stack stack, int i); 
extern int IsAtkUtil(NspObject *O);
extern NspAtkUtil *GetAtkUtilCopy (Stack stack, int i); 
extern NspAtkUtil *GetAtkUtil (Stack stack, int i); 

#endif 

#ifdef AtkUtil_Private 
static int init_atkutil(NspAtkUtil *o,NspTypeAtkUtil *type);
static char *atkutil_type_as_string(void);
static char *atkutil_type_short_string(NspObject *v);
static AttrTab atkutil_attrs[];
/* static int int_atkutil_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *atkutil_get_methods(void); 
#endif /* AtkUtil_Private */
