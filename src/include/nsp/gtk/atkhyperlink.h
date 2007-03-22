/* -*- Mode: C -*- */
#ifndef INC_NSP_AtkHyperlink
#define INC_NSP_AtkHyperlink

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspAtkHyperlink inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspAtkHyperlink ;
typedef NspTypeGObject NspTypeAtkHyperlink ;

extern int nsp_type_atkhyperlink_id;
extern NspTypeAtkHyperlink *nsp_type_atkhyperlink;

/* type instances for gobject */

NspTypeAtkHyperlink *new_type_atkhyperlink(type_mode mode);

/* instance for AtkHyperlink */

NspAtkHyperlink *new_atkhyperlink();

/*
* Object methods redefined for atkhyperlink 
*/

#define NULLATKHYPERLINK (NspAtkHyperlink*) 0

NspAtkHyperlink *atkhyperlink_create(char *name,NspTypeBase *type);

/* from AtkHyperlinkObj.c */

extern NspAtkHyperlink *atkhyperlink_object (NspObject *O); 
extern int IsAtkHyperlinkObj (Stack stack, int i); 
extern int IsAtkHyperlink(NspObject *O);
extern NspAtkHyperlink *GetAtkHyperlinkCopy (Stack stack, int i); 
extern NspAtkHyperlink *GetAtkHyperlink (Stack stack, int i); 

#endif 

#ifdef AtkHyperlink_Private 
static int init_atkhyperlink(NspAtkHyperlink *o,NspTypeAtkHyperlink *type);
static char *atkhyperlink_type_as_string(void);
static char *atkhyperlink_type_short_string(NspObject *v);
static AttrTab atkhyperlink_attrs[];
/* static int int_atkhyperlink_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *atkhyperlink_get_methods(void); 
#endif /* AtkHyperlink_Private */
