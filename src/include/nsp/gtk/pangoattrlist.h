/* -*- Mode: C -*- */
#ifndef INC_NSP_PangoAttrList
#define INC_NSP_PangoAttrList

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspPangoAttrList inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspPangoAttrList ;
typedef NspTypeGBoxed NspTypePangoAttrList ;

extern int nsp_type_pangoattrlist_id;
extern NspTypePangoAttrList *nsp_type_pangoattrlist;

/* type instances for gboxed */

NspTypePangoAttrList *new_type_pangoattrlist(type_mode mode);

/* instance for PangoAttrList */

NspPangoAttrList *new_pangoattrlist();

/*
* Object methods redefined for pangoattrlist 
*/

#ifdef PangoAttrList_Private 
static int init_pangoattrlist(NspPangoAttrList *o,NspTypePangoAttrList *type);
static char *pangoattrlist_type_as_string(void);
static char *pangoattrlist_type_short_string(void);
static AttrTab pangoattrlist_attrs[];
/* static int int_pangoattrlist_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *pangoattrlist_get_methods(void); 
#endif /* PangoAttrList_Private */

#define NULLPANGOATTRLIST (NspPangoAttrList*) 0

NspPangoAttrList *pangoattrlist_create(char *name,NspTypeBase *type);

/* from PangoAttrListObj.c */

extern NspPangoAttrList *pangoattrlist_object (NspObject *O); 
extern int IsPangoAttrListObj (Stack stack, int i); 
extern int IsPangoAttrList(NspObject *O);
extern NspPangoAttrList *GetPangoAttrListCopy (Stack stack, int i); 
extern NspPangoAttrList *GetPangoAttrList (Stack stack, int i); 

#endif 
