/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkList
#define INC_NSP_GtkList

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkList inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkList ;
typedef NspTypeGtkContainer NspTypeGtkList ;

extern int nsp_type_gtklist_id;
extern NspTypeGtkList *nsp_type_gtklist;

/* type instances for gtkcontainer */

NspTypeGtkList *new_type_gtklist(type_mode mode);

/* instance for GtkList */

NspGtkList *new_gtklist();

/*
* Object methods redefined for gtklist 
*/

#ifdef GtkList_Private 
static int init_gtklist(NspGtkList *o,NspTypeGtkList *type);
static char *gtklist_type_as_string(void);
static char *gtklist_type_short_string(void);
static AttrTab gtklist_attrs[];
/* static int int_gtklist_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtklist_get_methods(void); 
#endif /* GtkList_Private */

#define NULLGTKLIST (NspGtkList*) 0

NspGtkList *gtklist_create(char *name,NspTypeBase *type);

/* from GtkListObj.c */

extern NspGtkList *gtklist_object (NspObject *O); 
extern int IsGtkListObj (Stack stack, int i); 
extern int IsGtkList(NspObject *O);
extern NspGtkList *GetGtkListCopy (Stack stack, int i); 
extern NspGtkList *GetGtkList (Stack stack, int i); 

#endif 
