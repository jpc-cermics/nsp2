/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkCList
#define INC_NSP_GtkCList

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkCList inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkCList ;
typedef NspTypeGtkContainer NspTypeGtkCList ;

extern int nsp_type_gtkclist_id;
extern NspTypeGtkCList *nsp_type_gtkclist;

/* type instances for gtkcontainer */

NspTypeGtkCList *new_type_gtkclist(type_mode mode);

/* instance for GtkCList */

NspGtkCList *new_gtkclist();

/*
* Object methods redefined for gtkclist 
*/

#ifdef GtkCList_Private 
static int init_gtkclist(NspGtkCList *o,NspTypeGtkCList *type);
static char *gtkclist_type_as_string(void);
static char *gtkclist_type_short_string(NspObject *v);
static AttrTab gtkclist_attrs[];
/* static int int_gtkclist_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkclist_get_methods(void); 
#endif /* GtkCList_Private */

#define NULLGTKCLIST (NspGtkCList*) 0

NspGtkCList *gtkclist_create(char *name,NspTypeBase *type);

/* from GtkCListObj.c */

extern NspGtkCList *gtkclist_object (NspObject *O); 
extern int IsGtkCListObj (Stack stack, int i); 
extern int IsGtkCList(NspObject *O);
extern NspGtkCList *GetGtkCListCopy (Stack stack, int i); 
extern NspGtkCList *GetGtkCList (Stack stack, int i); 

#endif 
