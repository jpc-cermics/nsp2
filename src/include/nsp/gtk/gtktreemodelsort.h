/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeModelSort
#define INC_NSP_GtkTreeModelSort

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkTreeModelSort inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkTreeModelSort ;
typedef NspTypeGObject NspTypeGtkTreeModelSort ;

extern int nsp_type_gtktreemodelsort_id;
extern NspTypeGtkTreeModelSort *nsp_type_gtktreemodelsort;

/* type instances for gobject */

NspTypeGtkTreeModelSort *new_type_gtktreemodelsort(type_mode mode);

/* instance for GtkTreeModelSort */

NspGtkTreeModelSort *new_gtktreemodelsort();

/*
* Object methods redefined for gtktreemodelsort 
*/

#define NULLGTKTREEMODELSORT (NspGtkTreeModelSort*) 0

NspGtkTreeModelSort *gtktreemodelsort_create(char *name,NspTypeBase *type);

/* from GtkTreeModelSortObj.c */

extern NspGtkTreeModelSort *gtktreemodelsort_object (NspObject *O); 
extern int IsGtkTreeModelSortObj (Stack stack, int i); 
extern int IsGtkTreeModelSort(NspObject *O);
extern NspGtkTreeModelSort *GetGtkTreeModelSortCopy (Stack stack, int i); 
extern NspGtkTreeModelSort *GetGtkTreeModelSort (Stack stack, int i); 

#endif 

#ifdef GtkTreeModelSort_Private 
static int init_gtktreemodelsort(NspGtkTreeModelSort *o,NspTypeGtkTreeModelSort *type);
static char *gtktreemodelsort_type_as_string(void);
static char *gtktreemodelsort_type_short_string(NspObject *v);
static AttrTab gtktreemodelsort_attrs[];
/* static int int_gtktreemodelsort_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreemodelsort_get_methods(void); 
#endif /* GtkTreeModelSort_Private */
