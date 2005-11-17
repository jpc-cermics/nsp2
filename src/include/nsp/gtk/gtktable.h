/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTable
#define INC_NSP_GtkTable

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkcontainer.h"

/*
* NspGtkTable inherits from NspGtkContainer
* just change some type attributes 
*/

typedef NspGtkContainer NspGtkTable ;
typedef NspTypeGtkContainer NspTypeGtkTable ;

extern int nsp_type_gtktable_id;
extern NspTypeGtkTable *nsp_type_gtktable;

/* type instances for gtkcontainer */

NspTypeGtkTable *new_type_gtktable(type_mode mode);

/* instance for GtkTable */

NspGtkTable *new_gtktable();

/*
* Object methods redefined for gtktable 
*/

#define NULLGTKTABLE (NspGtkTable*) 0

NspGtkTable *gtktable_create(char *name,NspTypeBase *type);

/* from GtkTableObj.c */

extern NspGtkTable *gtktable_object (NspObject *O); 
extern int IsGtkTableObj (Stack stack, int i); 
extern int IsGtkTable(NspObject *O);
extern NspGtkTable *GetGtkTableCopy (Stack stack, int i); 
extern NspGtkTable *GetGtkTable (Stack stack, int i); 

#endif 

#ifdef GtkTable_Private 
static int init_gtktable(NspGtkTable *o,NspTypeGtkTable *type);
static char *gtktable_type_as_string(void);
static char *gtktable_type_short_string(void);
static AttrTab gtktable_attrs[];
/* static int int_gtktable_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktable_get_methods(void); 
#endif /* GtkTable_Private */
