/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreeSelection
#define INC_NSP_GtkTreeSelection

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkobject.h"

/*
* NspGtkTreeSelection inherits from NspGtkObject
* just change some type attributes 
*/

typedef NspGtkObject NspGtkTreeSelection ;
typedef NspTypeGtkObject NspTypeGtkTreeSelection ;

extern int nsp_type_gtktreeselection_id;
extern NspTypeGtkTreeSelection *nsp_type_gtktreeselection;

/* type instances for gtkobject */

NspTypeGtkTreeSelection *new_type_gtktreeselection(type_mode mode);

/* instance for GtkTreeSelection */

NspGtkTreeSelection *new_gtktreeselection();

/*
* Object methods redefined for gtktreeselection 
*/

#ifdef GtkTreeSelection_Private 
static int init_gtktreeselection(NspGtkTreeSelection *o,NspTypeGtkTreeSelection *type);
static char *gtktreeselection_type_as_string(void);
static char *gtktreeselection_type_short_string(void);
static AttrTab gtktreeselection_attrs[];
/* static int int_gtktreeselection_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreeselection_get_methods(void); 
#endif /* GtkTreeSelection_Private */

#define NULLGTKTREESELECTION (NspGtkTreeSelection*) 0

NspGtkTreeSelection *gtktreeselection_create(char *name,NspTypeBase *type);

/* from GtkTreeSelectionObj.c */

extern NspGtkTreeSelection *gtktreeselection_object (NspObject *O); 
extern int IsGtkTreeSelectionObj (Stack stack, int i); 
extern int IsGtkTreeSelection(NspObject *O);
extern NspGtkTreeSelection *GetGtkTreeSelectionCopy (Stack stack, int i); 
extern NspGtkTreeSelection *GetGtkTreeSelection (Stack stack, int i); 

#endif 
