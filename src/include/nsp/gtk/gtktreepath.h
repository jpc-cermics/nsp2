/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkTreePath
#define INC_NSP_GtkTreePath

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGtkTreePath inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGtkTreePath ;
typedef NspTypeGBoxed NspTypeGtkTreePath ;

extern int nsp_type_gtktreepath_id;
extern NspTypeGtkTreePath *nsp_type_gtktreepath;

/* type instances for gboxed */

NspTypeGtkTreePath *new_type_gtktreepath(type_mode mode);

/* instance for GtkTreePath */

NspGtkTreePath *new_gtktreepath();

/*
* Object methods redefined for gtktreepath 
*/

#define NULLGTKTREEPATH (NspGtkTreePath*) 0

NspGtkTreePath *gtktreepath_create(char *name,NspTypeBase *type);

/* from GtkTreePathObj.c */

extern NspGtkTreePath *gtktreepath_object (NspObject *O); 
extern int IsGtkTreePathObj (Stack stack, int i); 
extern int IsGtkTreePath(NspObject *O);
extern NspGtkTreePath *GetGtkTreePathCopy (Stack stack, int i); 
extern NspGtkTreePath *GetGtkTreePath (Stack stack, int i); 

#endif 

#ifdef GtkTreePath_Private 
static int init_gtktreepath(NspGtkTreePath *o,NspTypeGtkTreePath *type);
static char *gtktreepath_type_as_string(void);
static char *gtktreepath_type_short_string(NspObject *v);
static AttrTab gtktreepath_attrs[];
/* static int int_gtktreepath_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtktreepath_get_methods(void); 
#endif /* GtkTreePath_Private */
