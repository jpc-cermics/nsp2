/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkTreePath
#define INC_NSP_GdkTreePath

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGdkTreePath inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGdkTreePath ;
typedef NspTypeGBoxed NspTypeGdkTreePath ;

extern int nsp_type_gdktreepath_id;
extern NspTypeGdkTreePath *nsp_type_gdktreepath;

/* type instances for gboxed */

NspTypeGdkTreePath *new_type_gdktreepath(type_mode mode);

/* instance for GdkTreePath */

NspGdkTreePath *new_gdktreepath();

/*
* Object methods redefined for gdktreepath 
*/

#ifdef GdkTreePath_Private 
static int init_gdktreepath(NspGdkTreePath *o,NspTypeGdkTreePath *type);
static char *gdktreepath_type_as_string(void);
static char *gdktreepath_type_short_string(void);
static AttrTab gdktreepath_attrs[];
/* static int int_gdktreepath_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdktreepath_get_methods(void); 
#endif /* GdkTreePath_Private */

#define NULLGDKTREEPATH (NspGdkTreePath*) 0

NspGdkTreePath *gdktreepath_create(char *name,NspTypeBase *type);

/* from GdkTreePathObj.c */

extern NspGdkTreePath *gdktreepath_object (NspObject *O); 
extern int IsGdkTreePathObj (Stack stack, int i); 
extern int IsGdkTreePath(NspObject *O);
extern NspGdkTreePath *GetGdkTreePathCopy (Stack stack, int i); 
extern NspGdkTreePath *GetGdkTreePath (Stack stack, int i); 

#endif 
