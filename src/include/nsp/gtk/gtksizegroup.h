/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkSizeGroup
#define INC_NSP_GtkSizeGroup

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGtkSizeGroup inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGtkSizeGroup ;
typedef NspTypeGObject NspTypeGtkSizeGroup ;

extern int nsp_type_gtksizegroup_id;
extern NspTypeGtkSizeGroup *nsp_type_gtksizegroup;

/* type instances for gobject */

NspTypeGtkSizeGroup *new_type_gtksizegroup(type_mode mode);

/* instance for GtkSizeGroup */

NspGtkSizeGroup *new_gtksizegroup();

/*
* Object methods redefined for gtksizegroup 
*/

#ifdef GtkSizeGroup_Private 
static int init_gtksizegroup(NspGtkSizeGroup *o,NspTypeGtkSizeGroup *type);
static char *gtksizegroup_type_as_string(void);
static char *gtksizegroup_type_short_string(void);
static AttrTab gtksizegroup_attrs[];
/* static int int_gtksizegroup_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtksizegroup_get_methods(void); 
#endif /* GtkSizeGroup_Private */

#define NULLGTKSIZEGROUP (NspGtkSizeGroup*) 0

NspGtkSizeGroup *gtksizegroup_create(char *name,NspTypeBase *type);

/* from GtkSizeGroupObj.c */

extern NspGtkSizeGroup *gtksizegroup_object (NspObject *O); 
extern int IsGtkSizeGroupObj (Stack stack, int i); 
extern int IsGtkSizeGroup(NspObject *O);
extern NspGtkSizeGroup *GetGtkSizeGroupCopy (Stack stack, int i); 
extern NspGtkSizeGroup *GetGtkSizeGroup (Stack stack, int i); 

#endif 
