/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkCursor
#define INC_NSP_GdkCursor

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGdkCursor inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGdkCursor ;
typedef NspTypeGBoxed NspTypeGdkCursor ;

extern int nsp_type_gdkcursor_id;
extern NspTypeGdkCursor *nsp_type_gdkcursor;

/* type instances for gboxed */

NspTypeGdkCursor *new_type_gdkcursor(type_mode mode);

/* instance for GdkCursor */

NspGdkCursor *new_gdkcursor();

/*
* Object methods redefined for gdkcursor 
*/

#ifdef GdkCursor_Private 
static int init_gdkcursor(NspGdkCursor *o,NspTypeGdkCursor *type);
static char *gdkcursor_type_as_string(void);
static char *gdkcursor_type_short_string(void);
static AttrTab gdkcursor_attrs[];
/* static int int_gdkcursor_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkcursor_get_methods(void); 
#endif /* GdkCursor_Private */

#define NULLGDKCURSOR (NspGdkCursor*) 0

NspGdkCursor *gdkcursor_create(char *name,NspTypeBase *type);

/* from GdkCursorObj.c */

extern NspGdkCursor *gdkcursor_object (NspObject *O); 
extern int IsGdkCursorObj (Stack stack, int i); 
extern int IsGdkCursor(NspObject *O);
extern NspGdkCursor *GetGdkCursorCopy (Stack stack, int i); 
extern NspGdkCursor *GetGdkCursor (Stack stack, int i); 

#endif 
