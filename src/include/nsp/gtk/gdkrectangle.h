/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkRectangle
#define INC_NSP_GdkRectangle

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gboxed.h"

/*
* NspGdkRectangle inherits from NspGBoxed
* just change some type attributes 
*/

typedef NspGBoxed NspGdkRectangle ;
typedef NspTypeGBoxed NspTypeGdkRectangle ;

extern int nsp_type_gdkrectangle_id;
extern NspTypeGdkRectangle *nsp_type_gdkrectangle;

/* type instances for gboxed */

NspTypeGdkRectangle *new_type_gdkrectangle(type_mode mode);

/* instance for GdkRectangle */

NspGdkRectangle *new_gdkrectangle();

/*
* Object methods redefined for gdkrectangle 
*/

#ifdef GdkRectangle_Private 
static int init_gdkrectangle(NspGdkRectangle *o,NspTypeGdkRectangle *type);
static char *gdkrectangle_type_as_string(void);
static char *gdkrectangle_type_short_string(void);
static AttrTab gdkrectangle_attrs[];
/* static int int_gdkrectangle_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkrectangle_get_methods(void); 
#endif /* GdkRectangle_Private */

#define NULLGDKRECTANGLE (NspGdkRectangle*) 0

NspGdkRectangle *gdkrectangle_create(char *name,NspTypeBase *type);

/* from GdkRectangleObj.c */

extern NspGdkRectangle *gdkrectangle_object (NspObject *O); 
extern int IsGdkRectangleObj (Stack stack, int i); 
extern int IsGdkRectangle(NspObject *O);
extern NspGdkRectangle *GetGdkRectangleCopy (Stack stack, int i); 
extern NspGdkRectangle *GetGdkRectangle (Stack stack, int i); 

#endif 
