/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkDrawable
#define INC_NSP_GdkDrawable

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkDrawable inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkDrawable ;
typedef NspTypeGObject NspTypeGdkDrawable ;

extern int nsp_type_gdkdrawable_id;
extern NspTypeGdkDrawable *nsp_type_gdkdrawable;

/* type instances for gobject */

NspTypeGdkDrawable *new_type_gdkdrawable(type_mode mode);

/* instance for GdkDrawable */

NspGdkDrawable *new_gdkdrawable();

/*
* Object methods redefined for gdkdrawable 
*/

#define NULLGDKDRAWABLE (NspGdkDrawable*) 0

NspGdkDrawable *gdkdrawable_create(char *name,NspTypeBase *type);

/* from GdkDrawableObj.c */

extern NspGdkDrawable *gdkdrawable_object (NspObject *O); 
extern int IsGdkDrawableObj (Stack stack, int i); 
extern int IsGdkDrawable(NspObject *O);
extern NspGdkDrawable *GetGdkDrawableCopy (Stack stack, int i); 
extern NspGdkDrawable *GetGdkDrawable (Stack stack, int i); 

#endif 

#ifdef GdkDrawable_Private 
static int init_gdkdrawable(NspGdkDrawable *o,NspTypeGdkDrawable *type);
static char *gdkdrawable_type_as_string(void);
static char *gdkdrawable_type_short_string(NspObject *v);
static AttrTab gdkdrawable_attrs[];
/* static int int_gdkdrawable_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkdrawable_get_methods(void); 
#endif /* GdkDrawable_Private */
