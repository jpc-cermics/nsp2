/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkScreen
#define INC_NSP_GdkScreen

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkScreen inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkScreen ;
typedef NspTypeGObject NspTypeGdkScreen ;

extern int nsp_type_gdkscreen_id;
extern NspTypeGdkScreen *nsp_type_gdkscreen;

/* type instances for gobject */

NspTypeGdkScreen *new_type_gdkscreen(type_mode mode);

/* instance for GdkScreen */

NspGdkScreen *new_gdkscreen();

/*
* Object methods redefined for gdkscreen 
*/

#define NULLGDKSCREEN (NspGdkScreen*) 0

NspGdkScreen *gdkscreen_create(char *name,NspTypeBase *type);

/* from GdkScreenObj.c */

extern NspGdkScreen *gdkscreen_object (NspObject *O); 
extern int IsGdkScreenObj (Stack stack, int i); 
extern int IsGdkScreen(NspObject *O);
extern NspGdkScreen *GetGdkScreenCopy (Stack stack, int i); 
extern NspGdkScreen *GetGdkScreen (Stack stack, int i); 

#endif 

#ifdef GdkScreen_Private 
static int init_gdkscreen(NspGdkScreen *o,NspTypeGdkScreen *type);
static char *gdkscreen_type_as_string(void);
static char *gdkscreen_type_short_string(NspObject *v);
static AttrTab gdkscreen_attrs[];
/* static int int_gdkscreen_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkscreen_get_methods(void); 
#endif /* GdkScreen_Private */
