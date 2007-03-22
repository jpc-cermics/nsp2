/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkDisplay
#define INC_NSP_GdkDisplay

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gobject.h"

/*
* NspGdkDisplay inherits from NspGObject
* just change some type attributes 
*/

typedef NspGObject NspGdkDisplay ;
typedef NspTypeGObject NspTypeGdkDisplay ;

extern int nsp_type_gdkdisplay_id;
extern NspTypeGdkDisplay *nsp_type_gdkdisplay;

/* type instances for gobject */

NspTypeGdkDisplay *new_type_gdkdisplay(type_mode mode);

/* instance for GdkDisplay */

NspGdkDisplay *new_gdkdisplay();

/*
* Object methods redefined for gdkdisplay 
*/

#define NULLGDKDISPLAY (NspGdkDisplay*) 0

NspGdkDisplay *gdkdisplay_create(char *name,NspTypeBase *type);

/* from GdkDisplayObj.c */

extern NspGdkDisplay *gdkdisplay_object (NspObject *O); 
extern int IsGdkDisplayObj (Stack stack, int i); 
extern int IsGdkDisplay(NspObject *O);
extern NspGdkDisplay *GetGdkDisplayCopy (Stack stack, int i); 
extern NspGdkDisplay *GetGdkDisplay (Stack stack, int i); 

#endif 

#ifdef GdkDisplay_Private 
static int init_gdkdisplay(NspGdkDisplay *o,NspTypeGdkDisplay *type);
static char *gdkdisplay_type_as_string(void);
static char *gdkdisplay_type_short_string(NspObject *v);
static AttrTab gdkdisplay_attrs[];
/* static int int_gdkdisplay_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkdisplay_get_methods(void); 
#endif /* GdkDisplay_Private */
