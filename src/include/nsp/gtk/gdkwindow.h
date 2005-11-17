/* -*- Mode: C -*- */
#ifndef INC_NSP_GdkWindow
#define INC_NSP_GdkWindow

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gdkdrawable.h"

/*
* NspGdkWindow inherits from NspGdkDrawable
* just change some type attributes 
*/

typedef NspGdkDrawable NspGdkWindow ;
typedef NspTypeGdkDrawable NspTypeGdkWindow ;

extern int nsp_type_gdkwindow_id;
extern NspTypeGdkWindow *nsp_type_gdkwindow;

/* type instances for gdkdrawable */

NspTypeGdkWindow *new_type_gdkwindow(type_mode mode);

/* instance for GdkWindow */

NspGdkWindow *new_gdkwindow();

/*
* Object methods redefined for gdkwindow 
*/

#define NULLGDKWINDOW (NspGdkWindow*) 0

NspGdkWindow *gdkwindow_create(char *name,NspTypeBase *type);

/* from GdkWindowObj.c */

extern NspGdkWindow *gdkwindow_object (NspObject *O); 
extern int IsGdkWindowObj (Stack stack, int i); 
extern int IsGdkWindow(NspObject *O);
extern NspGdkWindow *GetGdkWindowCopy (Stack stack, int i); 
extern NspGdkWindow *GetGdkWindow (Stack stack, int i); 

#endif 

#ifdef GdkWindow_Private 
static int init_gdkwindow(NspGdkWindow *o,NspTypeGdkWindow *type);
static char *gdkwindow_type_as_string(void);
static char *gdkwindow_type_short_string(void);
static AttrTab gdkwindow_attrs[];
/* static int int_gdkwindow_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gdkwindow_get_methods(void); 
#endif /* GdkWindow_Private */
