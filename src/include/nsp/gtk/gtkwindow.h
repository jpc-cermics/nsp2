/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkWindow
#define INC_NSP_GtkWindow

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkWindow inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkWindow ;
typedef NspTypeGtkBin NspTypeGtkWindow ;

extern int nsp_type_gtkwindow_id;
extern NspTypeGtkWindow *nsp_type_gtkwindow;

/* type instances for gtkbin */

NspTypeGtkWindow *new_type_gtkwindow(type_mode mode);

/* instance for GtkWindow */

NspGtkWindow *new_gtkwindow();

/*
* Object methods redefined for gtkwindow 
*/

#define NULLGTKWINDOW (NspGtkWindow*) 0

NspGtkWindow *gtkwindow_create(char *name,NspTypeBase *type);

/* from GtkWindowObj.c */

extern NspGtkWindow *gtkwindow_object (NspObject *O); 
extern int IsGtkWindowObj (Stack stack, int i); 
extern int IsGtkWindow(NspObject *O);
extern NspGtkWindow *GetGtkWindowCopy (Stack stack, int i); 
extern NspGtkWindow *GetGtkWindow (Stack stack, int i); 

#endif 

#ifdef GtkWindow_Private 
static int init_gtkwindow(NspGtkWindow *o,NspTypeGtkWindow *type);
static char *gtkwindow_type_as_string(void);
static char *gtkwindow_type_short_string(NspObject *v);
static AttrTab gtkwindow_attrs[];
/* static int int_gtkwindow_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkwindow_get_methods(void); 
#endif /* GtkWindow_Private */
