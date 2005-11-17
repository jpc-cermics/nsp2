/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkScrolledWindow
#define INC_NSP_GtkScrolledWindow

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkScrolledWindow inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkScrolledWindow ;
typedef NspTypeGtkBin NspTypeGtkScrolledWindow ;

extern int nsp_type_gtkscrolledwindow_id;
extern NspTypeGtkScrolledWindow *nsp_type_gtkscrolledwindow;

/* type instances for gtkbin */

NspTypeGtkScrolledWindow *new_type_gtkscrolledwindow(type_mode mode);

/* instance for GtkScrolledWindow */

NspGtkScrolledWindow *new_gtkscrolledwindow();

/*
* Object methods redefined for gtkscrolledwindow 
*/

#define NULLGTKSCROLLEDWINDOW (NspGtkScrolledWindow*) 0

NspGtkScrolledWindow *gtkscrolledwindow_create(char *name,NspTypeBase *type);

/* from GtkScrolledWindowObj.c */

extern NspGtkScrolledWindow *gtkscrolledwindow_object (NspObject *O); 
extern int IsGtkScrolledWindowObj (Stack stack, int i); 
extern int IsGtkScrolledWindow(NspObject *O);
extern NspGtkScrolledWindow *GetGtkScrolledWindowCopy (Stack stack, int i); 
extern NspGtkScrolledWindow *GetGtkScrolledWindow (Stack stack, int i); 

#endif 

#ifdef GtkScrolledWindow_Private 
static int init_gtkscrolledwindow(NspGtkScrolledWindow *o,NspTypeGtkScrolledWindow *type);
static char *gtkscrolledwindow_type_as_string(void);
static char *gtkscrolledwindow_type_short_string(void);
static AttrTab gtkscrolledwindow_attrs[];
/* static int int_gtkscrolledwindow_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkscrolledwindow_get_methods(void); 
#endif /* GtkScrolledWindow_Private */
