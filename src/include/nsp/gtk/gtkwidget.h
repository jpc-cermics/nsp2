/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkWidget
#define INC_NSP_GtkWidget

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkobject.h"

/*
* NspGtkWidget inherits from NspGtkObject
* just change some type attributes 
*/

typedef NspGtkObject NspGtkWidget ;
typedef NspTypeGtkObject NspTypeGtkWidget ;

extern int nsp_type_gtkwidget_id;
extern NspTypeGtkWidget *nsp_type_gtkwidget;

/* type instances for gtkobject */

NspTypeGtkWidget *new_type_gtkwidget(type_mode mode);

/* instance for GtkWidget */

NspGtkWidget *new_gtkwidget();

/*
* Object methods redefined for gtkwidget 
*/

#ifdef GtkWidget_Private 
static int init_gtkwidget(NspGtkWidget *o,NspTypeGtkWidget *type);
static char *gtkwidget_type_as_string(void);
static char *gtkwidget_type_short_string(void);
static AttrTab gtkwidget_attrs[];
/* static int int_gtkwidget_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkwidget_get_methods(void); 
#endif /* GtkWidget_Private */

#define NULLGTKWIDGET (NspGtkWidget*) 0

NspGtkWidget *gtkwidget_create(char *name,NspTypeBase *type);

/* from GtkWidgetObj.c */

extern NspGtkWidget *gtkwidget_object (NspObject *O); 
extern int IsGtkWidgetObj (Stack stack, int i); 
extern int IsGtkWidget(NspObject *O);
extern NspGtkWidget *GetGtkWidgetCopy (Stack stack, int i); 
extern NspGtkWidget *GetGtkWidget (Stack stack, int i); 

#endif 
