/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFileChooserWidget
#define INC_NSP_GtkFileChooserWidget

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkvbox.h"

/*
* NspGtkFileChooserWidget inherits from NspGtkVBox
* just change some type attributes 
*/

typedef NspGtkVBox NspGtkFileChooserWidget ;
typedef NspTypeGtkVBox NspTypeGtkFileChooserWidget ;

extern int nsp_type_gtkfilechooserwidget_id;
extern NspTypeGtkFileChooserWidget *nsp_type_gtkfilechooserwidget;

/* type instances for gtkvbox */

NspTypeGtkFileChooserWidget *new_type_gtkfilechooserwidget(type_mode mode);

/* instance for GtkFileChooserWidget */

NspGtkFileChooserWidget *new_gtkfilechooserwidget();

/*
* Object methods redefined for gtkfilechooserwidget 
*/

#define NULLGTKFILECHOOSERWIDGET (NspGtkFileChooserWidget*) 0

NspGtkFileChooserWidget *gtkfilechooserwidget_create(char *name,NspTypeBase *type);

/* from GtkFileChooserWidgetObj.c */

extern NspGtkFileChooserWidget *gtkfilechooserwidget_object (NspObject *O); 
extern int IsGtkFileChooserWidgetObj (Stack stack, int i); 
extern int IsGtkFileChooserWidget(NspObject *O);
extern NspGtkFileChooserWidget *GetGtkFileChooserWidgetCopy (Stack stack, int i); 
extern NspGtkFileChooserWidget *GetGtkFileChooserWidget (Stack stack, int i); 

#endif 

#ifdef GtkFileChooserWidget_Private 
static int init_gtkfilechooserwidget(NspGtkFileChooserWidget *o,NspTypeGtkFileChooserWidget *type);
static char *gtkfilechooserwidget_type_as_string(void);
static char *gtkfilechooserwidget_type_short_string(NspObject *v);
static AttrTab gtkfilechooserwidget_attrs[];
/* static int int_gtkfilechooserwidget_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkfilechooserwidget_get_methods(void); 
#endif /* GtkFileChooserWidget_Private */
