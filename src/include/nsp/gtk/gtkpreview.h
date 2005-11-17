/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkPreview
#define INC_NSP_GtkPreview

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkPreview inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkPreview ;
typedef NspTypeGtkWidget NspTypeGtkPreview ;

extern int nsp_type_gtkpreview_id;
extern NspTypeGtkPreview *nsp_type_gtkpreview;

/* type instances for gtkwidget */

NspTypeGtkPreview *new_type_gtkpreview(type_mode mode);

/* instance for GtkPreview */

NspGtkPreview *new_gtkpreview();

/*
* Object methods redefined for gtkpreview 
*/

#define NULLGTKPREVIEW (NspGtkPreview*) 0

NspGtkPreview *gtkpreview_create(char *name,NspTypeBase *type);

/* from GtkPreviewObj.c */

extern NspGtkPreview *gtkpreview_object (NspObject *O); 
extern int IsGtkPreviewObj (Stack stack, int i); 
extern int IsGtkPreview(NspObject *O);
extern NspGtkPreview *GetGtkPreviewCopy (Stack stack, int i); 
extern NspGtkPreview *GetGtkPreview (Stack stack, int i); 

#endif 

#ifdef GtkPreview_Private 
static int init_gtkpreview(NspGtkPreview *o,NspTypeGtkPreview *type);
static char *gtkpreview_type_as_string(void);
static char *gtkpreview_type_short_string(void);
static AttrTab gtkpreview_attrs[];
/* static int int_gtkpreview_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkpreview_get_methods(void); 
#endif /* GtkPreview_Private */
