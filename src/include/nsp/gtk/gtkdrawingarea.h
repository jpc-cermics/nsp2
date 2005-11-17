/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkDrawingArea
#define INC_NSP_GtkDrawingArea

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwidget.h"

/*
* NspGtkDrawingArea inherits from NspGtkWidget
* just change some type attributes 
*/

typedef NspGtkWidget NspGtkDrawingArea ;
typedef NspTypeGtkWidget NspTypeGtkDrawingArea ;

extern int nsp_type_gtkdrawingarea_id;
extern NspTypeGtkDrawingArea *nsp_type_gtkdrawingarea;

/* type instances for gtkwidget */

NspTypeGtkDrawingArea *new_type_gtkdrawingarea(type_mode mode);

/* instance for GtkDrawingArea */

NspGtkDrawingArea *new_gtkdrawingarea();

/*
* Object methods redefined for gtkdrawingarea 
*/

#define NULLGTKDRAWINGAREA (NspGtkDrawingArea*) 0

NspGtkDrawingArea *gtkdrawingarea_create(char *name,NspTypeBase *type);

/* from GtkDrawingAreaObj.c */

extern NspGtkDrawingArea *gtkdrawingarea_object (NspObject *O); 
extern int IsGtkDrawingAreaObj (Stack stack, int i); 
extern int IsGtkDrawingArea(NspObject *O);
extern NspGtkDrawingArea *GetGtkDrawingAreaCopy (Stack stack, int i); 
extern NspGtkDrawingArea *GetGtkDrawingArea (Stack stack, int i); 

#endif 

#ifdef GtkDrawingArea_Private 
static int init_gtkdrawingarea(NspGtkDrawingArea *o,NspTypeGtkDrawingArea *type);
static char *gtkdrawingarea_type_as_string(void);
static char *gtkdrawingarea_type_short_string(void);
static AttrTab gtkdrawingarea_attrs[];
/* static int int_gtkdrawingarea_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkdrawingarea_get_methods(void); 
#endif /* GtkDrawingArea_Private */
