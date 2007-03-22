/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkGLArea
#define INC_NSP_GtkGLArea

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkdrawingarea.h"

/*
* NspGtkGLArea inherits from NspGtkDrawingArea
* just change some type attributes 
*/

typedef NspGtkDrawingArea NspGtkGLArea ;
typedef NspTypeGtkDrawingArea NspTypeGtkGLArea ;

extern int nsp_type_gtkglarea_id;
extern NspTypeGtkGLArea *nsp_type_gtkglarea;

/* type instances for gtkdrawingarea */

NspTypeGtkGLArea *new_type_gtkglarea(type_mode mode);

/* instance for GtkGLArea */

NspGtkGLArea *new_gtkglarea();

/*
* Object methods redefined for gtkglarea 
*/

#define NULLGTKGLAREA (NspGtkGLArea*) 0

NspGtkGLArea *gtkglarea_create(char *name,NspTypeBase *type);

/* from GtkGLAreaObj.c */

extern NspGtkGLArea *gtkglarea_object (NspObject *O); 
extern int IsGtkGLAreaObj (Stack stack, int i); 
extern int IsGtkGLArea(NspObject *O);
extern NspGtkGLArea *GetGtkGLAreaCopy (Stack stack, int i); 
extern NspGtkGLArea *GetGtkGLArea (Stack stack, int i); 

#endif 

#ifdef GtkGLArea_Private 
static int init_gtkglarea(NspGtkGLArea *o,NspTypeGtkGLArea *type);
static char *gtkglarea_type_as_string(void);
static char *gtkglarea_type_short_string(NspObject *v);
static AttrTab gtkglarea_attrs[];
/* static int int_gtkglarea_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkglarea_get_methods(void); 
#endif /* GtkGLArea_Private */
