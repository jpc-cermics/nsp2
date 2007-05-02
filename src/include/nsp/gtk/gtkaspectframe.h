/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkAspectFrame
#define INC_NSP_GtkAspectFrame

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkframe.h"

/*
* NspGtkAspectFrame inherits from NspGtkFrame
* just change some type attributes 
*/

typedef NspGtkFrame NspGtkAspectFrame ;
typedef NspTypeGtkFrame NspTypeGtkAspectFrame ;

extern int nsp_type_gtkaspectframe_id;
extern NspTypeGtkAspectFrame *nsp_type_gtkaspectframe;

/* type instances for gtkframe */

NspTypeGtkAspectFrame *new_type_gtkaspectframe(type_mode mode);

/* instance for GtkAspectFrame */

NspGtkAspectFrame *new_gtkaspectframe();

/*
* Object methods redefined for gtkaspectframe 
*/

#define NULLGTKASPECTFRAME (NspGtkAspectFrame*) 0

NspGtkAspectFrame *gtkaspectframe_create(char *name,NspTypeBase *type);

/* from GtkAspectFrameObj.c */

extern NspGtkAspectFrame *gtkaspectframe_object (NspObject *O); 
extern int IsGtkAspectFrameObj (Stack stack, int i); 
extern int IsGtkAspectFrame(NspObject *O);
extern NspGtkAspectFrame *GetGtkAspectFrameCopy (Stack stack, int i); 
extern NspGtkAspectFrame *GetGtkAspectFrame (Stack stack, int i); 

#endif 

#ifdef GtkAspectFrame_Private 
static int init_gtkaspectframe(NspGtkAspectFrame *o,NspTypeGtkAspectFrame *type);
static char *gtkaspectframe_type_as_string(void);
static char *gtkaspectframe_type_short_string(NspObject *v);
static AttrTab gtkaspectframe_attrs[];
/* static int int_gtkaspectframe_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkaspectframe_get_methods(void); 
#endif /* GtkAspectFrame_Private */
