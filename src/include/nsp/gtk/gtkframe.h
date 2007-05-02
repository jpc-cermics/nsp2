/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFrame
#define INC_NSP_GtkFrame

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkbin.h"

/*
* NspGtkFrame inherits from NspGtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkFrame ;
typedef NspTypeGtkBin NspTypeGtkFrame ;

extern int nsp_type_gtkframe_id;
extern NspTypeGtkFrame *nsp_type_gtkframe;

/* type instances for gtkbin */

NspTypeGtkFrame *new_type_gtkframe(type_mode mode);

/* instance for GtkFrame */

NspGtkFrame *new_gtkframe();

/*
* Object methods redefined for gtkframe 
*/

#define NULLGTKFRAME (NspGtkFrame*) 0

NspGtkFrame *gtkframe_create(char *name,NspTypeBase *type);

/* from GtkFrameObj.c */

extern NspGtkFrame *gtkframe_object (NspObject *O); 
extern int IsGtkFrameObj (Stack stack, int i); 
extern int IsGtkFrame(NspObject *O);
extern NspGtkFrame *GetGtkFrameCopy (Stack stack, int i); 
extern NspGtkFrame *GetGtkFrame (Stack stack, int i); 

#endif 

#ifdef GtkFrame_Private 
static int init_gtkframe(NspGtkFrame *o,NspTypeGtkFrame *type);
static char *gtkframe_type_as_string(void);
static char *gtkframe_type_short_string(NspObject *v);
static AttrTab gtkframe_attrs[];
/* static int int_gtkframe_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkframe_get_methods(void); 
#endif /* GtkFrame_Private */
