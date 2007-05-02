/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkColorSelection
#define INC_NSP_GtkColorSelection

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkvbox.h"

/*
* NspGtkColorSelection inherits from NspGtkVBox
* just change some type attributes 
*/

typedef NspGtkVBox NspGtkColorSelection ;
typedef NspTypeGtkVBox NspTypeGtkColorSelection ;

extern int nsp_type_gtkcolorselection_id;
extern NspTypeGtkColorSelection *nsp_type_gtkcolorselection;

/* type instances for gtkvbox */

NspTypeGtkColorSelection *new_type_gtkcolorselection(type_mode mode);

/* instance for GtkColorSelection */

NspGtkColorSelection *new_gtkcolorselection();

/*
* Object methods redefined for gtkcolorselection 
*/

#define NULLGTKCOLORSELECTION (NspGtkColorSelection*) 0

NspGtkColorSelection *gtkcolorselection_create(char *name,NspTypeBase *type);

/* from GtkColorSelectionObj.c */

extern NspGtkColorSelection *gtkcolorselection_object (NspObject *O); 
extern int IsGtkColorSelectionObj (Stack stack, int i); 
extern int IsGtkColorSelection(NspObject *O);
extern NspGtkColorSelection *GetGtkColorSelectionCopy (Stack stack, int i); 
extern NspGtkColorSelection *GetGtkColorSelection (Stack stack, int i); 

#endif 

#ifdef GtkColorSelection_Private 
static int init_gtkcolorselection(NspGtkColorSelection *o,NspTypeGtkColorSelection *type);
static char *gtkcolorselection_type_as_string(void);
static char *gtkcolorselection_type_short_string(NspObject *v);
static AttrTab gtkcolorselection_attrs[];
/* static int int_gtkcolorselection_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcolorselection_get_methods(void); 
#endif /* GtkColorSelection_Private */
