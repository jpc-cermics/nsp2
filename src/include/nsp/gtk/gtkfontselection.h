/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFontSelection
#define INC_NSP_GtkFontSelection

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkvbox.h"

/*
* NspGtkFontSelection inherits from NspGtkVBox
* just change some type attributes 
*/

typedef NspGtkVBox NspGtkFontSelection ;
typedef NspTypeGtkVBox NspTypeGtkFontSelection ;

extern int nsp_type_gtkfontselection_id;
extern NspTypeGtkFontSelection *nsp_type_gtkfontselection;

/* type instances for gtkvbox */

NspTypeGtkFontSelection *new_type_gtkfontselection(type_mode mode);

/* instance for GtkFontSelection */

NspGtkFontSelection *new_gtkfontselection();

/*
* Object methods redefined for gtkfontselection 
*/

#ifdef GtkFontSelection_Private 
static int init_gtkfontselection(NspGtkFontSelection *o,NspTypeGtkFontSelection *type);
static char *gtkfontselection_type_as_string(void);
static char *gtkfontselection_type_short_string(void);
static AttrTab gtkfontselection_attrs[];
/* static int int_gtkfontselection_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkfontselection_get_methods(void); 
#endif /* GtkFontSelection_Private */

#define NULLGTKFONTSELECTION (NspGtkFontSelection*) 0

NspGtkFontSelection *gtkfontselection_create(char *name,NspTypeBase *type);

/* from GtkFontSelectionObj.c */

extern NspGtkFontSelection *gtkfontselection_object (NspObject *O); 
extern int IsGtkFontSelectionObj (Stack stack, int i); 
extern int IsGtkFontSelection(NspObject *O);
extern NspGtkFontSelection *GetGtkFontSelectionCopy (Stack stack, int i); 
extern NspGtkFontSelection *GetGtkFontSelection (Stack stack, int i); 

#endif 
