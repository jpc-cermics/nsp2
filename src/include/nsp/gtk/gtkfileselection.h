/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFileSelection
#define INC_NSP_GtkFileSelection

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkdialog.h"

/*
* NspGtkFileSelection inherits from NspGtkDialog
* just change some type attributes 
*/

typedef NspGtkDialog NspGtkFileSelection ;
typedef NspTypeGtkDialog NspTypeGtkFileSelection ;

extern int nsp_type_gtkfileselection_id;
extern NspTypeGtkFileSelection *nsp_type_gtkfileselection;

/* type instances for gtkdialog */

NspTypeGtkFileSelection *new_type_gtkfileselection(type_mode mode);

/* instance for GtkFileSelection */

NspGtkFileSelection *new_gtkfileselection();

/*
* Object methods redefined for gtkfileselection 
*/

#ifdef GtkFileSelection_Private 
static int init_gtkfileselection(NspGtkFileSelection *o,NspTypeGtkFileSelection *type);
static char *gtkfileselection_type_as_string(void);
static char *gtkfileselection_type_short_string(void);
static AttrTab gtkfileselection_attrs[];
/* static int int_gtkfileselection_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkfileselection_get_methods(void); 
#endif /* GtkFileSelection_Private */

#define NULLGTKFILESELECTION (NspGtkFileSelection*) 0

NspGtkFileSelection *gtkfileselection_create(char *name,NspTypeBase *type);

/* from GtkFileSelectionObj.c */

extern NspGtkFileSelection *gtkfileselection_object (NspObject *O); 
extern int IsGtkFileSelectionObj (Stack stack, int i); 
extern int IsGtkFileSelection(NspObject *O);
extern NspGtkFileSelection *GetGtkFileSelectionCopy (Stack stack, int i); 
extern NspGtkFileSelection *GetGtkFileSelection (Stack stack, int i); 

#endif 
