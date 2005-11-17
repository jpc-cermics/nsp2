/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFontSelectionDialog
#define INC_NSP_GtkFontSelectionDialog

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkdialog.h"

/*
* NspGtkFontSelectionDialog inherits from NspGtkDialog
* just change some type attributes 
*/

typedef NspGtkDialog NspGtkFontSelectionDialog ;
typedef NspTypeGtkDialog NspTypeGtkFontSelectionDialog ;

extern int nsp_type_gtkfontselectiondialog_id;
extern NspTypeGtkFontSelectionDialog *nsp_type_gtkfontselectiondialog;

/* type instances for gtkdialog */

NspTypeGtkFontSelectionDialog *new_type_gtkfontselectiondialog(type_mode mode);

/* instance for GtkFontSelectionDialog */

NspGtkFontSelectionDialog *new_gtkfontselectiondialog();

/*
* Object methods redefined for gtkfontselectiondialog 
*/

#define NULLGTKFONTSELECTIONDIALOG (NspGtkFontSelectionDialog*) 0

NspGtkFontSelectionDialog *gtkfontselectiondialog_create(char *name,NspTypeBase *type);

/* from GtkFontSelectionDialogObj.c */

extern NspGtkFontSelectionDialog *gtkfontselectiondialog_object (NspObject *O); 
extern int IsGtkFontSelectionDialogObj (Stack stack, int i); 
extern int IsGtkFontSelectionDialog(NspObject *O);
extern NspGtkFontSelectionDialog *GetGtkFontSelectionDialogCopy (Stack stack, int i); 
extern NspGtkFontSelectionDialog *GetGtkFontSelectionDialog (Stack stack, int i); 

#endif 

#ifdef GtkFontSelectionDialog_Private 
static int init_gtkfontselectiondialog(NspGtkFontSelectionDialog *o,NspTypeGtkFontSelectionDialog *type);
static char *gtkfontselectiondialog_type_as_string(void);
static char *gtkfontselectiondialog_type_short_string(void);
static AttrTab gtkfontselectiondialog_attrs[];
/* static int int_gtkfontselectiondialog_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkfontselectiondialog_get_methods(void); 
#endif /* GtkFontSelectionDialog_Private */
