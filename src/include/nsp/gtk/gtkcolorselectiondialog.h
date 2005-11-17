/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkColorSelectionDialog
#define INC_NSP_GtkColorSelectionDialog

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkdialog.h"

/*
* NspGtkColorSelectionDialog inherits from NspGtkDialog
* just change some type attributes 
*/

typedef NspGtkDialog NspGtkColorSelectionDialog ;
typedef NspTypeGtkDialog NspTypeGtkColorSelectionDialog ;

extern int nsp_type_gtkcolorselectiondialog_id;
extern NspTypeGtkColorSelectionDialog *nsp_type_gtkcolorselectiondialog;

/* type instances for gtkdialog */

NspTypeGtkColorSelectionDialog *new_type_gtkcolorselectiondialog(type_mode mode);

/* instance for GtkColorSelectionDialog */

NspGtkColorSelectionDialog *new_gtkcolorselectiondialog();

/*
* Object methods redefined for gtkcolorselectiondialog 
*/

#define NULLGTKCOLORSELECTIONDIALOG (NspGtkColorSelectionDialog*) 0

NspGtkColorSelectionDialog *gtkcolorselectiondialog_create(char *name,NspTypeBase *type);

/* from GtkColorSelectionDialogObj.c */

extern NspGtkColorSelectionDialog *gtkcolorselectiondialog_object (NspObject *O); 
extern int IsGtkColorSelectionDialogObj (Stack stack, int i); 
extern int IsGtkColorSelectionDialog(NspObject *O);
extern NspGtkColorSelectionDialog *GetGtkColorSelectionDialogCopy (Stack stack, int i); 
extern NspGtkColorSelectionDialog *GetGtkColorSelectionDialog (Stack stack, int i); 

#endif 

#ifdef GtkColorSelectionDialog_Private 
static int init_gtkcolorselectiondialog(NspGtkColorSelectionDialog *o,NspTypeGtkColorSelectionDialog *type);
static char *gtkcolorselectiondialog_type_as_string(void);
static char *gtkcolorselectiondialog_type_short_string(void);
static AttrTab gtkcolorselectiondialog_attrs[];
/* static int int_gtkcolorselectiondialog_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkcolorselectiondialog_get_methods(void); 
#endif /* GtkColorSelectionDialog_Private */
