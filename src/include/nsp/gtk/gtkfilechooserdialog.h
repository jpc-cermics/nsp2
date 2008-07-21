/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkFileChooserDialog
#define INC_NSP_GtkFileChooserDialog

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkdialog.h"

/*
* NspGtkFileChooserDialog inherits from NspGtkDialog
* just change some type attributes 
*/

typedef NspGtkDialog NspGtkFileChooserDialog ;
typedef NspTypeGtkDialog NspTypeGtkFileChooserDialog ;

extern int nsp_type_gtkfilechooserdialog_id;
extern NspTypeGtkFileChooserDialog *nsp_type_gtkfilechooserdialog;

/* type instances for gtkdialog */

NspTypeGtkFileChooserDialog *new_type_gtkfilechooserdialog(type_mode mode);

/* instance for GtkFileChooserDialog */

NspGtkFileChooserDialog *new_gtkfilechooserdialog();

/*
* Object methods redefined for gtkfilechooserdialog 
*/

#define NULLGTKFILECHOOSERDIALOG (NspGtkFileChooserDialog*) 0

NspGtkFileChooserDialog *gtkfilechooserdialog_create(char *name,NspTypeBase *type);

/* from GtkFileChooserDialogObj.c */

extern NspGtkFileChooserDialog *gtkfilechooserdialog_object (NspObject *O); 
extern int IsGtkFileChooserDialogObj (Stack stack, int i); 
extern int IsGtkFileChooserDialog(NspObject *O);
extern NspGtkFileChooserDialog *GetGtkFileChooserDialogCopy (Stack stack, int i); 
extern NspGtkFileChooserDialog *GetGtkFileChooserDialog (Stack stack, int i); 

#endif 

#ifdef GtkFileChooserDialog_Private 
static int init_gtkfilechooserdialog(NspGtkFileChooserDialog *o,NspTypeGtkFileChooserDialog *type);
static char *gtkfilechooserdialog_type_as_string(void);
static char *gtkfilechooserdialog_type_short_string(NspObject *v);
static AttrTab gtkfilechooserdialog_attrs[];
/* static int int_gtkfilechooserdialog_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkfilechooserdialog_get_methods(void); 
#endif /* GtkFileChooserDialog_Private */
