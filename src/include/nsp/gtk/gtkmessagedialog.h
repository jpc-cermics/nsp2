/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkMessageDialog
#define INC_NSP_GtkMessageDialog

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2003 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkdialog.h"

/*
* NspGtkMessageDialog inherits from NspGtkDialog
* just change some type attributes 
*/

typedef NspGtkDialog NspGtkMessageDialog ;
typedef NspTypeGtkDialog NspTypeGtkMessageDialog ;

extern int nsp_type_gtkmessagedialog_id;
extern NspTypeGtkMessageDialog *nsp_type_gtkmessagedialog;

/* type instances for gtkdialog */

NspTypeGtkMessageDialog *new_type_gtkmessagedialog(type_mode mode);

/* instance for GtkMessageDialog */

NspGtkMessageDialog *new_gtkmessagedialog();

/*
* Object methods redefined for gtkmessagedialog 
*/

#define NULLGTKMESSAGEDIALOG (NspGtkMessageDialog*) 0

NspGtkMessageDialog *gtkmessagedialog_create(char *name,NspTypeBase *type);

/* from GtkMessageDialogObj.c */

extern NspGtkMessageDialog *gtkmessagedialog_object (NspObject *O); 
extern int IsGtkMessageDialogObj (Stack stack, int i); 
extern int IsGtkMessageDialog(NspObject *O);
extern NspGtkMessageDialog *GetGtkMessageDialogCopy (Stack stack, int i); 
extern NspGtkMessageDialog *GetGtkMessageDialog (Stack stack, int i); 

#endif 

#ifdef GtkMessageDialog_Private 
static int init_gtkmessagedialog(NspGtkMessageDialog *o,NspTypeGtkMessageDialog *type);
static char *gtkmessagedialog_type_as_string(void);
static char *gtkmessagedialog_type_short_string(void);
static AttrTab gtkmessagedialog_attrs[];
/* static int int_gtkmessagedialog_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkmessagedialog_get_methods(void); 
#endif /* GtkMessageDialog_Private */
