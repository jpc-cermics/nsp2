/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkDialog
#define INC_NSP_GtkDialog

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkwindow.h"

/*
* NspGtkDialog inherits from NspGtkWindow
* just change some type attributes 
*/

typedef NspGtkWindow NspGtkDialog ;
typedef NspTypeGtkWindow NspTypeGtkDialog ;

extern int nsp_type_gtkdialog_id;
extern NspTypeGtkDialog *nsp_type_gtkdialog;

/* type instances for gtkwindow */

NspTypeGtkDialog *new_type_gtkdialog(type_mode mode);

/* instance for GtkDialog */

NspGtkDialog *new_gtkdialog();

/*
* Object methods redefined for gtkdialog 
*/

#define NULLGTKDIALOG (NspGtkDialog*) 0

NspGtkDialog *gtkdialog_create(char *name,NspTypeBase *type);

/* from GtkDialogObj.c */

extern NspGtkDialog *gtkdialog_object (NspObject *O); 
extern int IsGtkDialogObj (Stack stack, int i); 
extern int IsGtkDialog(NspObject *O);
extern NspGtkDialog *GetGtkDialogCopy (Stack stack, int i); 
extern NspGtkDialog *GetGtkDialog (Stack stack, int i); 

#endif 

#ifdef GtkDialog_Private 
static int init_gtkdialog(NspGtkDialog *o,NspTypeGtkDialog *type);
static char *gtkdialog_type_as_string(void);
static char *gtkdialog_type_short_string(NspObject *v);
static AttrTab gtkdialog_attrs[];
/* static int int_gtkdialog_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkdialog_get_methods(void); 
#endif /* GtkDialog_Private */
