/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkAboutDialog
#define INC_NSP_GtkAboutDialog

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkdialog.h"

/*
* NspGtkAboutDialog inherits from NspGtkDialog
* just change some type attributes 
*/

typedef NspGtkDialog NspGtkAboutDialog ;
typedef NspTypeGtkDialog NspTypeGtkAboutDialog ;

extern int nsp_type_gtkaboutdialog_id;
extern NspTypeGtkAboutDialog *nsp_type_gtkaboutdialog;

/* type instances for gtkdialog */

NspTypeGtkAboutDialog *new_type_gtkaboutdialog(type_mode mode);

/* instance for GtkAboutDialog */

NspGtkAboutDialog *new_gtkaboutdialog();

/*
* Object methods redefined for gtkaboutdialog 
*/

#define NULLGTKABOUTDIALOG (NspGtkAboutDialog*) 0

NspGtkAboutDialog *gtkaboutdialog_create(char *name,NspTypeBase *type);

/* from GtkAboutDialogObj.c */

extern NspGtkAboutDialog *gtkaboutdialog_object (NspObject *O); 
extern int IsGtkAboutDialogObj (Stack stack, int i); 
extern int IsGtkAboutDialog(NspObject *O);
extern NspGtkAboutDialog *GetGtkAboutDialogCopy (Stack stack, int i); 
extern NspGtkAboutDialog *GetGtkAboutDialog (Stack stack, int i); 

#endif 

#ifdef GtkAboutDialog_Private 
static int init_gtkaboutdialog(NspGtkAboutDialog *o,NspTypeGtkAboutDialog *type);
static char *gtkaboutdialog_type_as_string(void);
static char *gtkaboutdialog_type_short_string(NspObject *v);
static AttrTab gtkaboutdialog_attrs[];
/* static int int_gtkaboutdialog_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkaboutdialog_get_methods(void); 
#endif /* GtkAboutDialog_Private */
