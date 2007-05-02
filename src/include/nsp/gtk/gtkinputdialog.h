/* -*- Mode: C -*- */
#ifndef INC_NSP_GtkInputDialog
#define INC_NSP_GtkInputDialog

/*-----------------------------------------------------------------
* This Software is ( Copyright ENPC 1998-2007 )
* Jean-Philippe Chancelier Enpc/Cermics
*-----------------------------------------------------------------*/

#include "nsp/gtk/gtkdialog.h"

/*
* NspGtkInputDialog inherits from NspGtkDialog
* just change some type attributes 
*/

typedef NspGtkDialog NspGtkInputDialog ;
typedef NspTypeGtkDialog NspTypeGtkInputDialog ;

extern int nsp_type_gtkinputdialog_id;
extern NspTypeGtkInputDialog *nsp_type_gtkinputdialog;

/* type instances for gtkdialog */

NspTypeGtkInputDialog *new_type_gtkinputdialog(type_mode mode);

/* instance for GtkInputDialog */

NspGtkInputDialog *new_gtkinputdialog();

/*
* Object methods redefined for gtkinputdialog 
*/

#define NULLGTKINPUTDIALOG (NspGtkInputDialog*) 0

NspGtkInputDialog *gtkinputdialog_create(char *name,NspTypeBase *type);

/* from GtkInputDialogObj.c */

extern NspGtkInputDialog *gtkinputdialog_object (NspObject *O); 
extern int IsGtkInputDialogObj (Stack stack, int i); 
extern int IsGtkInputDialog(NspObject *O);
extern NspGtkInputDialog *GetGtkInputDialogCopy (Stack stack, int i); 
extern NspGtkInputDialog *GetGtkInputDialog (Stack stack, int i); 

#endif 

#ifdef GtkInputDialog_Private 
static int init_gtkinputdialog(NspGtkInputDialog *o,NspTypeGtkInputDialog *type);
static char *gtkinputdialog_type_as_string(void);
static char *gtkinputdialog_type_short_string(NspObject *v);
static AttrTab gtkinputdialog_attrs[];
/* static int int_gtkinputdialog_create(Stack stack, int rhs, int opt, int lhs);*/
static NspMethods *gtkinputdialog_get_methods(void); 
#endif /* GtkInputDialog_Private */
