/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkMessageDialog
#define NSP_INC_NspGtkMessageDialog

/*
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* NspGtkMessageDialog */

#include <nsp/gtk/gtkdialog.h>

/*
 * NspGtkMessageDialog inherits from GtkDialog
 * just change some type attributes 
 */

typedef NspGtkDialog NspGtkMessageDialog ;
typedef NspTypeGtkDialog NspTypeGtkMessageDialog ;

extern int nsp_type_gtkmessagedialog_id;
extern NspTypeGtkMessageDialog *nsp_type_gtkmessagedialog;

/* type instances for gtkdialog */

NspTypeGtkMessageDialog *new_type_gtkmessagedialog(type_mode mode);

/* instance for NspGtkMessageDialog */

NspGtkMessageDialog *new_gtkmessagedialog();

/*
 * Object methods redefined for gtkmessagedialog 
 */

#define NULLGTKMESSAGEDIALOG (NspGtkMessageDialog*) 0


/* from NspGtkMessageDialogObj.c */

extern NspGtkMessageDialog *nsp_gtkmessagedialog_object (NspObject *O);
extern int IsGtkMessageDialogObj (Stack stack, int i);
extern int IsGtkMessageDialog(NspObject *O);
extern NspGtkMessageDialog *GetGtkMessageDialogCopy (Stack stack, int i);
extern NspGtkMessageDialog *GetGtkMessageDialog (Stack stack, int i);

#endif /* NSP_INC_NspGtkMessageDialog */ 

#ifdef NspGtkMessageDialog_Private 
static int init_gtkmessagedialog(NspGtkMessageDialog *o,NspTypeGtkMessageDialog *type);
static char *nsp_gtkmessagedialog_type_as_string(void);
static char *nsp_gtkmessagedialog_type_short_string(NspObject *v);
static AttrTab gtkmessagedialog_attrs[];
static NspMethods *gtkmessagedialog_get_methods(void);
/* static int int_gtkmessagedialog_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkMessageDialog_Private */
