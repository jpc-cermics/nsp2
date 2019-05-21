/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkColorSelectionDialog
#define NSP_INC_NspGtkColorSelectionDialog

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

/* NspGtkColorSelectionDialog */

#include <nsp/gtk/gtkdialog.h>

/*
 * NspGtkColorSelectionDialog inherits from GtkDialog
 * just change some type attributes 
 */

typedef NspGtkDialog NspGtkColorSelectionDialog ;
typedef NspTypeGtkDialog NspTypeGtkColorSelectionDialog ;

extern int nsp_type_gtkcolorselectiondialog_id;
extern NspTypeGtkColorSelectionDialog *nsp_type_gtkcolorselectiondialog;

/* type instances for gtkdialog */

NspTypeGtkColorSelectionDialog *new_type_gtkcolorselectiondialog(type_mode mode);

/* instance for NspGtkColorSelectionDialog */

NspGtkColorSelectionDialog *new_gtkcolorselectiondialog();

/*
 * Object methods redefined for gtkcolorselectiondialog 
 */

#define NULLGTKCOLORSELECTIONDIALOG (NspGtkColorSelectionDialog*) 0


/* from NspGtkColorSelectionDialogObj.c */

extern NspGtkColorSelectionDialog *nsp_gtkcolorselectiondialog_object (NspObject *O);
extern int IsGtkColorSelectionDialogObj (Stack stack, int i);
extern int IsGtkColorSelectionDialog(NspObject *O);
extern NspGtkColorSelectionDialog *GetGtkColorSelectionDialogCopy (Stack stack, int i);
extern NspGtkColorSelectionDialog *GetGtkColorSelectionDialog (Stack stack, int i);

#endif /* NSP_INC_NspGtkColorSelectionDialog */ 

#ifdef NspGtkColorSelectionDialog_Private 
static int init_gtkcolorselectiondialog(NspGtkColorSelectionDialog *o,NspTypeGtkColorSelectionDialog *type);
static char *nsp_gtkcolorselectiondialog_type_as_string(void);
static char *nsp_gtkcolorselectiondialog_type_short_string(NspObject *v);
static AttrTab gtkcolorselectiondialog_attrs[];
static NspMethods *gtkcolorselectiondialog_get_methods(void);
/* static int int_gtkcolorselectiondialog_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkColorSelectionDialog_Private */
