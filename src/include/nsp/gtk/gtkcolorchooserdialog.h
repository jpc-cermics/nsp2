/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkColorChooserDialog
#define NSP_INC_NspGtkColorChooserDialog

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

/* NspGtkColorChooserDialog */

#include <nsp/gtk/gtkdialog.h>

/*
 * NspGtkColorChooserDialog inherits from GtkDialog
 * just change some type attributes 
 */

typedef NspGtkDialog NspGtkColorChooserDialog ;
typedef NspTypeGtkDialog NspTypeGtkColorChooserDialog ;

extern int nsp_type_gtkcolorchooserdialog_id;
extern NspTypeGtkColorChooserDialog *nsp_type_gtkcolorchooserdialog;

/* type instances for gtkdialog */

NspTypeGtkColorChooserDialog *new_type_gtkcolorchooserdialog(type_mode mode);

/* instance for NspGtkColorChooserDialog */

NspGtkColorChooserDialog *new_gtkcolorchooserdialog();

/*
 * Object methods redefined for gtkcolorchooserdialog 
 */

#define NULLGTKCOLORCHOOSERDIALOG (NspGtkColorChooserDialog*) 0


/* from NspGtkColorChooserDialogObj.c */

extern NspGtkColorChooserDialog *nsp_gtkcolorchooserdialog_object (NspObject *O);
extern int IsGtkColorChooserDialogObj (Stack stack, int i);
extern int IsGtkColorChooserDialog(NspObject *O);
extern NspGtkColorChooserDialog *GetGtkColorChooserDialogCopy (Stack stack, int i);
extern NspGtkColorChooserDialog *GetGtkColorChooserDialog (Stack stack, int i);

#endif /* NSP_INC_NspGtkColorChooserDialog */ 

#ifdef NspGtkColorChooserDialog_Private 
static int init_gtkcolorchooserdialog(NspGtkColorChooserDialog *o,NspTypeGtkColorChooserDialog *type);
static char *nsp_gtkcolorchooserdialog_type_as_string(void);
static char *nsp_gtkcolorchooserdialog_type_short_string(NspObject *v);
static AttrTab gtkcolorchooserdialog_attrs[];
static NspMethods *gtkcolorchooserdialog_get_methods(void);
/* static int int_gtkcolorchooserdialog_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkColorChooserDialog_Private */
