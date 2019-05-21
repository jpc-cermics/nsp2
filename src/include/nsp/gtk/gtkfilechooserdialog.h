/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFileChooserDialog
#define NSP_INC_NspGtkFileChooserDialog

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

/* NspGtkFileChooserDialog */

#include <nsp/gtk/gtkdialog.h>

/*
 * NspGtkFileChooserDialog inherits from GtkDialog
 * just change some type attributes 
 */

typedef NspGtkDialog NspGtkFileChooserDialog ;
typedef NspTypeGtkDialog NspTypeGtkFileChooserDialog ;

extern int nsp_type_gtkfilechooserdialog_id;
extern NspTypeGtkFileChooserDialog *nsp_type_gtkfilechooserdialog;

/* type instances for gtkdialog */

NspTypeGtkFileChooserDialog *new_type_gtkfilechooserdialog(type_mode mode);

/* instance for NspGtkFileChooserDialog */

NspGtkFileChooserDialog *new_gtkfilechooserdialog();

/*
 * Object methods redefined for gtkfilechooserdialog 
 */

#define NULLGTKFILECHOOSERDIALOG (NspGtkFileChooserDialog*) 0


/* from NspGtkFileChooserDialogObj.c */

extern NspGtkFileChooserDialog *nsp_gtkfilechooserdialog_object (NspObject *O);
extern int IsGtkFileChooserDialogObj (Stack stack, int i);
extern int IsGtkFileChooserDialog(NspObject *O);
extern NspGtkFileChooserDialog *GetGtkFileChooserDialogCopy (Stack stack, int i);
extern NspGtkFileChooserDialog *GetGtkFileChooserDialog (Stack stack, int i);

#endif /* NSP_INC_NspGtkFileChooserDialog */ 

#ifdef NspGtkFileChooserDialog_Private 
static int init_gtkfilechooserdialog(NspGtkFileChooserDialog *o,NspTypeGtkFileChooserDialog *type);
static char *nsp_gtkfilechooserdialog_type_as_string(void);
static char *nsp_gtkfilechooserdialog_type_short_string(NspObject *v);
static AttrTab gtkfilechooserdialog_attrs[];
static NspMethods *gtkfilechooserdialog_get_methods(void);
/* static int int_gtkfilechooserdialog_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFileChooserDialog_Private */
