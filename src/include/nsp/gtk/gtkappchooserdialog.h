/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAppChooserDialog
#define NSP_INC_NspGtkAppChooserDialog

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

/* NspGtkAppChooserDialog */

#include <nsp/gtk/gtkdialog.h>

/*
 * NspGtkAppChooserDialog inherits from GtkDialog
 * just change some type attributes 
 */

typedef NspGtkDialog NspGtkAppChooserDialog ;
typedef NspTypeGtkDialog NspTypeGtkAppChooserDialog ;

extern int nsp_type_gtkappchooserdialog_id;
extern NspTypeGtkAppChooserDialog *nsp_type_gtkappchooserdialog;

/* type instances for gtkdialog */

NspTypeGtkAppChooserDialog *new_type_gtkappchooserdialog(type_mode mode);

/* instance for NspGtkAppChooserDialog */

NspGtkAppChooserDialog *new_gtkappchooserdialog();

/*
 * Object methods redefined for gtkappchooserdialog 
 */

#define NULLGTKAPPCHOOSERDIALOG (NspGtkAppChooserDialog*) 0


/* from NspGtkAppChooserDialogObj.c */

extern NspGtkAppChooserDialog *nsp_gtkappchooserdialog_object (NspObject *O);
extern int IsGtkAppChooserDialogObj (Stack stack, int i);
extern int IsGtkAppChooserDialog(NspObject *O);
extern NspGtkAppChooserDialog *GetGtkAppChooserDialogCopy (Stack stack, int i);
extern NspGtkAppChooserDialog *GetGtkAppChooserDialog (Stack stack, int i);

#endif /* NSP_INC_NspGtkAppChooserDialog */ 

#ifdef NspGtkAppChooserDialog_Private 
static int init_gtkappchooserdialog(NspGtkAppChooserDialog *o,NspTypeGtkAppChooserDialog *type);
static char *nsp_gtkappchooserdialog_type_as_string(void);
static char *nsp_gtkappchooserdialog_type_short_string(NspObject *v);
static AttrTab gtkappchooserdialog_attrs[];
static NspMethods *gtkappchooserdialog_get_methods(void);
/* static int int_gtkappchooserdialog_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAppChooserDialog_Private */
