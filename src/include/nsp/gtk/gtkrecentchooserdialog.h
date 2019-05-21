/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkRecentChooserDialog
#define NSP_INC_NspGtkRecentChooserDialog

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

/* NspGtkRecentChooserDialog */

#include <nsp/gtk/gtkdialog.h>

/*
 * NspGtkRecentChooserDialog inherits from GtkDialog
 * just change some type attributes 
 */

typedef NspGtkDialog NspGtkRecentChooserDialog ;
typedef NspTypeGtkDialog NspTypeGtkRecentChooserDialog ;

extern int nsp_type_gtkrecentchooserdialog_id;
extern NspTypeGtkRecentChooserDialog *nsp_type_gtkrecentchooserdialog;

/* type instances for gtkdialog */

NspTypeGtkRecentChooserDialog *new_type_gtkrecentchooserdialog(type_mode mode);

/* instance for NspGtkRecentChooserDialog */

NspGtkRecentChooserDialog *new_gtkrecentchooserdialog();

/*
 * Object methods redefined for gtkrecentchooserdialog 
 */

#define NULLGTKRECENTCHOOSERDIALOG (NspGtkRecentChooserDialog*) 0


/* from NspGtkRecentChooserDialogObj.c */

extern NspGtkRecentChooserDialog *nsp_gtkrecentchooserdialog_object (NspObject *O);
extern int IsGtkRecentChooserDialogObj (Stack stack, int i);
extern int IsGtkRecentChooserDialog(NspObject *O);
extern NspGtkRecentChooserDialog *GetGtkRecentChooserDialogCopy (Stack stack, int i);
extern NspGtkRecentChooserDialog *GetGtkRecentChooserDialog (Stack stack, int i);

#endif /* NSP_INC_NspGtkRecentChooserDialog */ 

#ifdef NspGtkRecentChooserDialog_Private 
static int init_gtkrecentchooserdialog(NspGtkRecentChooserDialog *o,NspTypeGtkRecentChooserDialog *type);
static char *nsp_gtkrecentchooserdialog_type_as_string(void);
static char *nsp_gtkrecentchooserdialog_type_short_string(NspObject *v);
static AttrTab gtkrecentchooserdialog_attrs[];
static NspMethods *gtkrecentchooserdialog_get_methods(void);
/* static int int_gtkrecentchooserdialog_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkRecentChooserDialog_Private */
