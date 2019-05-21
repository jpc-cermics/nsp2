/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFontChooserDialog
#define NSP_INC_NspGtkFontChooserDialog

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

/* NspGtkFontChooserDialog */

#include <nsp/gtk/gtkdialog.h>

/*
 * NspGtkFontChooserDialog inherits from GtkDialog
 * just change some type attributes 
 */

typedef NspGtkDialog NspGtkFontChooserDialog ;
typedef NspTypeGtkDialog NspTypeGtkFontChooserDialog ;

extern int nsp_type_gtkfontchooserdialog_id;
extern NspTypeGtkFontChooserDialog *nsp_type_gtkfontchooserdialog;

/* type instances for gtkdialog */

NspTypeGtkFontChooserDialog *new_type_gtkfontchooserdialog(type_mode mode);

/* instance for NspGtkFontChooserDialog */

NspGtkFontChooserDialog *new_gtkfontchooserdialog();

/*
 * Object methods redefined for gtkfontchooserdialog 
 */

#define NULLGTKFONTCHOOSERDIALOG (NspGtkFontChooserDialog*) 0


/* from NspGtkFontChooserDialogObj.c */

extern NspGtkFontChooserDialog *nsp_gtkfontchooserdialog_object (NspObject *O);
extern int IsGtkFontChooserDialogObj (Stack stack, int i);
extern int IsGtkFontChooserDialog(NspObject *O);
extern NspGtkFontChooserDialog *GetGtkFontChooserDialogCopy (Stack stack, int i);
extern NspGtkFontChooserDialog *GetGtkFontChooserDialog (Stack stack, int i);

#endif /* NSP_INC_NspGtkFontChooserDialog */ 

#ifdef NspGtkFontChooserDialog_Private 
static int init_gtkfontchooserdialog(NspGtkFontChooserDialog *o,NspTypeGtkFontChooserDialog *type);
static char *nsp_gtkfontchooserdialog_type_as_string(void);
static char *nsp_gtkfontchooserdialog_type_short_string(NspObject *v);
static AttrTab gtkfontchooserdialog_attrs[];
static NspMethods *gtkfontchooserdialog_get_methods(void);
/* static int int_gtkfontchooserdialog_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFontChooserDialog_Private */
