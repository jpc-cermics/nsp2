/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFontSelectionDialog
#define NSP_INC_NspGtkFontSelectionDialog

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

/* NspGtkFontSelectionDialog */

#include <nsp/gtk/gtkdialog.h>

/*
 * NspGtkFontSelectionDialog inherits from GtkDialog
 * just change some type attributes 
 */

typedef NspGtkDialog NspGtkFontSelectionDialog ;
typedef NspTypeGtkDialog NspTypeGtkFontSelectionDialog ;

extern int nsp_type_gtkfontselectiondialog_id;
extern NspTypeGtkFontSelectionDialog *nsp_type_gtkfontselectiondialog;

/* type instances for gtkdialog */

NspTypeGtkFontSelectionDialog *new_type_gtkfontselectiondialog(type_mode mode);

/* instance for NspGtkFontSelectionDialog */

NspGtkFontSelectionDialog *new_gtkfontselectiondialog();

/*
 * Object methods redefined for gtkfontselectiondialog 
 */

#define NULLGTKFONTSELECTIONDIALOG (NspGtkFontSelectionDialog*) 0


/* from NspGtkFontSelectionDialogObj.c */

extern NspGtkFontSelectionDialog *nsp_gtkfontselectiondialog_object (NspObject *O);
extern int IsGtkFontSelectionDialogObj (Stack stack, int i);
extern int IsGtkFontSelectionDialog(NspObject *O);
extern NspGtkFontSelectionDialog *GetGtkFontSelectionDialogCopy (Stack stack, int i);
extern NspGtkFontSelectionDialog *GetGtkFontSelectionDialog (Stack stack, int i);

#endif /* NSP_INC_NspGtkFontSelectionDialog */ 

#ifdef NspGtkFontSelectionDialog_Private 
static int init_gtkfontselectiondialog(NspGtkFontSelectionDialog *o,NspTypeGtkFontSelectionDialog *type);
static char *nsp_gtkfontselectiondialog_type_as_string(void);
static char *nsp_gtkfontselectiondialog_type_short_string(NspObject *v);
static AttrTab gtkfontselectiondialog_attrs[];
static NspMethods *gtkfontselectiondialog_get_methods(void);
/* static int int_gtkfontselectiondialog_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFontSelectionDialog_Private */
