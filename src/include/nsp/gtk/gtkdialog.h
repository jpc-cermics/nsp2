/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkDialog
#define NSP_INC_NspGtkDialog

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

/* NspGtkDialog */

#include <nsp/gtk/gtkwindow.h>

/*
 * NspGtkDialog inherits from GtkWindow
 * just change some type attributes 
 */

typedef NspGtkWindow NspGtkDialog ;
typedef NspTypeGtkWindow NspTypeGtkDialog ;

extern int nsp_type_gtkdialog_id;
extern NspTypeGtkDialog *nsp_type_gtkdialog;

/* type instances for gtkwindow */

NspTypeGtkDialog *new_type_gtkdialog(type_mode mode);

/* instance for NspGtkDialog */

NspGtkDialog *new_gtkdialog();

/*
 * Object methods redefined for gtkdialog 
 */

#define NULLGTKDIALOG (NspGtkDialog*) 0


/* from NspGtkDialogObj.c */

extern NspGtkDialog *nsp_gtkdialog_object (NspObject *O);
extern int IsGtkDialogObj (Stack stack, int i);
extern int IsGtkDialog(NspObject *O);
extern NspGtkDialog *GetGtkDialogCopy (Stack stack, int i);
extern NspGtkDialog *GetGtkDialog (Stack stack, int i);

#endif /* NSP_INC_NspGtkDialog */ 

#ifdef NspGtkDialog_Private 
static int init_gtkdialog(NspGtkDialog *o,NspTypeGtkDialog *type);
static char *nsp_gtkdialog_type_as_string(void);
static char *nsp_gtkdialog_type_short_string(NspObject *v);
static AttrTab gtkdialog_attrs[];
static NspMethods *gtkdialog_get_methods(void);
/* static int int_gtkdialog_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkDialog_Private */
