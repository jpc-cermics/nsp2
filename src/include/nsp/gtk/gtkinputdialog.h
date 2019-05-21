/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkInputDialog
#define NSP_INC_NspGtkInputDialog

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

/* NspGtkInputDialog */

#include <nsp/gtk/gtkdialog.h>

/*
 * NspGtkInputDialog inherits from GtkDialog
 * just change some type attributes 
 */

typedef NspGtkDialog NspGtkInputDialog ;
typedef NspTypeGtkDialog NspTypeGtkInputDialog ;

extern int nsp_type_gtkinputdialog_id;
extern NspTypeGtkInputDialog *nsp_type_gtkinputdialog;

/* type instances for gtkdialog */

NspTypeGtkInputDialog *new_type_gtkinputdialog(type_mode mode);

/* instance for NspGtkInputDialog */

NspGtkInputDialog *new_gtkinputdialog();

/*
 * Object methods redefined for gtkinputdialog 
 */

#define NULLGTKINPUTDIALOG (NspGtkInputDialog*) 0


/* from NspGtkInputDialogObj.c */

extern NspGtkInputDialog *nsp_gtkinputdialog_object (NspObject *O);
extern int IsGtkInputDialogObj (Stack stack, int i);
extern int IsGtkInputDialog(NspObject *O);
extern NspGtkInputDialog *GetGtkInputDialogCopy (Stack stack, int i);
extern NspGtkInputDialog *GetGtkInputDialog (Stack stack, int i);

#endif /* NSP_INC_NspGtkInputDialog */ 

#ifdef NspGtkInputDialog_Private 
static int init_gtkinputdialog(NspGtkInputDialog *o,NspTypeGtkInputDialog *type);
static char *nsp_gtkinputdialog_type_as_string(void);
static char *nsp_gtkinputdialog_type_short_string(NspObject *v);
static AttrTab gtkinputdialog_attrs[];
static NspMethods *gtkinputdialog_get_methods(void);
/* static int int_gtkinputdialog_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkInputDialog_Private */
