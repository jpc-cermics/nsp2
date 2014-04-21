/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAboutDialog
#define NSP_INC_NspGtkAboutDialog

/*
 * Copyright (C) 1998-2014 Jean-Philippe Chancelier Enpc/Cermics
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

/* NspGtkAboutDialog */

#include <nsp/gtk/gtkdialog.h>

/*
* NspGtkAboutDialog inherits from GtkDialog
* just change some type attributes 
*/

typedef NspGtkDialog NspGtkAboutDialog ;
typedef NspTypeGtkDialog NspTypeGtkAboutDialog ;

extern int nsp_type_gtkaboutdialog_id;
extern NspTypeGtkAboutDialog *nsp_type_gtkaboutdialog;

/* type instances for gtkdialog */

NspTypeGtkAboutDialog *new_type_gtkaboutdialog(type_mode mode);

/* instance for NspGtkAboutDialog */

NspGtkAboutDialog *new_gtkaboutdialog();

/*
* Object methods redefined for gtkaboutdialog 
*/

#define NULLGTKABOUTDIALOG (NspGtkAboutDialog*) 0


/* from NspGtkAboutDialogObj.c */

extern NspGtkAboutDialog *nsp_gtkaboutdialog_object (NspObject *O); 
extern int IsGtkAboutDialogObj (Stack stack, int i); 
extern int IsGtkAboutDialog(NspObject *O);
extern NspGtkAboutDialog *GetGtkAboutDialogCopy (Stack stack, int i); 
extern NspGtkAboutDialog *GetGtkAboutDialog (Stack stack, int i); 

#endif /* NSP_INC_NspGtkAboutDialog */

#ifdef NspGtkAboutDialog_Private 
static int init_gtkaboutdialog(NspGtkAboutDialog *o,NspTypeGtkAboutDialog *type);
static char *nsp_gtkaboutdialog_type_as_string(void);
static char *nsp_gtkaboutdialog_type_short_string(NspObject *v);
static AttrTab gtkaboutdialog_attrs[];
static NspMethods *gtkaboutdialog_get_methods(void); 
/* static int int_gtkaboutdialog_create(Stack stack, int rhs, int opt, int lhs);*/
#endif /* NspGtkAboutDialog_Private */
