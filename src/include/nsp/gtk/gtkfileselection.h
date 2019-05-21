/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFileSelection
#define NSP_INC_NspGtkFileSelection

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

/* NspGtkFileSelection */

#include <nsp/gtk/gtkdialog.h>

/*
 * NspGtkFileSelection inherits from GtkDialog
 * just change some type attributes 
 */

typedef NspGtkDialog NspGtkFileSelection ;
typedef NspTypeGtkDialog NspTypeGtkFileSelection ;

extern int nsp_type_gtkfileselection_id;
extern NspTypeGtkFileSelection *nsp_type_gtkfileselection;

/* type instances for gtkdialog */

NspTypeGtkFileSelection *new_type_gtkfileselection(type_mode mode);

/* instance for NspGtkFileSelection */

NspGtkFileSelection *new_gtkfileselection();

/*
 * Object methods redefined for gtkfileselection 
 */

#define NULLGTKFILESELECTION (NspGtkFileSelection*) 0


/* from NspGtkFileSelectionObj.c */

extern NspGtkFileSelection *nsp_gtkfileselection_object (NspObject *O);
extern int IsGtkFileSelectionObj (Stack stack, int i);
extern int IsGtkFileSelection(NspObject *O);
extern NspGtkFileSelection *GetGtkFileSelectionCopy (Stack stack, int i);
extern NspGtkFileSelection *GetGtkFileSelection (Stack stack, int i);

#endif /* NSP_INC_NspGtkFileSelection */ 

#ifdef NspGtkFileSelection_Private 
static int init_gtkfileselection(NspGtkFileSelection *o,NspTypeGtkFileSelection *type);
static char *nsp_gtkfileselection_type_as_string(void);
static char *nsp_gtkfileselection_type_short_string(NspObject *v);
static AttrTab gtkfileselection_attrs[];
static NspMethods *gtkfileselection_get_methods(void);
/* static int int_gtkfileselection_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFileSelection_Private */
