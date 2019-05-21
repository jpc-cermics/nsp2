/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkOldEditable
#define NSP_INC_NspGtkOldEditable

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

/* NspGtkOldEditable */

#include <nsp/gtk/gtkwidget.h>

/*
 * NspGtkOldEditable inherits from GtkWidget
 * just change some type attributes 
 */

typedef NspGtkWidget NspGtkOldEditable ;
typedef NspTypeGtkWidget NspTypeGtkOldEditable ;

extern int nsp_type_gtkoldeditable_id;
extern NspTypeGtkOldEditable *nsp_type_gtkoldeditable;

/* type instances for gtkwidget */

NspTypeGtkOldEditable *new_type_gtkoldeditable(type_mode mode);

/* instance for NspGtkOldEditable */

NspGtkOldEditable *new_gtkoldeditable();

/*
 * Object methods redefined for gtkoldeditable 
 */

#define NULLGTKOLDEDITABLE (NspGtkOldEditable*) 0


/* from NspGtkOldEditableObj.c */

extern NspGtkOldEditable *nsp_gtkoldeditable_object (NspObject *O);
extern int IsGtkOldEditableObj (Stack stack, int i);
extern int IsGtkOldEditable(NspObject *O);
extern NspGtkOldEditable *GetGtkOldEditableCopy (Stack stack, int i);
extern NspGtkOldEditable *GetGtkOldEditable (Stack stack, int i);

#endif /* NSP_INC_NspGtkOldEditable */ 

#ifdef NspGtkOldEditable_Private 
static int init_gtkoldeditable(NspGtkOldEditable *o,NspTypeGtkOldEditable *type);
static char *nsp_gtkoldeditable_type_as_string(void);
static char *nsp_gtkoldeditable_type_short_string(NspObject *v);
static AttrTab gtkoldeditable_attrs[];
static NspMethods *gtkoldeditable_get_methods(void);
/* static int int_gtkoldeditable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkOldEditable_Private */
