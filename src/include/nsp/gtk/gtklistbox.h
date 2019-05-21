/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkListBox
#define NSP_INC_NspGtkListBox

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

/* NspGtkListBox */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkListBox inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkListBox ;
typedef NspTypeGtkContainer NspTypeGtkListBox ;

extern int nsp_type_gtklistbox_id;
extern NspTypeGtkListBox *nsp_type_gtklistbox;

/* type instances for gtkcontainer */

NspTypeGtkListBox *new_type_gtklistbox(type_mode mode);

/* instance for NspGtkListBox */

NspGtkListBox *new_gtklistbox();

/*
 * Object methods redefined for gtklistbox 
 */

#define NULLGTKLISTBOX (NspGtkListBox*) 0


/* from NspGtkListBoxObj.c */

extern NspGtkListBox *nsp_gtklistbox_object (NspObject *O);
extern int IsGtkListBoxObj (Stack stack, int i);
extern int IsGtkListBox(NspObject *O);
extern NspGtkListBox *GetGtkListBoxCopy (Stack stack, int i);
extern NspGtkListBox *GetGtkListBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkListBox */ 

#ifdef NspGtkListBox_Private 
static int init_gtklistbox(NspGtkListBox *o,NspTypeGtkListBox *type);
static char *nsp_gtklistbox_type_as_string(void);
static char *nsp_gtklistbox_type_short_string(NspObject *v);
static AttrTab gtklistbox_attrs[];
static NspMethods *gtklistbox_get_methods(void);
/* static int int_gtklistbox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkListBox_Private */
