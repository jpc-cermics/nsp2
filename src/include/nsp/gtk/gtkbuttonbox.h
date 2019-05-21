/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkButtonBox
#define NSP_INC_NspGtkButtonBox

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

/* NspGtkButtonBox */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkButtonBox inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkButtonBox ;
typedef NspTypeGtkBox NspTypeGtkButtonBox ;

extern int nsp_type_gtkbuttonbox_id;
extern NspTypeGtkButtonBox *nsp_type_gtkbuttonbox;

/* type instances for gtkbox */

NspTypeGtkButtonBox *new_type_gtkbuttonbox(type_mode mode);

/* instance for NspGtkButtonBox */

NspGtkButtonBox *new_gtkbuttonbox();

/*
 * Object methods redefined for gtkbuttonbox 
 */

#define NULLGTKBUTTONBOX (NspGtkButtonBox*) 0


/* from NspGtkButtonBoxObj.c */

extern NspGtkButtonBox *nsp_gtkbuttonbox_object (NspObject *O);
extern int IsGtkButtonBoxObj (Stack stack, int i);
extern int IsGtkButtonBox(NspObject *O);
extern NspGtkButtonBox *GetGtkButtonBoxCopy (Stack stack, int i);
extern NspGtkButtonBox *GetGtkButtonBox (Stack stack, int i);

#endif /* NSP_INC_NspGtkButtonBox */ 

#ifdef NspGtkButtonBox_Private 
static int init_gtkbuttonbox(NspGtkButtonBox *o,NspTypeGtkButtonBox *type);
static char *nsp_gtkbuttonbox_type_as_string(void);
static char *nsp_gtkbuttonbox_type_short_string(NspObject *v);
static AttrTab gtkbuttonbox_attrs[];
static NspMethods *gtkbuttonbox_get_methods(void);
/* static int int_gtkbuttonbox_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkButtonBox_Private */
