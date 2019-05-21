/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkOptionMenu
#define NSP_INC_NspGtkOptionMenu

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

/* NspGtkOptionMenu */

#include <nsp/gtk/gtkbutton.h>

/*
 * NspGtkOptionMenu inherits from GtkButton
 * just change some type attributes 
 */

typedef NspGtkButton NspGtkOptionMenu ;
typedef NspTypeGtkButton NspTypeGtkOptionMenu ;

extern int nsp_type_gtkoptionmenu_id;
extern NspTypeGtkOptionMenu *nsp_type_gtkoptionmenu;

/* type instances for gtkbutton */

NspTypeGtkOptionMenu *new_type_gtkoptionmenu(type_mode mode);

/* instance for NspGtkOptionMenu */

NspGtkOptionMenu *new_gtkoptionmenu();

/*
 * Object methods redefined for gtkoptionmenu 
 */

#define NULLGTKOPTIONMENU (NspGtkOptionMenu*) 0


/* from NspGtkOptionMenuObj.c */

extern NspGtkOptionMenu *nsp_gtkoptionmenu_object (NspObject *O);
extern int IsGtkOptionMenuObj (Stack stack, int i);
extern int IsGtkOptionMenu(NspObject *O);
extern NspGtkOptionMenu *GetGtkOptionMenuCopy (Stack stack, int i);
extern NspGtkOptionMenu *GetGtkOptionMenu (Stack stack, int i);

#endif /* NSP_INC_NspGtkOptionMenu */ 

#ifdef NspGtkOptionMenu_Private 
static int init_gtkoptionmenu(NspGtkOptionMenu *o,NspTypeGtkOptionMenu *type);
static char *nsp_gtkoptionmenu_type_as_string(void);
static char *nsp_gtkoptionmenu_type_short_string(NspObject *v);
static AttrTab gtkoptionmenu_attrs[];
static NspMethods *gtkoptionmenu_get_methods(void);
/* static int int_gtkoptionmenu_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkOptionMenu_Private */
