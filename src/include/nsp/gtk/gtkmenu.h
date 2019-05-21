/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkMenu
#define NSP_INC_NspGtkMenu

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

/* NspGtkMenu */

#include <nsp/gtk/gtkmenushell.h>

/*
 * NspGtkMenu inherits from GtkMenuShell
 * just change some type attributes 
 */

typedef NspGtkMenuShell NspGtkMenu ;
typedef NspTypeGtkMenuShell NspTypeGtkMenu ;

extern int nsp_type_gtkmenu_id;
extern NspTypeGtkMenu *nsp_type_gtkmenu;

/* type instances for gtkmenushell */

NspTypeGtkMenu *new_type_gtkmenu(type_mode mode);

/* instance for NspGtkMenu */

NspGtkMenu *new_gtkmenu();

/*
 * Object methods redefined for gtkmenu 
 */

#define NULLGTKMENU (NspGtkMenu*) 0


/* from NspGtkMenuObj.c */

extern NspGtkMenu *nsp_gtkmenu_object (NspObject *O);
extern int IsGtkMenuObj (Stack stack, int i);
extern int IsGtkMenu(NspObject *O);
extern NspGtkMenu *GetGtkMenuCopy (Stack stack, int i);
extern NspGtkMenu *GetGtkMenu (Stack stack, int i);

#endif /* NSP_INC_NspGtkMenu */ 

#ifdef NspGtkMenu_Private 
static int init_gtkmenu(NspGtkMenu *o,NspTypeGtkMenu *type);
static char *nsp_gtkmenu_type_as_string(void);
static char *nsp_gtkmenu_type_short_string(NspObject *v);
static AttrTab gtkmenu_attrs[];
static NspMethods *gtkmenu_get_methods(void);
/* static int int_gtkmenu_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkMenu_Private */
