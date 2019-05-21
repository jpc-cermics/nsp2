/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkPlacesSidebar
#define NSP_INC_NspGtkPlacesSidebar

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

/* NspGtkPlacesSidebar */

#include <nsp/gtk/gtkscrolledwindow.h>

/*
 * NspGtkPlacesSidebar inherits from GtkScrolledWindow
 * just change some type attributes 
 */

typedef NspGtkScrolledWindow NspGtkPlacesSidebar ;
typedef NspTypeGtkScrolledWindow NspTypeGtkPlacesSidebar ;

extern int nsp_type_gtkplacessidebar_id;
extern NspTypeGtkPlacesSidebar *nsp_type_gtkplacessidebar;

/* type instances for gtkscrolledwindow */

NspTypeGtkPlacesSidebar *new_type_gtkplacessidebar(type_mode mode);

/* instance for NspGtkPlacesSidebar */

NspGtkPlacesSidebar *new_gtkplacessidebar();

/*
 * Object methods redefined for gtkplacessidebar 
 */

#define NULLGTKPLACESSIDEBAR (NspGtkPlacesSidebar*) 0


/* from NspGtkPlacesSidebarObj.c */

extern NspGtkPlacesSidebar *nsp_gtkplacessidebar_object (NspObject *O);
extern int IsGtkPlacesSidebarObj (Stack stack, int i);
extern int IsGtkPlacesSidebar(NspObject *O);
extern NspGtkPlacesSidebar *GetGtkPlacesSidebarCopy (Stack stack, int i);
extern NspGtkPlacesSidebar *GetGtkPlacesSidebar (Stack stack, int i);

#endif /* NSP_INC_NspGtkPlacesSidebar */ 

#ifdef NspGtkPlacesSidebar_Private 
static int init_gtkplacessidebar(NspGtkPlacesSidebar *o,NspTypeGtkPlacesSidebar *type);
static char *nsp_gtkplacessidebar_type_as_string(void);
static char *nsp_gtkplacessidebar_type_short_string(NspObject *v);
static AttrTab gtkplacessidebar_attrs[];
static NspMethods *gtkplacessidebar_get_methods(void);
/* static int int_gtkplacessidebar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkPlacesSidebar_Private */
