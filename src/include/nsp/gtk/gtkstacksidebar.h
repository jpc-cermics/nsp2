/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkStackSidebar
#define NSP_INC_NspGtkStackSidebar

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

/* NspGtkStackSidebar */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkStackSidebar inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkStackSidebar ;
typedef NspTypeGtkBin NspTypeGtkStackSidebar ;

extern int nsp_type_gtkstacksidebar_id;
extern NspTypeGtkStackSidebar *nsp_type_gtkstacksidebar;

/* type instances for gtkbin */

NspTypeGtkStackSidebar *new_type_gtkstacksidebar(type_mode mode);

/* instance for NspGtkStackSidebar */

NspGtkStackSidebar *new_gtkstacksidebar();

/*
 * Object methods redefined for gtkstacksidebar 
 */

#define NULLGTKSTACKSIDEBAR (NspGtkStackSidebar*) 0


/* from NspGtkStackSidebarObj.c */

extern NspGtkStackSidebar *nsp_gtkstacksidebar_object (NspObject *O);
extern int IsGtkStackSidebarObj (Stack stack, int i);
extern int IsGtkStackSidebar(NspObject *O);
extern NspGtkStackSidebar *GetGtkStackSidebarCopy (Stack stack, int i);
extern NspGtkStackSidebar *GetGtkStackSidebar (Stack stack, int i);

#endif /* NSP_INC_NspGtkStackSidebar */ 

#ifdef NspGtkStackSidebar_Private 
static int init_gtkstacksidebar(NspGtkStackSidebar *o,NspTypeGtkStackSidebar *type);
static char *nsp_gtkstacksidebar_type_as_string(void);
static char *nsp_gtkstacksidebar_type_short_string(NspObject *v);
static AttrTab gtkstacksidebar_attrs[];
static NspMethods *gtkstacksidebar_get_methods(void);
/* static int int_gtkstacksidebar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkStackSidebar_Private */
