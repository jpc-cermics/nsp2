/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkStackSwitcher
#define NSP_INC_NspGtkStackSwitcher

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

/* NspGtkStackSwitcher */

#include <nsp/gtk/gtkbox.h>

/*
 * NspGtkStackSwitcher inherits from GtkBox
 * just change some type attributes 
 */

typedef NspGtkBox NspGtkStackSwitcher ;
typedef NspTypeGtkBox NspTypeGtkStackSwitcher ;

extern int nsp_type_gtkstackswitcher_id;
extern NspTypeGtkStackSwitcher *nsp_type_gtkstackswitcher;

/* type instances for gtkbox */

NspTypeGtkStackSwitcher *new_type_gtkstackswitcher(type_mode mode);

/* instance for NspGtkStackSwitcher */

NspGtkStackSwitcher *new_gtkstackswitcher();

/*
 * Object methods redefined for gtkstackswitcher 
 */

#define NULLGTKSTACKSWITCHER (NspGtkStackSwitcher*) 0


/* from NspGtkStackSwitcherObj.c */

extern NspGtkStackSwitcher *nsp_gtkstackswitcher_object (NspObject *O);
extern int IsGtkStackSwitcherObj (Stack stack, int i);
extern int IsGtkStackSwitcher(NspObject *O);
extern NspGtkStackSwitcher *GetGtkStackSwitcherCopy (Stack stack, int i);
extern NspGtkStackSwitcher *GetGtkStackSwitcher (Stack stack, int i);

#endif /* NSP_INC_NspGtkStackSwitcher */ 

#ifdef NspGtkStackSwitcher_Private 
static int init_gtkstackswitcher(NspGtkStackSwitcher *o,NspTypeGtkStackSwitcher *type);
static char *nsp_gtkstackswitcher_type_as_string(void);
static char *nsp_gtkstackswitcher_type_short_string(NspObject *v);
static AttrTab gtkstackswitcher_attrs[];
static NspMethods *gtkstackswitcher_get_methods(void);
/* static int int_gtkstackswitcher_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkStackSwitcher_Private */
