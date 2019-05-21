/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkPopoverMenu
#define NSP_INC_NspGtkPopoverMenu

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

/* NspGtkPopoverMenu */

#include <nsp/gtk/gtkpopover.h>

/*
 * NspGtkPopoverMenu inherits from GtkPopover
 * just change some type attributes 
 */

typedef NspGtkPopover NspGtkPopoverMenu ;
typedef NspTypeGtkPopover NspTypeGtkPopoverMenu ;

extern int nsp_type_gtkpopovermenu_id;
extern NspTypeGtkPopoverMenu *nsp_type_gtkpopovermenu;

/* type instances for gtkpopover */

NspTypeGtkPopoverMenu *new_type_gtkpopovermenu(type_mode mode);

/* instance for NspGtkPopoverMenu */

NspGtkPopoverMenu *new_gtkpopovermenu();

/*
 * Object methods redefined for gtkpopovermenu 
 */

#define NULLGTKPOPOVERMENU (NspGtkPopoverMenu*) 0


/* from NspGtkPopoverMenuObj.c */

extern NspGtkPopoverMenu *nsp_gtkpopovermenu_object (NspObject *O);
extern int IsGtkPopoverMenuObj (Stack stack, int i);
extern int IsGtkPopoverMenu(NspObject *O);
extern NspGtkPopoverMenu *GetGtkPopoverMenuCopy (Stack stack, int i);
extern NspGtkPopoverMenu *GetGtkPopoverMenu (Stack stack, int i);

#endif /* NSP_INC_NspGtkPopoverMenu */ 

#ifdef NspGtkPopoverMenu_Private 
static int init_gtkpopovermenu(NspGtkPopoverMenu *o,NspTypeGtkPopoverMenu *type);
static char *nsp_gtkpopovermenu_type_as_string(void);
static char *nsp_gtkpopovermenu_type_short_string(NspObject *v);
static AttrTab gtkpopovermenu_attrs[];
static NspMethods *gtkpopovermenu_get_methods(void);
/* static int int_gtkpopovermenu_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkPopoverMenu_Private */
