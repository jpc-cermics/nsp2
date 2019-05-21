/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSeparatorMenuItem
#define NSP_INC_NspGtkSeparatorMenuItem

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

/* NspGtkSeparatorMenuItem */

#include <nsp/gtk/gtkmenuitem.h>

/*
 * NspGtkSeparatorMenuItem inherits from GtkMenuItem
 * just change some type attributes 
 */

typedef NspGtkMenuItem NspGtkSeparatorMenuItem ;
typedef NspTypeGtkMenuItem NspTypeGtkSeparatorMenuItem ;

extern int nsp_type_gtkseparatormenuitem_id;
extern NspTypeGtkSeparatorMenuItem *nsp_type_gtkseparatormenuitem;

/* type instances for gtkmenuitem */

NspTypeGtkSeparatorMenuItem *new_type_gtkseparatormenuitem(type_mode mode);

/* instance for NspGtkSeparatorMenuItem */

NspGtkSeparatorMenuItem *new_gtkseparatormenuitem();

/*
 * Object methods redefined for gtkseparatormenuitem 
 */

#define NULLGTKSEPARATORMENUITEM (NspGtkSeparatorMenuItem*) 0


/* from NspGtkSeparatorMenuItemObj.c */

extern NspGtkSeparatorMenuItem *nsp_gtkseparatormenuitem_object (NspObject *O);
extern int IsGtkSeparatorMenuItemObj (Stack stack, int i);
extern int IsGtkSeparatorMenuItem(NspObject *O);
extern NspGtkSeparatorMenuItem *GetGtkSeparatorMenuItemCopy (Stack stack, int i);
extern NspGtkSeparatorMenuItem *GetGtkSeparatorMenuItem (Stack stack, int i);

#endif /* NSP_INC_NspGtkSeparatorMenuItem */ 

#ifdef NspGtkSeparatorMenuItem_Private 
static int init_gtkseparatormenuitem(NspGtkSeparatorMenuItem *o,NspTypeGtkSeparatorMenuItem *type);
static char *nsp_gtkseparatormenuitem_type_as_string(void);
static char *nsp_gtkseparatormenuitem_type_short_string(NspObject *v);
static AttrTab gtkseparatormenuitem_attrs[];
static NspMethods *gtkseparatormenuitem_get_methods(void);
/* static int int_gtkseparatormenuitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSeparatorMenuItem_Private */
