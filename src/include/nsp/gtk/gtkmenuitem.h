/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkMenuItem
#define NSP_INC_NspGtkMenuItem

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

/* NspGtkMenuItem */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkMenuItem inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkMenuItem ;
typedef NspTypeGtkBin NspTypeGtkMenuItem ;

extern int nsp_type_gtkmenuitem_id;
extern NspTypeGtkMenuItem *nsp_type_gtkmenuitem;

/* type instances for gtkbin */

NspTypeGtkMenuItem *new_type_gtkmenuitem(type_mode mode);

/* instance for NspGtkMenuItem */

NspGtkMenuItem *new_gtkmenuitem();

/*
 * Object methods redefined for gtkmenuitem 
 */

#define NULLGTKMENUITEM (NspGtkMenuItem*) 0


/* from NspGtkMenuItemObj.c */

extern NspGtkMenuItem *nsp_gtkmenuitem_object (NspObject *O);
extern int IsGtkMenuItemObj (Stack stack, int i);
extern int IsGtkMenuItem(NspObject *O);
extern NspGtkMenuItem *GetGtkMenuItemCopy (Stack stack, int i);
extern NspGtkMenuItem *GetGtkMenuItem (Stack stack, int i);

#endif /* NSP_INC_NspGtkMenuItem */ 

#ifdef NspGtkMenuItem_Private 
static int init_gtkmenuitem(NspGtkMenuItem *o,NspTypeGtkMenuItem *type);
static char *nsp_gtkmenuitem_type_as_string(void);
static char *nsp_gtkmenuitem_type_short_string(NspObject *v);
static AttrTab gtkmenuitem_attrs[];
static NspMethods *gtkmenuitem_get_methods(void);
/* static int int_gtkmenuitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkMenuItem_Private */
