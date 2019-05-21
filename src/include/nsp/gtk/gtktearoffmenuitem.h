/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTearoffMenuItem
#define NSP_INC_NspGtkTearoffMenuItem

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

/* NspGtkTearoffMenuItem */

#include <nsp/gtk/gtkmenuitem.h>

/*
 * NspGtkTearoffMenuItem inherits from GtkMenuItem
 * just change some type attributes 
 */

typedef NspGtkMenuItem NspGtkTearoffMenuItem ;
typedef NspTypeGtkMenuItem NspTypeGtkTearoffMenuItem ;

extern int nsp_type_gtktearoffmenuitem_id;
extern NspTypeGtkTearoffMenuItem *nsp_type_gtktearoffmenuitem;

/* type instances for gtkmenuitem */

NspTypeGtkTearoffMenuItem *new_type_gtktearoffmenuitem(type_mode mode);

/* instance for NspGtkTearoffMenuItem */

NspGtkTearoffMenuItem *new_gtktearoffmenuitem();

/*
 * Object methods redefined for gtktearoffmenuitem 
 */

#define NULLGTKTEAROFFMENUITEM (NspGtkTearoffMenuItem*) 0


/* from NspGtkTearoffMenuItemObj.c */

extern NspGtkTearoffMenuItem *nsp_gtktearoffmenuitem_object (NspObject *O);
extern int IsGtkTearoffMenuItemObj (Stack stack, int i);
extern int IsGtkTearoffMenuItem(NspObject *O);
extern NspGtkTearoffMenuItem *GetGtkTearoffMenuItemCopy (Stack stack, int i);
extern NspGtkTearoffMenuItem *GetGtkTearoffMenuItem (Stack stack, int i);

#endif /* NSP_INC_NspGtkTearoffMenuItem */ 

#ifdef NspGtkTearoffMenuItem_Private 
static int init_gtktearoffmenuitem(NspGtkTearoffMenuItem *o,NspTypeGtkTearoffMenuItem *type);
static char *nsp_gtktearoffmenuitem_type_as_string(void);
static char *nsp_gtktearoffmenuitem_type_short_string(NspObject *v);
static AttrTab gtktearoffmenuitem_attrs[];
static NspMethods *gtktearoffmenuitem_get_methods(void);
/* static int int_gtktearoffmenuitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTearoffMenuItem_Private */
