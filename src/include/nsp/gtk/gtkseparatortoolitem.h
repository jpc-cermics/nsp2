/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSeparatorToolItem
#define NSP_INC_NspGtkSeparatorToolItem

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

/* NspGtkSeparatorToolItem */

#include <nsp/gtk/gtktoolitem.h>

/*
 * NspGtkSeparatorToolItem inherits from GtkToolItem
 * just change some type attributes 
 */

typedef NspGtkToolItem NspGtkSeparatorToolItem ;
typedef NspTypeGtkToolItem NspTypeGtkSeparatorToolItem ;

extern int nsp_type_gtkseparatortoolitem_id;
extern NspTypeGtkSeparatorToolItem *nsp_type_gtkseparatortoolitem;

/* type instances for gtktoolitem */

NspTypeGtkSeparatorToolItem *new_type_gtkseparatortoolitem(type_mode mode);

/* instance for NspGtkSeparatorToolItem */

NspGtkSeparatorToolItem *new_gtkseparatortoolitem();

/*
 * Object methods redefined for gtkseparatortoolitem 
 */

#define NULLGTKSEPARATORTOOLITEM (NspGtkSeparatorToolItem*) 0


/* from NspGtkSeparatorToolItemObj.c */

extern NspGtkSeparatorToolItem *nsp_gtkseparatortoolitem_object (NspObject *O);
extern int IsGtkSeparatorToolItemObj (Stack stack, int i);
extern int IsGtkSeparatorToolItem(NspObject *O);
extern NspGtkSeparatorToolItem *GetGtkSeparatorToolItemCopy (Stack stack, int i);
extern NspGtkSeparatorToolItem *GetGtkSeparatorToolItem (Stack stack, int i);

#endif /* NSP_INC_NspGtkSeparatorToolItem */ 

#ifdef NspGtkSeparatorToolItem_Private 
static int init_gtkseparatortoolitem(NspGtkSeparatorToolItem *o,NspTypeGtkSeparatorToolItem *type);
static char *nsp_gtkseparatortoolitem_type_as_string(void);
static char *nsp_gtkseparatortoolitem_type_short_string(NspObject *v);
static AttrTab gtkseparatortoolitem_attrs[];
static NspMethods *gtkseparatortoolitem_get_methods(void);
/* static int int_gtkseparatortoolitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSeparatorToolItem_Private */
