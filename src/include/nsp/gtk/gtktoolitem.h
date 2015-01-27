/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkToolItem
#define NSP_INC_NspGtkToolItem

/*
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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

/* NspGtkToolItem */

#include <nsp/gtk/gtkbin.h>

/*
* NspGtkToolItem inherits from GtkBin
* just change some type attributes 
*/

typedef NspGtkBin NspGtkToolItem ;
typedef NspTypeGtkBin NspTypeGtkToolItem ;

extern int nsp_type_gtktoolitem_id;
extern NspTypeGtkToolItem *nsp_type_gtktoolitem;

/* type instances for gtkbin */

NspTypeGtkToolItem *new_type_gtktoolitem(type_mode mode);

/* instance for NspGtkToolItem */

NspGtkToolItem *new_gtktoolitem();

/*
* Object methods redefined for gtktoolitem 
*/

#define NULLGTKTOOLITEM (NspGtkToolItem*) 0


/* from NspGtkToolItemObj.c */

extern NspGtkToolItem *nsp_gtktoolitem_object (NspObject *O); 
extern int IsGtkToolItemObj (Stack stack, int i); 
extern int IsGtkToolItem(NspObject *O);
extern NspGtkToolItem *GetGtkToolItemCopy (Stack stack, int i); 
extern NspGtkToolItem *GetGtkToolItem (Stack stack, int i); 

#endif /* NSP_INC_NspGtkToolItem */

#ifdef NspGtkToolItem_Private 
static int init_gtktoolitem(NspGtkToolItem *o,NspTypeGtkToolItem *type);
static char *nsp_gtktoolitem_type_as_string(void);
static char *nsp_gtktoolitem_type_short_string(NspObject *v);
static AttrTab gtktoolitem_attrs[];
static NspMethods *gtktoolitem_get_methods(void); 
/* static int int_gtktoolitem_create(Stack stack, int rhs, int opt, int lhs);*/
#endif /* NspGtkToolItem_Private */
