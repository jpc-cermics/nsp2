/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkItem
#define NSP_INC_NspGtkItem

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

/* NspGtkItem */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkItem inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkItem ;
typedef NspTypeGtkBin NspTypeGtkItem ;

extern int nsp_type_gtkitem_id;
extern NspTypeGtkItem *nsp_type_gtkitem;

/* type instances for gtkbin */

NspTypeGtkItem *new_type_gtkitem(type_mode mode);

/* instance for NspGtkItem */

NspGtkItem *new_gtkitem();

/*
 * Object methods redefined for gtkitem 
 */

#define NULLGTKITEM (NspGtkItem*) 0


/* from NspGtkItemObj.c */

extern NspGtkItem *nsp_gtkitem_object (NspObject *O);
extern int IsGtkItemObj (Stack stack, int i);
extern int IsGtkItem(NspObject *O);
extern NspGtkItem *GetGtkItemCopy (Stack stack, int i);
extern NspGtkItem *GetGtkItem (Stack stack, int i);

#endif /* NSP_INC_NspGtkItem */ 

#ifdef NspGtkItem_Private 
static int init_gtkitem(NspGtkItem *o,NspTypeGtkItem *type);
static char *nsp_gtkitem_type_as_string(void);
static char *nsp_gtkitem_type_short_string(NspObject *v);
static AttrTab gtkitem_attrs[];
static NspMethods *gtkitem_get_methods(void);
/* static int int_gtkitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkItem_Private */
