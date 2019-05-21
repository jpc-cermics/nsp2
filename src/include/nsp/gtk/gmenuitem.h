/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMenuItem
#define NSP_INC_NspGMenuItem

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

/* NspGMenuItem */

#include <nsp/gtk/gobject.h>

/*
 * NspGMenuItem inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGMenuItem ;
typedef NspTypeGObject NspTypeGMenuItem ;

extern int nsp_type_gmenuitem_id;
extern NspTypeGMenuItem *nsp_type_gmenuitem;

/* type instances for gobject */

NspTypeGMenuItem *new_type_gmenuitem(type_mode mode);

/* instance for NspGMenuItem */

NspGMenuItem *new_gmenuitem();

/*
 * Object methods redefined for gmenuitem 
 */

#define NULLGMENUITEM (NspGMenuItem*) 0


/* from NspGMenuItemObj.c */

extern NspGMenuItem *nsp_gmenuitem_object (NspObject *O);
extern int IsGMenuItemObj (Stack stack, int i);
extern int IsGMenuItem(NspObject *O);
extern NspGMenuItem *GetGMenuItemCopy (Stack stack, int i);
extern NspGMenuItem *GetGMenuItem (Stack stack, int i);

#endif /* NSP_INC_NspGMenuItem */ 

#ifdef NspGMenuItem_Private 
static int init_gmenuitem(NspGMenuItem *o,NspTypeGMenuItem *type);
static char *nsp_gmenuitem_type_as_string(void);
static char *nsp_gmenuitem_type_short_string(NspObject *v);
static AttrTab gmenuitem_attrs[];
static NspMethods *gmenuitem_get_methods(void);
/* static int int_gmenuitem_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMenuItem_Private */
