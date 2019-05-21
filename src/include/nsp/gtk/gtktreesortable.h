/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeSortable
#define NSP_INC_NspGtkTreeSortable

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

/* NspGtkTreeSortable */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTreeSortable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTreeSortable ;
typedef NspTypeGObject NspTypeGtkTreeSortable ;

extern int nsp_type_gtktreesortable_id;
extern NspTypeGtkTreeSortable *nsp_type_gtktreesortable;

/* type instances for gobject */

NspTypeGtkTreeSortable *new_type_gtktreesortable(type_mode mode);

/* instance for NspGtkTreeSortable */

NspGtkTreeSortable *new_gtktreesortable();

/*
 * Object methods redefined for gtktreesortable 
 */

#define NULLGTKTREESORTABLE (NspGtkTreeSortable*) 0


/* from NspGtkTreeSortableObj.c */

extern NspGtkTreeSortable *nsp_gtktreesortable_object (NspObject *O);
extern int IsGtkTreeSortableObj (Stack stack, int i);
extern int IsGtkTreeSortable(NspObject *O);
extern NspGtkTreeSortable *GetGtkTreeSortableCopy (Stack stack, int i);
extern NspGtkTreeSortable *GetGtkTreeSortable (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeSortable */ 

#ifdef NspGtkTreeSortable_Private 
static int init_gtktreesortable(NspGtkTreeSortable *o,NspTypeGtkTreeSortable *type);
static char *nsp_gtktreesortable_type_as_string(void);
static char *nsp_gtktreesortable_type_short_string(NspObject *v);
static AttrTab gtktreesortable_attrs[];
static NspMethods *gtktreesortable_get_methods(void);
/* static int int_gtktreesortable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeSortable_Private */
