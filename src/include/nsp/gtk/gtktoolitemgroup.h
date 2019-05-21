/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkToolItemGroup
#define NSP_INC_NspGtkToolItemGroup

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

/* NspGtkToolItemGroup */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkToolItemGroup inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkToolItemGroup ;
typedef NspTypeGtkContainer NspTypeGtkToolItemGroup ;

extern int nsp_type_gtktoolitemgroup_id;
extern NspTypeGtkToolItemGroup *nsp_type_gtktoolitemgroup;

/* type instances for gtkcontainer */

NspTypeGtkToolItemGroup *new_type_gtktoolitemgroup(type_mode mode);

/* instance for NspGtkToolItemGroup */

NspGtkToolItemGroup *new_gtktoolitemgroup();

/*
 * Object methods redefined for gtktoolitemgroup 
 */

#define NULLGTKTOOLITEMGROUP (NspGtkToolItemGroup*) 0


/* from NspGtkToolItemGroupObj.c */

extern NspGtkToolItemGroup *nsp_gtktoolitemgroup_object (NspObject *O);
extern int IsGtkToolItemGroupObj (Stack stack, int i);
extern int IsGtkToolItemGroup(NspObject *O);
extern NspGtkToolItemGroup *GetGtkToolItemGroupCopy (Stack stack, int i);
extern NspGtkToolItemGroup *GetGtkToolItemGroup (Stack stack, int i);

#endif /* NSP_INC_NspGtkToolItemGroup */ 

#ifdef NspGtkToolItemGroup_Private 
static int init_gtktoolitemgroup(NspGtkToolItemGroup *o,NspTypeGtkToolItemGroup *type);
static char *nsp_gtktoolitemgroup_type_as_string(void);
static char *nsp_gtktoolitemgroup_type_short_string(NspObject *v);
static AttrTab gtktoolitemgroup_attrs[];
static NspMethods *gtktoolitemgroup_get_methods(void);
/* static int int_gtktoolitemgroup_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkToolItemGroup_Private */
