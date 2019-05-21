/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkBuildable
#define NSP_INC_NspGtkBuildable

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

/* NspGtkBuildable */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkBuildable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkBuildable ;
typedef NspTypeGObject NspTypeGtkBuildable ;

extern int nsp_type_gtkbuildable_id;
extern NspTypeGtkBuildable *nsp_type_gtkbuildable;

/* type instances for gobject */

NspTypeGtkBuildable *new_type_gtkbuildable(type_mode mode);

/* instance for NspGtkBuildable */

NspGtkBuildable *new_gtkbuildable();

/*
 * Object methods redefined for gtkbuildable 
 */

#define NULLGTKBUILDABLE (NspGtkBuildable*) 0


/* from NspGtkBuildableObj.c */

extern NspGtkBuildable *nsp_gtkbuildable_object (NspObject *O);
extern int IsGtkBuildableObj (Stack stack, int i);
extern int IsGtkBuildable(NspObject *O);
extern NspGtkBuildable *GetGtkBuildableCopy (Stack stack, int i);
extern NspGtkBuildable *GetGtkBuildable (Stack stack, int i);

#endif /* NSP_INC_NspGtkBuildable */ 

#ifdef NspGtkBuildable_Private 
static int init_gtkbuildable(NspGtkBuildable *o,NspTypeGtkBuildable *type);
static char *nsp_gtkbuildable_type_as_string(void);
static char *nsp_gtkbuildable_type_short_string(NspObject *v);
static AttrTab gtkbuildable_attrs[];
static NspMethods *gtkbuildable_get_methods(void);
/* static int int_gtkbuildable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkBuildable_Private */
