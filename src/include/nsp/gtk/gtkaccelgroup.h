/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAccelGroup
#define NSP_INC_NspGtkAccelGroup

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

/* NspGtkAccelGroup */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkAccelGroup inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkAccelGroup ;
typedef NspTypeGObject NspTypeGtkAccelGroup ;

extern int nsp_type_gtkaccelgroup_id;
extern NspTypeGtkAccelGroup *nsp_type_gtkaccelgroup;

/* type instances for gobject */

NspTypeGtkAccelGroup *new_type_gtkaccelgroup(type_mode mode);

/* instance for NspGtkAccelGroup */

NspGtkAccelGroup *new_gtkaccelgroup();

/*
 * Object methods redefined for gtkaccelgroup 
 */

#define NULLGTKACCELGROUP (NspGtkAccelGroup*) 0


/* from NspGtkAccelGroupObj.c */

extern NspGtkAccelGroup *nsp_gtkaccelgroup_object (NspObject *O);
extern int IsGtkAccelGroupObj (Stack stack, int i);
extern int IsGtkAccelGroup(NspObject *O);
extern NspGtkAccelGroup *GetGtkAccelGroupCopy (Stack stack, int i);
extern NspGtkAccelGroup *GetGtkAccelGroup (Stack stack, int i);

#endif /* NSP_INC_NspGtkAccelGroup */ 

#ifdef NspGtkAccelGroup_Private 
static int init_gtkaccelgroup(NspGtkAccelGroup *o,NspTypeGtkAccelGroup *type);
static char *nsp_gtkaccelgroup_type_as_string(void);
static char *nsp_gtkaccelgroup_type_short_string(NspObject *v);
static AttrTab gtkaccelgroup_attrs[];
static NspMethods *gtkaccelgroup_get_methods(void);
/* static int int_gtkaccelgroup_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAccelGroup_Private */
