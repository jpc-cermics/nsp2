/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkWindowGroup
#define NSP_INC_NspGtkWindowGroup

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

/* NspGtkWindowGroup */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkWindowGroup inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkWindowGroup ;
typedef NspTypeGObject NspTypeGtkWindowGroup ;

extern int nsp_type_gtkwindowgroup_id;
extern NspTypeGtkWindowGroup *nsp_type_gtkwindowgroup;

/* type instances for gobject */

NspTypeGtkWindowGroup *new_type_gtkwindowgroup(type_mode mode);

/* instance for NspGtkWindowGroup */

NspGtkWindowGroup *new_gtkwindowgroup();

/*
 * Object methods redefined for gtkwindowgroup 
 */

#define NULLGTKWINDOWGROUP (NspGtkWindowGroup*) 0


/* from NspGtkWindowGroupObj.c */

extern NspGtkWindowGroup *nsp_gtkwindowgroup_object (NspObject *O);
extern int IsGtkWindowGroupObj (Stack stack, int i);
extern int IsGtkWindowGroup(NspObject *O);
extern NspGtkWindowGroup *GetGtkWindowGroupCopy (Stack stack, int i);
extern NspGtkWindowGroup *GetGtkWindowGroup (Stack stack, int i);

#endif /* NSP_INC_NspGtkWindowGroup */ 

#ifdef NspGtkWindowGroup_Private 
static int init_gtkwindowgroup(NspGtkWindowGroup *o,NspTypeGtkWindowGroup *type);
static char *nsp_gtkwindowgroup_type_as_string(void);
static char *nsp_gtkwindowgroup_type_short_string(NspObject *v);
static AttrTab gtkwindowgroup_attrs[];
static NspMethods *gtkwindowgroup_get_methods(void);
/* static int int_gtkwindowgroup_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkWindowGroup_Private */
