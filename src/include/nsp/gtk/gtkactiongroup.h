/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkActionGroup
#define NSP_INC_NspGtkActionGroup

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

/* NspGtkActionGroup */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkActionGroup inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkActionGroup ;
typedef NspTypeGObject NspTypeGtkActionGroup ;

extern int nsp_type_gtkactiongroup_id;
extern NspTypeGtkActionGroup *nsp_type_gtkactiongroup;

/* type instances for gobject */

NspTypeGtkActionGroup *new_type_gtkactiongroup(type_mode mode);

/* instance for NspGtkActionGroup */

NspGtkActionGroup *new_gtkactiongroup();

/*
 * Object methods redefined for gtkactiongroup 
 */

#define NULLGTKACTIONGROUP (NspGtkActionGroup*) 0


/* from NspGtkActionGroupObj.c */

extern NspGtkActionGroup *nsp_gtkactiongroup_object (NspObject *O);
extern int IsGtkActionGroupObj (Stack stack, int i);
extern int IsGtkActionGroup(NspObject *O);
extern NspGtkActionGroup *GetGtkActionGroupCopy (Stack stack, int i);
extern NspGtkActionGroup *GetGtkActionGroup (Stack stack, int i);

#endif /* NSP_INC_NspGtkActionGroup */ 

#ifdef NspGtkActionGroup_Private 
static int init_gtkactiongroup(NspGtkActionGroup *o,NspTypeGtkActionGroup *type);
static char *nsp_gtkactiongroup_type_as_string(void);
static char *nsp_gtkactiongroup_type_short_string(NspObject *v);
static AttrTab gtkactiongroup_attrs[];
static NspMethods *gtkactiongroup_get_methods(void);
/* static int int_gtkactiongroup_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkActionGroup_Private */
