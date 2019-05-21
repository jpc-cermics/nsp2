/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGActionGroup
#define NSP_INC_NspGActionGroup

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

/* NspGActionGroup */

#include <nsp/gtk/gobject.h>

/*
 * NspGActionGroup inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGActionGroup ;
typedef NspTypeGObject NspTypeGActionGroup ;

extern int nsp_type_gactiongroup_id;
extern NspTypeGActionGroup *nsp_type_gactiongroup;

/* type instances for gobject */

NspTypeGActionGroup *new_type_gactiongroup(type_mode mode);

/* instance for NspGActionGroup */

NspGActionGroup *new_gactiongroup();

/*
 * Object methods redefined for gactiongroup 
 */

#define NULLGACTIONGROUP (NspGActionGroup*) 0


/* from NspGActionGroupObj.c */

extern NspGActionGroup *nsp_gactiongroup_object (NspObject *O);
extern int IsGActionGroupObj (Stack stack, int i);
extern int IsGActionGroup(NspObject *O);
extern NspGActionGroup *GetGActionGroupCopy (Stack stack, int i);
extern NspGActionGroup *GetGActionGroup (Stack stack, int i);

#endif /* NSP_INC_NspGActionGroup */ 

#ifdef NspGActionGroup_Private 
static int init_gactiongroup(NspGActionGroup *o,NspTypeGActionGroup *type);
static char *nsp_gactiongroup_type_as_string(void);
static char *nsp_gactiongroup_type_short_string(NspObject *v);
static AttrTab gactiongroup_attrs[];
static NspMethods *gactiongroup_get_methods(void);
/* static int int_gactiongroup_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGActionGroup_Private */
