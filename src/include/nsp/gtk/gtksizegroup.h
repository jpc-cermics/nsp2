/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSizeGroup
#define NSP_INC_NspGtkSizeGroup

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

/* NspGtkSizeGroup */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSizeGroup inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSizeGroup ;
typedef NspTypeGObject NspTypeGtkSizeGroup ;

extern int nsp_type_gtksizegroup_id;
extern NspTypeGtkSizeGroup *nsp_type_gtksizegroup;

/* type instances for gobject */

NspTypeGtkSizeGroup *new_type_gtksizegroup(type_mode mode);

/* instance for NspGtkSizeGroup */

NspGtkSizeGroup *new_gtksizegroup();

/*
 * Object methods redefined for gtksizegroup 
 */

#define NULLGTKSIZEGROUP (NspGtkSizeGroup*) 0


/* from NspGtkSizeGroupObj.c */

extern NspGtkSizeGroup *nsp_gtksizegroup_object (NspObject *O);
extern int IsGtkSizeGroupObj (Stack stack, int i);
extern int IsGtkSizeGroup(NspObject *O);
extern NspGtkSizeGroup *GetGtkSizeGroupCopy (Stack stack, int i);
extern NspGtkSizeGroup *GetGtkSizeGroup (Stack stack, int i);

#endif /* NSP_INC_NspGtkSizeGroup */ 

#ifdef NspGtkSizeGroup_Private 
static int init_gtksizegroup(NspGtkSizeGroup *o,NspTypeGtkSizeGroup *type);
static char *nsp_gtksizegroup_type_as_string(void);
static char *nsp_gtksizegroup_type_short_string(NspObject *v);
static AttrTab gtksizegroup_attrs[];
static NspMethods *gtksizegroup_get_methods(void);
/* static int int_gtksizegroup_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSizeGroup_Private */
