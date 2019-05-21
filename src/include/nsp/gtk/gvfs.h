/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGVfs
#define NSP_INC_NspGVfs

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

/* NspGVfs */

#include <nsp/gtk/gobject.h>

/*
 * NspGVfs inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGVfs ;
typedef NspTypeGObject NspTypeGVfs ;

extern int nsp_type_gvfs_id;
extern NspTypeGVfs *nsp_type_gvfs;

/* type instances for gobject */

NspTypeGVfs *new_type_gvfs(type_mode mode);

/* instance for NspGVfs */

NspGVfs *new_gvfs();

/*
 * Object methods redefined for gvfs 
 */

#define NULLGVFS (NspGVfs*) 0


/* from NspGVfsObj.c */

extern NspGVfs *nsp_gvfs_object (NspObject *O);
extern int IsGVfsObj (Stack stack, int i);
extern int IsGVfs(NspObject *O);
extern NspGVfs *GetGVfsCopy (Stack stack, int i);
extern NspGVfs *GetGVfs (Stack stack, int i);

#endif /* NSP_INC_NspGVfs */ 

#ifdef NspGVfs_Private 
static int init_gvfs(NspGVfs *o,NspTypeGVfs *type);
static char *nsp_gvfs_type_as_string(void);
static char *nsp_gvfs_type_short_string(NspObject *v);
static AttrTab gvfs_attrs[];
static NspMethods *gvfs_get_methods(void);
/* static int int_gvfs_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGVfs_Private */
