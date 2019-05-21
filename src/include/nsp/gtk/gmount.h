/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGMount
#define NSP_INC_NspGMount

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

/* NspGMount */

#include <nsp/gtk/gobject.h>

/*
 * NspGMount inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGMount ;
typedef NspTypeGObject NspTypeGMount ;

extern int nsp_type_gmount_id;
extern NspTypeGMount *nsp_type_gmount;

/* type instances for gobject */

NspTypeGMount *new_type_gmount(type_mode mode);

/* instance for NspGMount */

NspGMount *new_gmount();

/*
 * Object methods redefined for gmount 
 */

#define NULLGMOUNT (NspGMount*) 0


/* from NspGMountObj.c */

extern NspGMount *nsp_gmount_object (NspObject *O);
extern int IsGMountObj (Stack stack, int i);
extern int IsGMount(NspObject *O);
extern NspGMount *GetGMountCopy (Stack stack, int i);
extern NspGMount *GetGMount (Stack stack, int i);

#endif /* NSP_INC_NspGMount */ 

#ifdef NspGMount_Private 
static int init_gmount(NspGMount *o,NspTypeGMount *type);
static char *nsp_gmount_type_as_string(void);
static char *nsp_gmount_type_short_string(NspObject *v);
static AttrTab gmount_attrs[];
static NspMethods *gmount_get_methods(void);
/* static int int_gmount_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGMount_Private */
