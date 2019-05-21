/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDBusObjectSkeleton
#define NSP_INC_NspGDBusObjectSkeleton

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

/* NspGDBusObjectSkeleton */

#include <nsp/gtk/gobject.h>

/*
 * NspGDBusObjectSkeleton inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGDBusObjectSkeleton ;
typedef NspTypeGObject NspTypeGDBusObjectSkeleton ;

extern int nsp_type_gdbusobjectskeleton_id;
extern NspTypeGDBusObjectSkeleton *nsp_type_gdbusobjectskeleton;

/* type instances for gobject */

NspTypeGDBusObjectSkeleton *new_type_gdbusobjectskeleton(type_mode mode);

/* instance for NspGDBusObjectSkeleton */

NspGDBusObjectSkeleton *new_gdbusobjectskeleton();

/*
 * Object methods redefined for gdbusobjectskeleton 
 */

#define NULLGDBUSOBJECTSKELETON (NspGDBusObjectSkeleton*) 0


/* from NspGDBusObjectSkeletonObj.c */

extern NspGDBusObjectSkeleton *nsp_gdbusobjectskeleton_object (NspObject *O);
extern int IsGDBusObjectSkeletonObj (Stack stack, int i);
extern int IsGDBusObjectSkeleton(NspObject *O);
extern NspGDBusObjectSkeleton *GetGDBusObjectSkeletonCopy (Stack stack, int i);
extern NspGDBusObjectSkeleton *GetGDBusObjectSkeleton (Stack stack, int i);

#endif /* NSP_INC_NspGDBusObjectSkeleton */ 

#ifdef NspGDBusObjectSkeleton_Private 
static int init_gdbusobjectskeleton(NspGDBusObjectSkeleton *o,NspTypeGDBusObjectSkeleton *type);
static char *nsp_gdbusobjectskeleton_type_as_string(void);
static char *nsp_gdbusobjectskeleton_type_short_string(NspObject *v);
static AttrTab gdbusobjectskeleton_attrs[];
static NspMethods *gdbusobjectskeleton_get_methods(void);
/* static int int_gdbusobjectskeleton_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDBusObjectSkeleton_Private */
