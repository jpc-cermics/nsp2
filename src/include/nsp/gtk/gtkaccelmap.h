/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAccelMap
#define NSP_INC_NspGtkAccelMap

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

/* NspGtkAccelMap */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkAccelMap inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkAccelMap ;
typedef NspTypeGObject NspTypeGtkAccelMap ;

extern int nsp_type_gtkaccelmap_id;
extern NspTypeGtkAccelMap *nsp_type_gtkaccelmap;

/* type instances for gobject */

NspTypeGtkAccelMap *new_type_gtkaccelmap(type_mode mode);

/* instance for NspGtkAccelMap */

NspGtkAccelMap *new_gtkaccelmap();

/*
 * Object methods redefined for gtkaccelmap 
 */

#define NULLGTKACCELMAP (NspGtkAccelMap*) 0


/* from NspGtkAccelMapObj.c */

extern NspGtkAccelMap *nsp_gtkaccelmap_object (NspObject *O);
extern int IsGtkAccelMapObj (Stack stack, int i);
extern int IsGtkAccelMap(NspObject *O);
extern NspGtkAccelMap *GetGtkAccelMapCopy (Stack stack, int i);
extern NspGtkAccelMap *GetGtkAccelMap (Stack stack, int i);

#endif /* NSP_INC_NspGtkAccelMap */ 

#ifdef NspGtkAccelMap_Private 
static int init_gtkaccelmap(NspGtkAccelMap *o,NspTypeGtkAccelMap *type);
static char *nsp_gtkaccelmap_type_as_string(void);
static char *nsp_gtkaccelmap_type_short_string(NspObject *v);
static AttrTab gtkaccelmap_attrs[];
static NspMethods *gtkaccelmap_get_methods(void);
/* static int int_gtkaccelmap_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAccelMap_Private */
