/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGActionMap
#define NSP_INC_NspGActionMap

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

/* NspGActionMap */

#include <nsp/gtk/gobject.h>

/*
 * NspGActionMap inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGActionMap ;
typedef NspTypeGObject NspTypeGActionMap ;

extern int nsp_type_gactionmap_id;
extern NspTypeGActionMap *nsp_type_gactionmap;

/* type instances for gobject */

NspTypeGActionMap *new_type_gactionmap(type_mode mode);

/* instance for NspGActionMap */

NspGActionMap *new_gactionmap();

/*
 * Object methods redefined for gactionmap 
 */

#define NULLGACTIONMAP (NspGActionMap*) 0


/* from NspGActionMapObj.c */

extern NspGActionMap *nsp_gactionmap_object (NspObject *O);
extern int IsGActionMapObj (Stack stack, int i);
extern int IsGActionMap(NspObject *O);
extern NspGActionMap *GetGActionMapCopy (Stack stack, int i);
extern NspGActionMap *GetGActionMap (Stack stack, int i);

#endif /* NSP_INC_NspGActionMap */ 

#ifdef NspGActionMap_Private 
static int init_gactionmap(NspGActionMap *o,NspTypeGActionMap *type);
static char *nsp_gactionmap_type_as_string(void);
static char *nsp_gactionmap_type_short_string(NspObject *v);
static AttrTab gactionmap_attrs[];
static NspMethods *gactionmap_get_methods(void);
/* static int int_gactionmap_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGActionMap_Private */
