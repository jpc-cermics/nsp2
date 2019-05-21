/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeDragSource
#define NSP_INC_NspGtkTreeDragSource

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

/* NspGtkTreeDragSource */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTreeDragSource inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTreeDragSource ;
typedef NspTypeGObject NspTypeGtkTreeDragSource ;

extern int nsp_type_gtktreedragsource_id;
extern NspTypeGtkTreeDragSource *nsp_type_gtktreedragsource;

/* type instances for gobject */

NspTypeGtkTreeDragSource *new_type_gtktreedragsource(type_mode mode);

/* instance for NspGtkTreeDragSource */

NspGtkTreeDragSource *new_gtktreedragsource();

/*
 * Object methods redefined for gtktreedragsource 
 */

#define NULLGTKTREEDRAGSOURCE (NspGtkTreeDragSource*) 0


/* from NspGtkTreeDragSourceObj.c */

extern NspGtkTreeDragSource *nsp_gtktreedragsource_object (NspObject *O);
extern int IsGtkTreeDragSourceObj (Stack stack, int i);
extern int IsGtkTreeDragSource(NspObject *O);
extern NspGtkTreeDragSource *GetGtkTreeDragSourceCopy (Stack stack, int i);
extern NspGtkTreeDragSource *GetGtkTreeDragSource (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeDragSource */ 

#ifdef NspGtkTreeDragSource_Private 
static int init_gtktreedragsource(NspGtkTreeDragSource *o,NspTypeGtkTreeDragSource *type);
static char *nsp_gtktreedragsource_type_as_string(void);
static char *nsp_gtktreedragsource_type_short_string(NspObject *v);
static AttrTab gtktreedragsource_attrs[];
static NspMethods *gtktreedragsource_get_methods(void);
/* static int int_gtktreedragsource_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeDragSource_Private */
