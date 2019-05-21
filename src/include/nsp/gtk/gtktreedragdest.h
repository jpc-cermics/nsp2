/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeDragDest
#define NSP_INC_NspGtkTreeDragDest

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

/* NspGtkTreeDragDest */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTreeDragDest inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTreeDragDest ;
typedef NspTypeGObject NspTypeGtkTreeDragDest ;

extern int nsp_type_gtktreedragdest_id;
extern NspTypeGtkTreeDragDest *nsp_type_gtktreedragdest;

/* type instances for gobject */

NspTypeGtkTreeDragDest *new_type_gtktreedragdest(type_mode mode);

/* instance for NspGtkTreeDragDest */

NspGtkTreeDragDest *new_gtktreedragdest();

/*
 * Object methods redefined for gtktreedragdest 
 */

#define NULLGTKTREEDRAGDEST (NspGtkTreeDragDest*) 0


/* from NspGtkTreeDragDestObj.c */

extern NspGtkTreeDragDest *nsp_gtktreedragdest_object (NspObject *O);
extern int IsGtkTreeDragDestObj (Stack stack, int i);
extern int IsGtkTreeDragDest(NspObject *O);
extern NspGtkTreeDragDest *GetGtkTreeDragDestCopy (Stack stack, int i);
extern NspGtkTreeDragDest *GetGtkTreeDragDest (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeDragDest */ 

#ifdef NspGtkTreeDragDest_Private 
static int init_gtktreedragdest(NspGtkTreeDragDest *o,NspTypeGtkTreeDragDest *type);
static char *nsp_gtktreedragdest_type_as_string(void);
static char *nsp_gtktreedragdest_type_short_string(NspObject *v);
static AttrTab gtktreedragdest_attrs[];
static NspMethods *gtktreedragdest_get_methods(void);
/* static int int_gtktreedragdest_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeDragDest_Private */
