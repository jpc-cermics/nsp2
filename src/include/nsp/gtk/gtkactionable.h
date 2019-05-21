/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkActionable
#define NSP_INC_NspGtkActionable

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

/* NspGtkActionable */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkActionable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkActionable ;
typedef NspTypeGObject NspTypeGtkActionable ;

extern int nsp_type_gtkactionable_id;
extern NspTypeGtkActionable *nsp_type_gtkactionable;

/* type instances for gobject */

NspTypeGtkActionable *new_type_gtkactionable(type_mode mode);

/* instance for NspGtkActionable */

NspGtkActionable *new_gtkactionable();

/*
 * Object methods redefined for gtkactionable 
 */

#define NULLGTKACTIONABLE (NspGtkActionable*) 0


/* from NspGtkActionableObj.c */

extern NspGtkActionable *nsp_gtkactionable_object (NspObject *O);
extern int IsGtkActionableObj (Stack stack, int i);
extern int IsGtkActionable(NspObject *O);
extern NspGtkActionable *GetGtkActionableCopy (Stack stack, int i);
extern NspGtkActionable *GetGtkActionable (Stack stack, int i);

#endif /* NSP_INC_NspGtkActionable */ 

#ifdef NspGtkActionable_Private 
static int init_gtkactionable(NspGtkActionable *o,NspTypeGtkActionable *type);
static char *nsp_gtkactionable_type_as_string(void);
static char *nsp_gtkactionable_type_short_string(NspObject *v);
static AttrTab gtkactionable_attrs[];
static NspMethods *gtkactionable_get_methods(void);
/* static int int_gtkactionable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkActionable_Private */
