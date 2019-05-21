/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkActivatable
#define NSP_INC_NspGtkActivatable

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

/* NspGtkActivatable */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkActivatable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkActivatable ;
typedef NspTypeGObject NspTypeGtkActivatable ;

extern int nsp_type_gtkactivatable_id;
extern NspTypeGtkActivatable *nsp_type_gtkactivatable;

/* type instances for gobject */

NspTypeGtkActivatable *new_type_gtkactivatable(type_mode mode);

/* instance for NspGtkActivatable */

NspGtkActivatable *new_gtkactivatable();

/*
 * Object methods redefined for gtkactivatable 
 */

#define NULLGTKACTIVATABLE (NspGtkActivatable*) 0


/* from NspGtkActivatableObj.c */

extern NspGtkActivatable *nsp_gtkactivatable_object (NspObject *O);
extern int IsGtkActivatableObj (Stack stack, int i);
extern int IsGtkActivatable(NspObject *O);
extern NspGtkActivatable *GetGtkActivatableCopy (Stack stack, int i);
extern NspGtkActivatable *GetGtkActivatable (Stack stack, int i);

#endif /* NSP_INC_NspGtkActivatable */ 

#ifdef NspGtkActivatable_Private 
static int init_gtkactivatable(NspGtkActivatable *o,NspTypeGtkActivatable *type);
static char *nsp_gtkactivatable_type_as_string(void);
static char *nsp_gtkactivatable_type_short_string(NspObject *v);
static AttrTab gtkactivatable_attrs[];
static NspMethods *gtkactivatable_get_methods(void);
/* static int int_gtkactivatable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkActivatable_Private */
