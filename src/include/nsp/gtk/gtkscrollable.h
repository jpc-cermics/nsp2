/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkScrollable
#define NSP_INC_NspGtkScrollable

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

/* NspGtkScrollable */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkScrollable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkScrollable ;
typedef NspTypeGObject NspTypeGtkScrollable ;

extern int nsp_type_gtkscrollable_id;
extern NspTypeGtkScrollable *nsp_type_gtkscrollable;

/* type instances for gobject */

NspTypeGtkScrollable *new_type_gtkscrollable(type_mode mode);

/* instance for NspGtkScrollable */

NspGtkScrollable *new_gtkscrollable();

/*
 * Object methods redefined for gtkscrollable 
 */

#define NULLGTKSCROLLABLE (NspGtkScrollable*) 0


/* from NspGtkScrollableObj.c */

extern NspGtkScrollable *nsp_gtkscrollable_object (NspObject *O);
extern int IsGtkScrollableObj (Stack stack, int i);
extern int IsGtkScrollable(NspObject *O);
extern NspGtkScrollable *GetGtkScrollableCopy (Stack stack, int i);
extern NspGtkScrollable *GetGtkScrollable (Stack stack, int i);

#endif /* NSP_INC_NspGtkScrollable */ 

#ifdef NspGtkScrollable_Private 
static int init_gtkscrollable(NspGtkScrollable *o,NspTypeGtkScrollable *type);
static char *nsp_gtkscrollable_type_as_string(void);
static char *nsp_gtkscrollable_type_short_string(NspObject *v);
static AttrTab gtkscrollable_attrs[];
static NspMethods *gtkscrollable_get_methods(void);
/* static int int_gtkscrollable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkScrollable_Private */
