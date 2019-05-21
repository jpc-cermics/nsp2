/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkStyle
#define NSP_INC_NspGtkStyle

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

/* NspGtkStyle */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkStyle inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkStyle ;
typedef NspTypeGObject NspTypeGtkStyle ;

extern int nsp_type_gtkstyle_id;
extern NspTypeGtkStyle *nsp_type_gtkstyle;

/* type instances for gobject */

NspTypeGtkStyle *new_type_gtkstyle(type_mode mode);

/* instance for NspGtkStyle */

NspGtkStyle *new_gtkstyle();

/*
 * Object methods redefined for gtkstyle 
 */

#define NULLGTKSTYLE (NspGtkStyle*) 0


/* from NspGtkStyleObj.c */

extern NspGtkStyle *nsp_gtkstyle_object (NspObject *O);
extern int IsGtkStyleObj (Stack stack, int i);
extern int IsGtkStyle(NspObject *O);
extern NspGtkStyle *GetGtkStyleCopy (Stack stack, int i);
extern NspGtkStyle *GetGtkStyle (Stack stack, int i);

#endif /* NSP_INC_NspGtkStyle */ 

#ifdef NspGtkStyle_Private 
static int init_gtkstyle(NspGtkStyle *o,NspTypeGtkStyle *type);
static char *nsp_gtkstyle_type_as_string(void);
static char *nsp_gtkstyle_type_short_string(NspObject *v);
static AttrTab gtkstyle_attrs[];
static NspMethods *gtkstyle_get_methods(void);
/* static int int_gtkstyle_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkStyle_Private */
