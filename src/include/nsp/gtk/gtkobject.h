/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkObject
#define NSP_INC_NspGtkObject

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

/* NspGtkObject */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkObject inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkObject ;
typedef NspTypeGObject NspTypeGtkObject ;

extern int nsp_type_gtkobject_id;
extern NspTypeGtkObject *nsp_type_gtkobject;

/* type instances for gobject */

NspTypeGtkObject *new_type_gtkobject(type_mode mode);

/* instance for NspGtkObject */

NspGtkObject *new_gtkobject();

/*
 * Object methods redefined for gtkobject 
 */

#define NULLGTKOBJECT (NspGtkObject*) 0


/* from NspGtkObjectObj.c */

extern NspGtkObject *nsp_gtkobject_object (NspObject *O);
extern int IsGtkObjectObj (Stack stack, int i);
extern int IsGtkObject(NspObject *O);
extern NspGtkObject *GetGtkObjectCopy (Stack stack, int i);
extern NspGtkObject *GetGtkObject (Stack stack, int i);

#endif /* NSP_INC_NspGtkObject */ 

#ifdef NspGtkObject_Private 
static int init_gtkobject(NspGtkObject *o,NspTypeGtkObject *type);
static char *nsp_gtkobject_type_as_string(void);
static char *nsp_gtkobject_type_short_string(NspObject *v);
static AttrTab gtkobject_attrs[];
static NspMethods *gtkobject_get_methods(void);
/* static int int_gtkobject_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkObject_Private */
