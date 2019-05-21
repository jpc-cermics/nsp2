/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDBusObject
#define NSP_INC_NspGDBusObject

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

/* NspGDBusObject */

#include <nsp/gtk/gobject.h>

/*
 * NspGDBusObject inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGDBusObject ;
typedef NspTypeGObject NspTypeGDBusObject ;

extern int nsp_type_gdbusobject_id;
extern NspTypeGDBusObject *nsp_type_gdbusobject;

/* type instances for gobject */

NspTypeGDBusObject *new_type_gdbusobject(type_mode mode);

/* instance for NspGDBusObject */

NspGDBusObject *new_gdbusobject();

/*
 * Object methods redefined for gdbusobject 
 */

#define NULLGDBUSOBJECT (NspGDBusObject*) 0


/* from NspGDBusObjectObj.c */

extern NspGDBusObject *nsp_gdbusobject_object (NspObject *O);
extern int IsGDBusObjectObj (Stack stack, int i);
extern int IsGDBusObject(NspObject *O);
extern NspGDBusObject *GetGDBusObjectCopy (Stack stack, int i);
extern NspGDBusObject *GetGDBusObject (Stack stack, int i);

#endif /* NSP_INC_NspGDBusObject */ 

#ifdef NspGDBusObject_Private 
static int init_gdbusobject(NspGDBusObject *o,NspTypeGDBusObject *type);
static char *nsp_gdbusobject_type_as_string(void);
static char *nsp_gdbusobject_type_short_string(NspObject *v);
static AttrTab gdbusobject_attrs[];
static NspMethods *gdbusobject_get_methods(void);
/* static int int_gdbusobject_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDBusObject_Private */
