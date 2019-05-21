/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkRegistry
#define NSP_INC_NspAtkRegistry

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

/* NspAtkRegistry */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkRegistry inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkRegistry ;
typedef NspTypeGObject NspTypeAtkRegistry ;

extern int nsp_type_atkregistry_id;
extern NspTypeAtkRegistry *nsp_type_atkregistry;

/* type instances for gobject */

NspTypeAtkRegistry *new_type_atkregistry(type_mode mode);

/* instance for NspAtkRegistry */

NspAtkRegistry *new_atkregistry();

/*
 * Object methods redefined for atkregistry 
 */

#define NULLATKREGISTRY (NspAtkRegistry*) 0


/* from NspAtkRegistryObj.c */

extern NspAtkRegistry *nsp_atkregistry_object (NspObject *O);
extern int IsAtkRegistryObj (Stack stack, int i);
extern int IsAtkRegistry(NspObject *O);
extern NspAtkRegistry *GetAtkRegistryCopy (Stack stack, int i);
extern NspAtkRegistry *GetAtkRegistry (Stack stack, int i);

#endif /* NSP_INC_NspAtkRegistry */ 

#ifdef NspAtkRegistry_Private 
static int init_atkregistry(NspAtkRegistry *o,NspTypeAtkRegistry *type);
static char *nsp_atkregistry_type_as_string(void);
static char *nsp_atkregistry_type_short_string(NspObject *v);
static AttrTab atkregistry_attrs[];
static NspMethods *atkregistry_get_methods(void);
/* static int int_atkregistry_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkRegistry_Private */
