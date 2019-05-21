/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkObjectFactory
#define NSP_INC_NspAtkObjectFactory

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

/* NspAtkObjectFactory */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkObjectFactory inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkObjectFactory ;
typedef NspTypeGObject NspTypeAtkObjectFactory ;

extern int nsp_type_atkobjectfactory_id;
extern NspTypeAtkObjectFactory *nsp_type_atkobjectfactory;

/* type instances for gobject */

NspTypeAtkObjectFactory *new_type_atkobjectfactory(type_mode mode);

/* instance for NspAtkObjectFactory */

NspAtkObjectFactory *new_atkobjectfactory();

/*
 * Object methods redefined for atkobjectfactory 
 */

#define NULLATKOBJECTFACTORY (NspAtkObjectFactory*) 0


/* from NspAtkObjectFactoryObj.c */

extern NspAtkObjectFactory *nsp_atkobjectfactory_object (NspObject *O);
extern int IsAtkObjectFactoryObj (Stack stack, int i);
extern int IsAtkObjectFactory(NspObject *O);
extern NspAtkObjectFactory *GetAtkObjectFactoryCopy (Stack stack, int i);
extern NspAtkObjectFactory *GetAtkObjectFactory (Stack stack, int i);

#endif /* NSP_INC_NspAtkObjectFactory */ 

#ifdef NspAtkObjectFactory_Private 
static int init_atkobjectfactory(NspAtkObjectFactory *o,NspTypeAtkObjectFactory *type);
static char *nsp_atkobjectfactory_type_as_string(void);
static char *nsp_atkobjectfactory_type_short_string(NspObject *v);
static AttrTab atkobjectfactory_attrs[];
static NspMethods *atkobjectfactory_get_methods(void);
/* static int int_atkobjectfactory_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkObjectFactory_Private */
