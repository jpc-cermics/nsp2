/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkNoOpObjectFactory
#define NSP_INC_NspAtkNoOpObjectFactory

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

/* NspAtkNoOpObjectFactory */

#include <nsp/gtk/atkobjectfactory.h>

/*
 * NspAtkNoOpObjectFactory inherits from AtkObjectFactory
 * just change some type attributes 
 */

typedef NspAtkObjectFactory NspAtkNoOpObjectFactory ;
typedef NspTypeAtkObjectFactory NspTypeAtkNoOpObjectFactory ;

extern int nsp_type_atknoopobjectfactory_id;
extern NspTypeAtkNoOpObjectFactory *nsp_type_atknoopobjectfactory;

/* type instances for atkobjectfactory */

NspTypeAtkNoOpObjectFactory *new_type_atknoopobjectfactory(type_mode mode);

/* instance for NspAtkNoOpObjectFactory */

NspAtkNoOpObjectFactory *new_atknoopobjectfactory();

/*
 * Object methods redefined for atknoopobjectfactory 
 */

#define NULLATKNOOPOBJECTFACTORY (NspAtkNoOpObjectFactory*) 0


/* from NspAtkNoOpObjectFactoryObj.c */

extern NspAtkNoOpObjectFactory *nsp_atknoopobjectfactory_object (NspObject *O);
extern int IsAtkNoOpObjectFactoryObj (Stack stack, int i);
extern int IsAtkNoOpObjectFactory(NspObject *O);
extern NspAtkNoOpObjectFactory *GetAtkNoOpObjectFactoryCopy (Stack stack, int i);
extern NspAtkNoOpObjectFactory *GetAtkNoOpObjectFactory (Stack stack, int i);

#endif /* NSP_INC_NspAtkNoOpObjectFactory */ 

#ifdef NspAtkNoOpObjectFactory_Private 
static int init_atknoopobjectfactory(NspAtkNoOpObjectFactory *o,NspTypeAtkNoOpObjectFactory *type);
static char *nsp_atknoopobjectfactory_type_as_string(void);
static char *nsp_atknoopobjectfactory_type_short_string(NspObject *v);
static AttrTab atknoopobjectfactory_attrs[];
static NspMethods *atknoopobjectfactory_get_methods(void);
/* static int int_atknoopobjectfactory_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkNoOpObjectFactory_Private */
