/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkItemFactory
#define NSP_INC_NspGtkItemFactory

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

/* NspGtkItemFactory */

#include <nsp/gtk/gtkobject.h>

/*
 * NspGtkItemFactory inherits from GtkObject
 * just change some type attributes 
 */

typedef NspGtkObject NspGtkItemFactory ;
typedef NspTypeGtkObject NspTypeGtkItemFactory ;

extern int nsp_type_gtkitemfactory_id;
extern NspTypeGtkItemFactory *nsp_type_gtkitemfactory;

/* type instances for gtkobject */

NspTypeGtkItemFactory *new_type_gtkitemfactory(type_mode mode);

/* instance for NspGtkItemFactory */

NspGtkItemFactory *new_gtkitemfactory();

/*
 * Object methods redefined for gtkitemfactory 
 */

#define NULLGTKITEMFACTORY (NspGtkItemFactory*) 0


/* from NspGtkItemFactoryObj.c */

extern NspGtkItemFactory *nsp_gtkitemfactory_object (NspObject *O);
extern int IsGtkItemFactoryObj (Stack stack, int i);
extern int IsGtkItemFactory(NspObject *O);
extern NspGtkItemFactory *GetGtkItemFactoryCopy (Stack stack, int i);
extern NspGtkItemFactory *GetGtkItemFactory (Stack stack, int i);

#endif /* NSP_INC_NspGtkItemFactory */ 

#ifdef NspGtkItemFactory_Private 
static int init_gtkitemfactory(NspGtkItemFactory *o,NspTypeGtkItemFactory *type);
static char *nsp_gtkitemfactory_type_as_string(void);
static char *nsp_gtkitemfactory_type_short_string(NspObject *v);
static AttrTab gtkitemfactory_attrs[];
static NspMethods *gtkitemfactory_get_methods(void);
/* static int int_gtkitemfactory_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkItemFactory_Private */
