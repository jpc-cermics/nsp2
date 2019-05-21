/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkIconFactory
#define NSP_INC_NspGtkIconFactory

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

/* NspGtkIconFactory */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkIconFactory inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkIconFactory ;
typedef NspTypeGObject NspTypeGtkIconFactory ;

extern int nsp_type_gtkiconfactory_id;
extern NspTypeGtkIconFactory *nsp_type_gtkiconfactory;

/* type instances for gobject */

NspTypeGtkIconFactory *new_type_gtkiconfactory(type_mode mode);

/* instance for NspGtkIconFactory */

NspGtkIconFactory *new_gtkiconfactory();

/*
 * Object methods redefined for gtkiconfactory 
 */

#define NULLGTKICONFACTORY (NspGtkIconFactory*) 0


/* from NspGtkIconFactoryObj.c */

extern NspGtkIconFactory *nsp_gtkiconfactory_object (NspObject *O);
extern int IsGtkIconFactoryObj (Stack stack, int i);
extern int IsGtkIconFactory(NspObject *O);
extern NspGtkIconFactory *GetGtkIconFactoryCopy (Stack stack, int i);
extern NspGtkIconFactory *GetGtkIconFactory (Stack stack, int i);

#endif /* NSP_INC_NspGtkIconFactory */ 

#ifdef NspGtkIconFactory_Private 
static int init_gtkiconfactory(NspGtkIconFactory *o,NspTypeGtkIconFactory *type);
static char *nsp_gtkiconfactory_type_as_string(void);
static char *nsp_gtkiconfactory_type_short_string(NspObject *v);
static AttrTab gtkiconfactory_attrs[];
static NspMethods *gtkiconfactory_get_methods(void);
/* static int int_gtkiconfactory_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkIconFactory_Private */
