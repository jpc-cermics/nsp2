/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSimpleProxyResolver
#define NSP_INC_NspGSimpleProxyResolver

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

/* NspGSimpleProxyResolver */

#include <nsp/gtk/gobject.h>

/*
 * NspGSimpleProxyResolver inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSimpleProxyResolver ;
typedef NspTypeGObject NspTypeGSimpleProxyResolver ;

extern int nsp_type_gsimpleproxyresolver_id;
extern NspTypeGSimpleProxyResolver *nsp_type_gsimpleproxyresolver;

/* type instances for gobject */

NspTypeGSimpleProxyResolver *new_type_gsimpleproxyresolver(type_mode mode);

/* instance for NspGSimpleProxyResolver */

NspGSimpleProxyResolver *new_gsimpleproxyresolver();

/*
 * Object methods redefined for gsimpleproxyresolver 
 */

#define NULLGSIMPLEPROXYRESOLVER (NspGSimpleProxyResolver*) 0


/* from NspGSimpleProxyResolverObj.c */

extern NspGSimpleProxyResolver *nsp_gsimpleproxyresolver_object (NspObject *O);
extern int IsGSimpleProxyResolverObj (Stack stack, int i);
extern int IsGSimpleProxyResolver(NspObject *O);
extern NspGSimpleProxyResolver *GetGSimpleProxyResolverCopy (Stack stack, int i);
extern NspGSimpleProxyResolver *GetGSimpleProxyResolver (Stack stack, int i);

#endif /* NSP_INC_NspGSimpleProxyResolver */ 

#ifdef NspGSimpleProxyResolver_Private 
static int init_gsimpleproxyresolver(NspGSimpleProxyResolver *o,NspTypeGSimpleProxyResolver *type);
static char *nsp_gsimpleproxyresolver_type_as_string(void);
static char *nsp_gsimpleproxyresolver_type_short_string(NspObject *v);
static AttrTab gsimpleproxyresolver_attrs[];
static NspMethods *gsimpleproxyresolver_get_methods(void);
/* static int int_gsimpleproxyresolver_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSimpleProxyResolver_Private */
