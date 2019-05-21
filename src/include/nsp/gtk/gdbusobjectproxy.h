/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDBusObjectProxy
#define NSP_INC_NspGDBusObjectProxy

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

/* NspGDBusObjectProxy */

#include <nsp/gtk/gobject.h>

/*
 * NspGDBusObjectProxy inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGDBusObjectProxy ;
typedef NspTypeGObject NspTypeGDBusObjectProxy ;

extern int nsp_type_gdbusobjectproxy_id;
extern NspTypeGDBusObjectProxy *nsp_type_gdbusobjectproxy;

/* type instances for gobject */

NspTypeGDBusObjectProxy *new_type_gdbusobjectproxy(type_mode mode);

/* instance for NspGDBusObjectProxy */

NspGDBusObjectProxy *new_gdbusobjectproxy();

/*
 * Object methods redefined for gdbusobjectproxy 
 */

#define NULLGDBUSOBJECTPROXY (NspGDBusObjectProxy*) 0


/* from NspGDBusObjectProxyObj.c */

extern NspGDBusObjectProxy *nsp_gdbusobjectproxy_object (NspObject *O);
extern int IsGDBusObjectProxyObj (Stack stack, int i);
extern int IsGDBusObjectProxy(NspObject *O);
extern NspGDBusObjectProxy *GetGDBusObjectProxyCopy (Stack stack, int i);
extern NspGDBusObjectProxy *GetGDBusObjectProxy (Stack stack, int i);

#endif /* NSP_INC_NspGDBusObjectProxy */ 

#ifdef NspGDBusObjectProxy_Private 
static int init_gdbusobjectproxy(NspGDBusObjectProxy *o,NspTypeGDBusObjectProxy *type);
static char *nsp_gdbusobjectproxy_type_as_string(void);
static char *nsp_gdbusobjectproxy_type_short_string(NspObject *v);
static AttrTab gdbusobjectproxy_attrs[];
static NspMethods *gdbusobjectproxy_get_methods(void);
/* static int int_gdbusobjectproxy_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDBusObjectProxy_Private */
