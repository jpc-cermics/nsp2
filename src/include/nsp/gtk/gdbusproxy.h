/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDBusProxy
#define NSP_INC_NspGDBusProxy

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

/* NspGDBusProxy */

#include <nsp/gtk/gobject.h>

/*
 * NspGDBusProxy inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGDBusProxy ;
typedef NspTypeGObject NspTypeGDBusProxy ;

extern int nsp_type_gdbusproxy_id;
extern NspTypeGDBusProxy *nsp_type_gdbusproxy;

/* type instances for gobject */

NspTypeGDBusProxy *new_type_gdbusproxy(type_mode mode);

/* instance for NspGDBusProxy */

NspGDBusProxy *new_gdbusproxy();

/*
 * Object methods redefined for gdbusproxy 
 */

#define NULLGDBUSPROXY (NspGDBusProxy*) 0


/* from NspGDBusProxyObj.c */

extern NspGDBusProxy *nsp_gdbusproxy_object (NspObject *O);
extern int IsGDBusProxyObj (Stack stack, int i);
extern int IsGDBusProxy(NspObject *O);
extern NspGDBusProxy *GetGDBusProxyCopy (Stack stack, int i);
extern NspGDBusProxy *GetGDBusProxy (Stack stack, int i);

#endif /* NSP_INC_NspGDBusProxy */ 

#ifdef NspGDBusProxy_Private 
static int init_gdbusproxy(NspGDBusProxy *o,NspTypeGDBusProxy *type);
static char *nsp_gdbusproxy_type_as_string(void);
static char *nsp_gdbusproxy_type_short_string(NspObject *v);
static AttrTab gdbusproxy_attrs[];
static NspMethods *gdbusproxy_get_methods(void);
/* static int int_gdbusproxy_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDBusProxy_Private */
