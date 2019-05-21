/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDBusObjectManagerServer
#define NSP_INC_NspGDBusObjectManagerServer

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

/* NspGDBusObjectManagerServer */

#include <nsp/gtk/gobject.h>

/*
 * NspGDBusObjectManagerServer inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGDBusObjectManagerServer ;
typedef NspTypeGObject NspTypeGDBusObjectManagerServer ;

extern int nsp_type_gdbusobjectmanagerserver_id;
extern NspTypeGDBusObjectManagerServer *nsp_type_gdbusobjectmanagerserver;

/* type instances for gobject */

NspTypeGDBusObjectManagerServer *new_type_gdbusobjectmanagerserver(type_mode mode);

/* instance for NspGDBusObjectManagerServer */

NspGDBusObjectManagerServer *new_gdbusobjectmanagerserver();

/*
 * Object methods redefined for gdbusobjectmanagerserver 
 */

#define NULLGDBUSOBJECTMANAGERSERVER (NspGDBusObjectManagerServer*) 0


/* from NspGDBusObjectManagerServerObj.c */

extern NspGDBusObjectManagerServer *nsp_gdbusobjectmanagerserver_object (NspObject *O);
extern int IsGDBusObjectManagerServerObj (Stack stack, int i);
extern int IsGDBusObjectManagerServer(NspObject *O);
extern NspGDBusObjectManagerServer *GetGDBusObjectManagerServerCopy (Stack stack, int i);
extern NspGDBusObjectManagerServer *GetGDBusObjectManagerServer (Stack stack, int i);

#endif /* NSP_INC_NspGDBusObjectManagerServer */ 

#ifdef NspGDBusObjectManagerServer_Private 
static int init_gdbusobjectmanagerserver(NspGDBusObjectManagerServer *o,NspTypeGDBusObjectManagerServer *type);
static char *nsp_gdbusobjectmanagerserver_type_as_string(void);
static char *nsp_gdbusobjectmanagerserver_type_short_string(NspObject *v);
static AttrTab gdbusobjectmanagerserver_attrs[];
static NspMethods *gdbusobjectmanagerserver_get_methods(void);
/* static int int_gdbusobjectmanagerserver_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDBusObjectManagerServer_Private */
