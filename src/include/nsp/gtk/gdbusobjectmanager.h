/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDBusObjectManager
#define NSP_INC_NspGDBusObjectManager

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

/* NspGDBusObjectManager */

#include <nsp/gtk/gobject.h>

/*
 * NspGDBusObjectManager inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGDBusObjectManager ;
typedef NspTypeGObject NspTypeGDBusObjectManager ;

extern int nsp_type_gdbusobjectmanager_id;
extern NspTypeGDBusObjectManager *nsp_type_gdbusobjectmanager;

/* type instances for gobject */

NspTypeGDBusObjectManager *new_type_gdbusobjectmanager(type_mode mode);

/* instance for NspGDBusObjectManager */

NspGDBusObjectManager *new_gdbusobjectmanager();

/*
 * Object methods redefined for gdbusobjectmanager 
 */

#define NULLGDBUSOBJECTMANAGER (NspGDBusObjectManager*) 0


/* from NspGDBusObjectManagerObj.c */

extern NspGDBusObjectManager *nsp_gdbusobjectmanager_object (NspObject *O);
extern int IsGDBusObjectManagerObj (Stack stack, int i);
extern int IsGDBusObjectManager(NspObject *O);
extern NspGDBusObjectManager *GetGDBusObjectManagerCopy (Stack stack, int i);
extern NspGDBusObjectManager *GetGDBusObjectManager (Stack stack, int i);

#endif /* NSP_INC_NspGDBusObjectManager */ 

#ifdef NspGDBusObjectManager_Private 
static int init_gdbusobjectmanager(NspGDBusObjectManager *o,NspTypeGDBusObjectManager *type);
static char *nsp_gdbusobjectmanager_type_as_string(void);
static char *nsp_gdbusobjectmanager_type_short_string(NspObject *v);
static AttrTab gdbusobjectmanager_attrs[];
static NspMethods *gdbusobjectmanager_get_methods(void);
/* static int int_gdbusobjectmanager_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDBusObjectManager_Private */
