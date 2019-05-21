/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkDisplayManager
#define NSP_INC_NspGdkDisplayManager

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

/* NspGdkDisplayManager */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkDisplayManager inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkDisplayManager ;
typedef NspTypeGObject NspTypeGdkDisplayManager ;

extern int nsp_type_gdkdisplaymanager_id;
extern NspTypeGdkDisplayManager *nsp_type_gdkdisplaymanager;

/* type instances for gobject */

NspTypeGdkDisplayManager *new_type_gdkdisplaymanager(type_mode mode);

/* instance for NspGdkDisplayManager */

NspGdkDisplayManager *new_gdkdisplaymanager();

/*
 * Object methods redefined for gdkdisplaymanager 
 */

#define NULLGDKDISPLAYMANAGER (NspGdkDisplayManager*) 0


/* from NspGdkDisplayManagerObj.c */

extern NspGdkDisplayManager *nsp_gdkdisplaymanager_object (NspObject *O);
extern int IsGdkDisplayManagerObj (Stack stack, int i);
extern int IsGdkDisplayManager(NspObject *O);
extern NspGdkDisplayManager *GetGdkDisplayManagerCopy (Stack stack, int i);
extern NspGdkDisplayManager *GetGdkDisplayManager (Stack stack, int i);

#endif /* NSP_INC_NspGdkDisplayManager */ 

#ifdef NspGdkDisplayManager_Private 
static int init_gdkdisplaymanager(NspGdkDisplayManager *o,NspTypeGdkDisplayManager *type);
static char *nsp_gdkdisplaymanager_type_as_string(void);
static char *nsp_gdkdisplaymanager_type_short_string(NspObject *v);
static AttrTab gdkdisplaymanager_attrs[];
static NspMethods *gdkdisplaymanager_get_methods(void);
/* static int int_gdkdisplaymanager_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkDisplayManager_Private */
