/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkDeviceManager
#define NSP_INC_NspGdkDeviceManager

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

/* NspGdkDeviceManager */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkDeviceManager inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkDeviceManager ;
typedef NspTypeGObject NspTypeGdkDeviceManager ;

extern int nsp_type_gdkdevicemanager_id;
extern NspTypeGdkDeviceManager *nsp_type_gdkdevicemanager;

/* type instances for gobject */

NspTypeGdkDeviceManager *new_type_gdkdevicemanager(type_mode mode);

/* instance for NspGdkDeviceManager */

NspGdkDeviceManager *new_gdkdevicemanager();

/*
 * Object methods redefined for gdkdevicemanager 
 */

#define NULLGDKDEVICEMANAGER (NspGdkDeviceManager*) 0


/* from NspGdkDeviceManagerObj.c */

extern NspGdkDeviceManager *nsp_gdkdevicemanager_object (NspObject *O);
extern int IsGdkDeviceManagerObj (Stack stack, int i);
extern int IsGdkDeviceManager(NspObject *O);
extern NspGdkDeviceManager *GetGdkDeviceManagerCopy (Stack stack, int i);
extern NspGdkDeviceManager *GetGdkDeviceManager (Stack stack, int i);

#endif /* NSP_INC_NspGdkDeviceManager */ 

#ifdef NspGdkDeviceManager_Private 
static int init_gdkdevicemanager(NspGdkDeviceManager *o,NspTypeGdkDeviceManager *type);
static char *nsp_gdkdevicemanager_type_as_string(void);
static char *nsp_gdkdevicemanager_type_short_string(NspObject *v);
static AttrTab gdkdevicemanager_attrs[];
static NspMethods *gdkdevicemanager_get_methods(void);
/* static int int_gdkdevicemanager_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkDeviceManager_Private */
