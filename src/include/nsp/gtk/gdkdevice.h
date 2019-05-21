/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkDevice
#define NSP_INC_NspGdkDevice

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

/* NspGdkDevice */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkDevice inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkDevice ;
typedef NspTypeGObject NspTypeGdkDevice ;

extern int nsp_type_gdkdevice_id;
extern NspTypeGdkDevice *nsp_type_gdkdevice;

/* type instances for gobject */

NspTypeGdkDevice *new_type_gdkdevice(type_mode mode);

/* instance for NspGdkDevice */

NspGdkDevice *new_gdkdevice();

/*
 * Object methods redefined for gdkdevice 
 */

#define NULLGDKDEVICE (NspGdkDevice*) 0


/* from NspGdkDeviceObj.c */

extern NspGdkDevice *nsp_gdkdevice_object (NspObject *O);
extern int IsGdkDeviceObj (Stack stack, int i);
extern int IsGdkDevice(NspObject *O);
extern NspGdkDevice *GetGdkDeviceCopy (Stack stack, int i);
extern NspGdkDevice *GetGdkDevice (Stack stack, int i);

#endif /* NSP_INC_NspGdkDevice */ 

#ifdef NspGdkDevice_Private 
static int init_gdkdevice(NspGdkDevice *o,NspTypeGdkDevice *type);
static char *nsp_gdkdevice_type_as_string(void);
static char *nsp_gdkdevice_type_short_string(NspObject *v);
static AttrTab gdkdevice_attrs[];
static NspMethods *gdkdevice_get_methods(void);
/* static int int_gdkdevice_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkDevice_Private */
