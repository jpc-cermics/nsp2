/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDBusInterface
#define NSP_INC_NspGDBusInterface

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

/* NspGDBusInterface */

#include <nsp/gtk/gobject.h>

/*
 * NspGDBusInterface inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGDBusInterface ;
typedef NspTypeGObject NspTypeGDBusInterface ;

extern int nsp_type_gdbusinterface_id;
extern NspTypeGDBusInterface *nsp_type_gdbusinterface;

/* type instances for gobject */

NspTypeGDBusInterface *new_type_gdbusinterface(type_mode mode);

/* instance for NspGDBusInterface */

NspGDBusInterface *new_gdbusinterface();

/*
 * Object methods redefined for gdbusinterface 
 */

#define NULLGDBUSINTERFACE (NspGDBusInterface*) 0


/* from NspGDBusInterfaceObj.c */

extern NspGDBusInterface *nsp_gdbusinterface_object (NspObject *O);
extern int IsGDBusInterfaceObj (Stack stack, int i);
extern int IsGDBusInterface(NspObject *O);
extern NspGDBusInterface *GetGDBusInterfaceCopy (Stack stack, int i);
extern NspGDBusInterface *GetGDBusInterface (Stack stack, int i);

#endif /* NSP_INC_NspGDBusInterface */ 

#ifdef NspGDBusInterface_Private 
static int init_gdbusinterface(NspGDBusInterface *o,NspTypeGDBusInterface *type);
static char *nsp_gdbusinterface_type_as_string(void);
static char *nsp_gdbusinterface_type_short_string(NspObject *v);
static AttrTab gdbusinterface_attrs[];
static NspMethods *gdbusinterface_get_methods(void);
/* static int int_gdbusinterface_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDBusInterface_Private */
