/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGDBusObjectManagerClient
#define NSP_INC_NspGDBusObjectManagerClient

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

/* NspGDBusObjectManagerClient */

#include <nsp/gtk/gobject.h>

/*
 * NspGDBusObjectManagerClient inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGDBusObjectManagerClient ;
typedef NspTypeGObject NspTypeGDBusObjectManagerClient ;

extern int nsp_type_gdbusobjectmanagerclient_id;
extern NspTypeGDBusObjectManagerClient *nsp_type_gdbusobjectmanagerclient;

/* type instances for gobject */

NspTypeGDBusObjectManagerClient *new_type_gdbusobjectmanagerclient(type_mode mode);

/* instance for NspGDBusObjectManagerClient */

NspGDBusObjectManagerClient *new_gdbusobjectmanagerclient();

/*
 * Object methods redefined for gdbusobjectmanagerclient 
 */

#define NULLGDBUSOBJECTMANAGERCLIENT (NspGDBusObjectManagerClient*) 0


/* from NspGDBusObjectManagerClientObj.c */

extern NspGDBusObjectManagerClient *nsp_gdbusobjectmanagerclient_object (NspObject *O);
extern int IsGDBusObjectManagerClientObj (Stack stack, int i);
extern int IsGDBusObjectManagerClient(NspObject *O);
extern NspGDBusObjectManagerClient *GetGDBusObjectManagerClientCopy (Stack stack, int i);
extern NspGDBusObjectManagerClient *GetGDBusObjectManagerClient (Stack stack, int i);

#endif /* NSP_INC_NspGDBusObjectManagerClient */ 

#ifdef NspGDBusObjectManagerClient_Private 
static int init_gdbusobjectmanagerclient(NspGDBusObjectManagerClient *o,NspTypeGDBusObjectManagerClient *type);
static char *nsp_gdbusobjectmanagerclient_type_as_string(void);
static char *nsp_gdbusobjectmanagerclient_type_short_string(NspObject *v);
static AttrTab gdbusobjectmanagerclient_attrs[];
static NspMethods *gdbusobjectmanagerclient_get_methods(void);
/* static int int_gdbusobjectmanagerclient_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGDBusObjectManagerClient_Private */
