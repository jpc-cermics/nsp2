/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGNetworkAddress
#define NSP_INC_NspGNetworkAddress

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

/* NspGNetworkAddress */

#include <nsp/gtk/gobject.h>

/*
 * NspGNetworkAddress inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGNetworkAddress ;
typedef NspTypeGObject NspTypeGNetworkAddress ;

extern int nsp_type_gnetworkaddress_id;
extern NspTypeGNetworkAddress *nsp_type_gnetworkaddress;

/* type instances for gobject */

NspTypeGNetworkAddress *new_type_gnetworkaddress(type_mode mode);

/* instance for NspGNetworkAddress */

NspGNetworkAddress *new_gnetworkaddress();

/*
 * Object methods redefined for gnetworkaddress 
 */

#define NULLGNETWORKADDRESS (NspGNetworkAddress*) 0


/* from NspGNetworkAddressObj.c */

extern NspGNetworkAddress *nsp_gnetworkaddress_object (NspObject *O);
extern int IsGNetworkAddressObj (Stack stack, int i);
extern int IsGNetworkAddress(NspObject *O);
extern NspGNetworkAddress *GetGNetworkAddressCopy (Stack stack, int i);
extern NspGNetworkAddress *GetGNetworkAddress (Stack stack, int i);

#endif /* NSP_INC_NspGNetworkAddress */ 

#ifdef NspGNetworkAddress_Private 
static int init_gnetworkaddress(NspGNetworkAddress *o,NspTypeGNetworkAddress *type);
static char *nsp_gnetworkaddress_type_as_string(void);
static char *nsp_gnetworkaddress_type_short_string(NspObject *v);
static AttrTab gnetworkaddress_attrs[];
static NspMethods *gnetworkaddress_get_methods(void);
/* static int int_gnetworkaddress_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGNetworkAddress_Private */
