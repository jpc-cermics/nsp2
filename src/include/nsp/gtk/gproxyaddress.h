/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGProxyAddress
#define NSP_INC_NspGProxyAddress

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

/* NspGProxyAddress */

#include <nsp/gtk/ginetsocketaddress.h>

/*
 * NspGProxyAddress inherits from GInetSocketAddress
 * just change some type attributes 
 */

typedef NspGInetSocketAddress NspGProxyAddress ;
typedef NspTypeGInetSocketAddress NspTypeGProxyAddress ;

extern int nsp_type_gproxyaddress_id;
extern NspTypeGProxyAddress *nsp_type_gproxyaddress;

/* type instances for ginetsocketaddress */

NspTypeGProxyAddress *new_type_gproxyaddress(type_mode mode);

/* instance for NspGProxyAddress */

NspGProxyAddress *new_gproxyaddress();

/*
 * Object methods redefined for gproxyaddress 
 */

#define NULLGPROXYADDRESS (NspGProxyAddress*) 0


/* from NspGProxyAddressObj.c */

extern NspGProxyAddress *nsp_gproxyaddress_object (NspObject *O);
extern int IsGProxyAddressObj (Stack stack, int i);
extern int IsGProxyAddress(NspObject *O);
extern NspGProxyAddress *GetGProxyAddressCopy (Stack stack, int i);
extern NspGProxyAddress *GetGProxyAddress (Stack stack, int i);

#endif /* NSP_INC_NspGProxyAddress */ 

#ifdef NspGProxyAddress_Private 
static int init_gproxyaddress(NspGProxyAddress *o,NspTypeGProxyAddress *type);
static char *nsp_gproxyaddress_type_as_string(void);
static char *nsp_gproxyaddress_type_short_string(NspObject *v);
static AttrTab gproxyaddress_attrs[];
static NspMethods *gproxyaddress_get_methods(void);
/* static int int_gproxyaddress_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGProxyAddress_Private */
