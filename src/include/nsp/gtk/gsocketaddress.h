/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSocketAddress
#define NSP_INC_NspGSocketAddress

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

/* NspGSocketAddress */

#include <nsp/gtk/gobject.h>

/*
 * NspGSocketAddress inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSocketAddress ;
typedef NspTypeGObject NspTypeGSocketAddress ;

extern int nsp_type_gsocketaddress_id;
extern NspTypeGSocketAddress *nsp_type_gsocketaddress;

/* type instances for gobject */

NspTypeGSocketAddress *new_type_gsocketaddress(type_mode mode);

/* instance for NspGSocketAddress */

NspGSocketAddress *new_gsocketaddress();

/*
 * Object methods redefined for gsocketaddress 
 */

#define NULLGSOCKETADDRESS (NspGSocketAddress*) 0


/* from NspGSocketAddressObj.c */

extern NspGSocketAddress *nsp_gsocketaddress_object (NspObject *O);
extern int IsGSocketAddressObj (Stack stack, int i);
extern int IsGSocketAddress(NspObject *O);
extern NspGSocketAddress *GetGSocketAddressCopy (Stack stack, int i);
extern NspGSocketAddress *GetGSocketAddress (Stack stack, int i);

#endif /* NSP_INC_NspGSocketAddress */ 

#ifdef NspGSocketAddress_Private 
static int init_gsocketaddress(NspGSocketAddress *o,NspTypeGSocketAddress *type);
static char *nsp_gsocketaddress_type_as_string(void);
static char *nsp_gsocketaddress_type_short_string(NspObject *v);
static AttrTab gsocketaddress_attrs[];
static NspMethods *gsocketaddress_get_methods(void);
/* static int int_gsocketaddress_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSocketAddress_Private */
