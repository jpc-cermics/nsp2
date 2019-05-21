/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGInetAddress
#define NSP_INC_NspGInetAddress

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

/* NspGInetAddress */

#include <nsp/gtk/gobject.h>

/*
 * NspGInetAddress inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGInetAddress ;
typedef NspTypeGObject NspTypeGInetAddress ;

extern int nsp_type_ginetaddress_id;
extern NspTypeGInetAddress *nsp_type_ginetaddress;

/* type instances for gobject */

NspTypeGInetAddress *new_type_ginetaddress(type_mode mode);

/* instance for NspGInetAddress */

NspGInetAddress *new_ginetaddress();

/*
 * Object methods redefined for ginetaddress 
 */

#define NULLGINETADDRESS (NspGInetAddress*) 0


/* from NspGInetAddressObj.c */

extern NspGInetAddress *nsp_ginetaddress_object (NspObject *O);
extern int IsGInetAddressObj (Stack stack, int i);
extern int IsGInetAddress(NspObject *O);
extern NspGInetAddress *GetGInetAddressCopy (Stack stack, int i);
extern NspGInetAddress *GetGInetAddress (Stack stack, int i);

#endif /* NSP_INC_NspGInetAddress */ 

#ifdef NspGInetAddress_Private 
static int init_ginetaddress(NspGInetAddress *o,NspTypeGInetAddress *type);
static char *nsp_ginetaddress_type_as_string(void);
static char *nsp_ginetaddress_type_short_string(NspObject *v);
static AttrTab ginetaddress_attrs[];
static NspMethods *ginetaddress_get_methods(void);
/* static int int_ginetaddress_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGInetAddress_Private */
