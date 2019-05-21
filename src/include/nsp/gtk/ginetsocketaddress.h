/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGInetSocketAddress
#define NSP_INC_NspGInetSocketAddress

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

/* NspGInetSocketAddress */

#include <nsp/gtk/gsocketaddress.h>

/*
 * NspGInetSocketAddress inherits from GSocketAddress
 * just change some type attributes 
 */

typedef NspGSocketAddress NspGInetSocketAddress ;
typedef NspTypeGSocketAddress NspTypeGInetSocketAddress ;

extern int nsp_type_ginetsocketaddress_id;
extern NspTypeGInetSocketAddress *nsp_type_ginetsocketaddress;

/* type instances for gsocketaddress */

NspTypeGInetSocketAddress *new_type_ginetsocketaddress(type_mode mode);

/* instance for NspGInetSocketAddress */

NspGInetSocketAddress *new_ginetsocketaddress();

/*
 * Object methods redefined for ginetsocketaddress 
 */

#define NULLGINETSOCKETADDRESS (NspGInetSocketAddress*) 0


/* from NspGInetSocketAddressObj.c */

extern NspGInetSocketAddress *nsp_ginetsocketaddress_object (NspObject *O);
extern int IsGInetSocketAddressObj (Stack stack, int i);
extern int IsGInetSocketAddress(NspObject *O);
extern NspGInetSocketAddress *GetGInetSocketAddressCopy (Stack stack, int i);
extern NspGInetSocketAddress *GetGInetSocketAddress (Stack stack, int i);

#endif /* NSP_INC_NspGInetSocketAddress */ 

#ifdef NspGInetSocketAddress_Private 
static int init_ginetsocketaddress(NspGInetSocketAddress *o,NspTypeGInetSocketAddress *type);
static char *nsp_ginetsocketaddress_type_as_string(void);
static char *nsp_ginetsocketaddress_type_short_string(NspObject *v);
static AttrTab ginetsocketaddress_attrs[];
static NspMethods *ginetsocketaddress_get_methods(void);
/* static int int_ginetsocketaddress_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGInetSocketAddress_Private */
