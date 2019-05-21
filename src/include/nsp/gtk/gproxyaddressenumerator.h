/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGProxyAddressEnumerator
#define NSP_INC_NspGProxyAddressEnumerator

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

/* NspGProxyAddressEnumerator */

#include <nsp/gtk/gsocketaddressenumerator.h>

/*
 * NspGProxyAddressEnumerator inherits from GSocketAddressEnumerator
 * just change some type attributes 
 */

typedef NspGSocketAddressEnumerator NspGProxyAddressEnumerator ;
typedef NspTypeGSocketAddressEnumerator NspTypeGProxyAddressEnumerator ;

extern int nsp_type_gproxyaddressenumerator_id;
extern NspTypeGProxyAddressEnumerator *nsp_type_gproxyaddressenumerator;

/* type instances for gsocketaddressenumerator */

NspTypeGProxyAddressEnumerator *new_type_gproxyaddressenumerator(type_mode mode);

/* instance for NspGProxyAddressEnumerator */

NspGProxyAddressEnumerator *new_gproxyaddressenumerator();

/*
 * Object methods redefined for gproxyaddressenumerator 
 */

#define NULLGPROXYADDRESSENUMERATOR (NspGProxyAddressEnumerator*) 0


/* from NspGProxyAddressEnumeratorObj.c */

extern NspGProxyAddressEnumerator *nsp_gproxyaddressenumerator_object (NspObject *O);
extern int IsGProxyAddressEnumeratorObj (Stack stack, int i);
extern int IsGProxyAddressEnumerator(NspObject *O);
extern NspGProxyAddressEnumerator *GetGProxyAddressEnumeratorCopy (Stack stack, int i);
extern NspGProxyAddressEnumerator *GetGProxyAddressEnumerator (Stack stack, int i);

#endif /* NSP_INC_NspGProxyAddressEnumerator */ 

#ifdef NspGProxyAddressEnumerator_Private 
static int init_gproxyaddressenumerator(NspGProxyAddressEnumerator *o,NspTypeGProxyAddressEnumerator *type);
static char *nsp_gproxyaddressenumerator_type_as_string(void);
static char *nsp_gproxyaddressenumerator_type_short_string(NspObject *v);
static AttrTab gproxyaddressenumerator_attrs[];
static NspMethods *gproxyaddressenumerator_get_methods(void);
/* static int int_gproxyaddressenumerator_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGProxyAddressEnumerator_Private */
