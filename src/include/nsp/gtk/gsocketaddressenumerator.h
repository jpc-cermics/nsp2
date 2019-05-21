/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSocketAddressEnumerator
#define NSP_INC_NspGSocketAddressEnumerator

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

/* NspGSocketAddressEnumerator */

#include <nsp/gtk/gobject.h>

/*
 * NspGSocketAddressEnumerator inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSocketAddressEnumerator ;
typedef NspTypeGObject NspTypeGSocketAddressEnumerator ;

extern int nsp_type_gsocketaddressenumerator_id;
extern NspTypeGSocketAddressEnumerator *nsp_type_gsocketaddressenumerator;

/* type instances for gobject */

NspTypeGSocketAddressEnumerator *new_type_gsocketaddressenumerator(type_mode mode);

/* instance for NspGSocketAddressEnumerator */

NspGSocketAddressEnumerator *new_gsocketaddressenumerator();

/*
 * Object methods redefined for gsocketaddressenumerator 
 */

#define NULLGSOCKETADDRESSENUMERATOR (NspGSocketAddressEnumerator*) 0


/* from NspGSocketAddressEnumeratorObj.c */

extern NspGSocketAddressEnumerator *nsp_gsocketaddressenumerator_object (NspObject *O);
extern int IsGSocketAddressEnumeratorObj (Stack stack, int i);
extern int IsGSocketAddressEnumerator(NspObject *O);
extern NspGSocketAddressEnumerator *GetGSocketAddressEnumeratorCopy (Stack stack, int i);
extern NspGSocketAddressEnumerator *GetGSocketAddressEnumerator (Stack stack, int i);

#endif /* NSP_INC_NspGSocketAddressEnumerator */ 

#ifdef NspGSocketAddressEnumerator_Private 
static int init_gsocketaddressenumerator(NspGSocketAddressEnumerator *o,NspTypeGSocketAddressEnumerator *type);
static char *nsp_gsocketaddressenumerator_type_as_string(void);
static char *nsp_gsocketaddressenumerator_type_short_string(NspObject *v);
static AttrTab gsocketaddressenumerator_attrs[];
static NspMethods *gsocketaddressenumerator_get_methods(void);
/* static int int_gsocketaddressenumerator_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSocketAddressEnumerator_Private */
