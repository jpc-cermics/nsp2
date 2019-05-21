/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkStateSet
#define NSP_INC_NspAtkStateSet

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

/* NspAtkStateSet */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkStateSet inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkStateSet ;
typedef NspTypeGObject NspTypeAtkStateSet ;

extern int nsp_type_atkstateset_id;
extern NspTypeAtkStateSet *nsp_type_atkstateset;

/* type instances for gobject */

NspTypeAtkStateSet *new_type_atkstateset(type_mode mode);

/* instance for NspAtkStateSet */

NspAtkStateSet *new_atkstateset();

/*
 * Object methods redefined for atkstateset 
 */

#define NULLATKSTATESET (NspAtkStateSet*) 0


/* from NspAtkStateSetObj.c */

extern NspAtkStateSet *nsp_atkstateset_object (NspObject *O);
extern int IsAtkStateSetObj (Stack stack, int i);
extern int IsAtkStateSet(NspObject *O);
extern NspAtkStateSet *GetAtkStateSetCopy (Stack stack, int i);
extern NspAtkStateSet *GetAtkStateSet (Stack stack, int i);

#endif /* NSP_INC_NspAtkStateSet */ 

#ifdef NspAtkStateSet_Private 
static int init_atkstateset(NspAtkStateSet *o,NspTypeAtkStateSet *type);
static char *nsp_atkstateset_type_as_string(void);
static char *nsp_atkstateset_type_short_string(NspObject *v);
static AttrTab atkstateset_attrs[];
static NspMethods *atkstateset_get_methods(void);
/* static int int_atkstateset_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkStateSet_Private */
