/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkRelationSet
#define NSP_INC_NspAtkRelationSet

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

/* NspAtkRelationSet */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkRelationSet inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkRelationSet ;
typedef NspTypeGObject NspTypeAtkRelationSet ;

extern int nsp_type_atkrelationset_id;
extern NspTypeAtkRelationSet *nsp_type_atkrelationset;

/* type instances for gobject */

NspTypeAtkRelationSet *new_type_atkrelationset(type_mode mode);

/* instance for NspAtkRelationSet */

NspAtkRelationSet *new_atkrelationset();

/*
 * Object methods redefined for atkrelationset 
 */

#define NULLATKRELATIONSET (NspAtkRelationSet*) 0


/* from NspAtkRelationSetObj.c */

extern NspAtkRelationSet *nsp_atkrelationset_object (NspObject *O);
extern int IsAtkRelationSetObj (Stack stack, int i);
extern int IsAtkRelationSet(NspObject *O);
extern NspAtkRelationSet *GetAtkRelationSetCopy (Stack stack, int i);
extern NspAtkRelationSet *GetAtkRelationSet (Stack stack, int i);

#endif /* NSP_INC_NspAtkRelationSet */ 

#ifdef NspAtkRelationSet_Private 
static int init_atkrelationset(NspAtkRelationSet *o,NspTypeAtkRelationSet *type);
static char *nsp_atkrelationset_type_as_string(void);
static char *nsp_atkrelationset_type_short_string(NspObject *v);
static AttrTab atkrelationset_attrs[];
static NspMethods *atkrelationset_get_methods(void);
/* static int int_atkrelationset_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkRelationSet_Private */
