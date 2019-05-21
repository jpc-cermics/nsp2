/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkRelation
#define NSP_INC_NspAtkRelation

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

/* NspAtkRelation */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkRelation inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkRelation ;
typedef NspTypeGObject NspTypeAtkRelation ;

extern int nsp_type_atkrelation_id;
extern NspTypeAtkRelation *nsp_type_atkrelation;

/* type instances for gobject */

NspTypeAtkRelation *new_type_atkrelation(type_mode mode);

/* instance for NspAtkRelation */

NspAtkRelation *new_atkrelation();

/*
 * Object methods redefined for atkrelation 
 */

#define NULLATKRELATION (NspAtkRelation*) 0


/* from NspAtkRelationObj.c */

extern NspAtkRelation *nsp_atkrelation_object (NspObject *O);
extern int IsAtkRelationObj (Stack stack, int i);
extern int IsAtkRelation(NspObject *O);
extern NspAtkRelation *GetAtkRelationCopy (Stack stack, int i);
extern NspAtkRelation *GetAtkRelation (Stack stack, int i);

#endif /* NSP_INC_NspAtkRelation */ 

#ifdef NspAtkRelation_Private 
static int init_atkrelation(NspAtkRelation *o,NspTypeAtkRelation *type);
static char *nsp_atkrelation_type_as_string(void);
static char *nsp_atkrelation_type_short_string(NspObject *v);
static AttrTab atkrelation_attrs[];
static NspMethods *atkrelation_get_methods(void);
/* static int int_atkrelation_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkRelation_Private */
