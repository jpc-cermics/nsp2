/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkNoOpObject
#define NSP_INC_NspAtkNoOpObject

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

/* NspAtkNoOpObject */

#include <nsp/gtk/atkobject.h>

/*
 * NspAtkNoOpObject inherits from AtkObject
 * just change some type attributes 
 */

typedef NspAtkObject NspAtkNoOpObject ;
typedef NspTypeAtkObject NspTypeAtkNoOpObject ;

extern int nsp_type_atknoopobject_id;
extern NspTypeAtkNoOpObject *nsp_type_atknoopobject;

/* type instances for atkobject */

NspTypeAtkNoOpObject *new_type_atknoopobject(type_mode mode);

/* instance for NspAtkNoOpObject */

NspAtkNoOpObject *new_atknoopobject();

/*
 * Object methods redefined for atknoopobject 
 */

#define NULLATKNOOPOBJECT (NspAtkNoOpObject*) 0


/* from NspAtkNoOpObjectObj.c */

extern NspAtkNoOpObject *nsp_atknoopobject_object (NspObject *O);
extern int IsAtkNoOpObjectObj (Stack stack, int i);
extern int IsAtkNoOpObject(NspObject *O);
extern NspAtkNoOpObject *GetAtkNoOpObjectCopy (Stack stack, int i);
extern NspAtkNoOpObject *GetAtkNoOpObject (Stack stack, int i);

#endif /* NSP_INC_NspAtkNoOpObject */ 

#ifdef NspAtkNoOpObject_Private 
static int init_atknoopobject(NspAtkNoOpObject *o,NspTypeAtkNoOpObject *type);
static char *nsp_atknoopobject_type_as_string(void);
static char *nsp_atknoopobject_type_short_string(NspObject *v);
static AttrTab atknoopobject_attrs[];
static NspMethods *atknoopobject_get_methods(void);
/* static int int_atknoopobject_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkNoOpObject_Private */
