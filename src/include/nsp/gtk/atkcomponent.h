/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkComponent
#define NSP_INC_NspAtkComponent

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

/* NspAtkComponent */

#include <nsp/gtk/gobject.h>

/*
 * NspAtkComponent inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspAtkComponent ;
typedef NspTypeGObject NspTypeAtkComponent ;

extern int nsp_type_atkcomponent_id;
extern NspTypeAtkComponent *nsp_type_atkcomponent;

/* type instances for gobject */

NspTypeAtkComponent *new_type_atkcomponent(type_mode mode);

/* instance for NspAtkComponent */

NspAtkComponent *new_atkcomponent();

/*
 * Object methods redefined for atkcomponent 
 */

#define NULLATKCOMPONENT (NspAtkComponent*) 0


/* from NspAtkComponentObj.c */

extern NspAtkComponent *nsp_atkcomponent_object (NspObject *O);
extern int IsAtkComponentObj (Stack stack, int i);
extern int IsAtkComponent(NspObject *O);
extern NspAtkComponent *GetAtkComponentCopy (Stack stack, int i);
extern NspAtkComponent *GetAtkComponent (Stack stack, int i);

#endif /* NSP_INC_NspAtkComponent */ 

#ifdef NspAtkComponent_Private 
static int init_atkcomponent(NspAtkComponent *o,NspTypeAtkComponent *type);
static char *nsp_atkcomponent_type_as_string(void);
static char *nsp_atkcomponent_type_short_string(NspObject *v);
static AttrTab atkcomponent_attrs[];
static NspMethods *atkcomponent_get_methods(void);
/* static int int_atkcomponent_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkComponent_Private */
