/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAccessible
#define NSP_INC_NspGtkAccessible

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

/* NspGtkAccessible */

#include <nsp/gtk/atkobject.h>

/*
 * NspGtkAccessible inherits from AtkObject
 * just change some type attributes 
 */

typedef NspAtkObject NspGtkAccessible ;
typedef NspTypeAtkObject NspTypeGtkAccessible ;

extern int nsp_type_gtkaccessible_id;
extern NspTypeGtkAccessible *nsp_type_gtkaccessible;

/* type instances for atkobject */

NspTypeGtkAccessible *new_type_gtkaccessible(type_mode mode);

/* instance for NspGtkAccessible */

NspGtkAccessible *new_gtkaccessible();

/*
 * Object methods redefined for gtkaccessible 
 */

#define NULLGTKACCESSIBLE (NspGtkAccessible*) 0


/* from NspGtkAccessibleObj.c */

extern NspGtkAccessible *nsp_gtkaccessible_object (NspObject *O);
extern int IsGtkAccessibleObj (Stack stack, int i);
extern int IsGtkAccessible(NspObject *O);
extern NspGtkAccessible *GetGtkAccessibleCopy (Stack stack, int i);
extern NspGtkAccessible *GetGtkAccessible (Stack stack, int i);

#endif /* NSP_INC_NspGtkAccessible */ 

#ifdef NspGtkAccessible_Private 
static int init_gtkaccessible(NspGtkAccessible *o,NspTypeGtkAccessible *type);
static char *nsp_gtkaccessible_type_as_string(void);
static char *nsp_gtkaccessible_type_short_string(NspObject *v);
static AttrTab gtkaccessible_attrs[];
static NspMethods *gtkaccessible_get_methods(void);
/* static int int_gtkaccessible_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAccessible_Private */
