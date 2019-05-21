/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAtkRectangle
#define NSP_INC_NspAtkRectangle

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

/* NspAtkRectangle */

#include <nsp/gtk/gboxed.h>

/*
 * NspAtkRectangle inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspAtkRectangle ;
typedef NspTypeGBoxed NspTypeAtkRectangle ;

extern int nsp_type_atkrectangle_id;
extern NspTypeAtkRectangle *nsp_type_atkrectangle;

/* type instances for gboxed */

NspTypeAtkRectangle *new_type_atkrectangle(type_mode mode);

/* instance for NspAtkRectangle */

NspAtkRectangle *new_atkrectangle();

/*
 * Object methods redefined for atkrectangle 
 */

#define NULLATKRECTANGLE (NspAtkRectangle*) 0


/* from NspAtkRectangleObj.c */

extern NspAtkRectangle *nsp_atkrectangle_object (NspObject *O);
extern int IsAtkRectangleObj (Stack stack, int i);
extern int IsAtkRectangle(NspObject *O);
extern NspAtkRectangle *GetAtkRectangleCopy (Stack stack, int i);
extern NspAtkRectangle *GetAtkRectangle (Stack stack, int i);

#endif /* NSP_INC_NspAtkRectangle */ 

#ifdef NspAtkRectangle_Private 
static int init_atkrectangle(NspAtkRectangle *o,NspTypeAtkRectangle *type);
static char *nsp_atkrectangle_type_as_string(void);
static char *nsp_atkrectangle_type_short_string(NspObject *v);
static AttrTab atkrectangle_attrs[];
static NspMethods *atkrectangle_get_methods(void);
/* static int int_atkrectangle_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspAtkRectangle_Private */
