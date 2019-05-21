/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkRectangle
#define NSP_INC_NspGdkRectangle

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

/* NspGdkRectangle */

#include <nsp/gtk/gboxed.h>

/*
 * NspGdkRectangle inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGdkRectangle ;
typedef NspTypeGBoxed NspTypeGdkRectangle ;

extern int nsp_type_gdkrectangle_id;
extern NspTypeGdkRectangle *nsp_type_gdkrectangle;

/* type instances for gboxed */

NspTypeGdkRectangle *new_type_gdkrectangle(type_mode mode);

/* instance for NspGdkRectangle */

NspGdkRectangle *new_gdkrectangle();

/*
 * Object methods redefined for gdkrectangle 
 */

#define NULLGDKRECTANGLE (NspGdkRectangle*) 0


/* from NspGdkRectangleObj.c */

extern NspGdkRectangle *nsp_gdkrectangle_object (NspObject *O);
extern int IsGdkRectangleObj (Stack stack, int i);
extern int IsGdkRectangle(NspObject *O);
extern NspGdkRectangle *GetGdkRectangleCopy (Stack stack, int i);
extern NspGdkRectangle *GetGdkRectangle (Stack stack, int i);

#endif /* NSP_INC_NspGdkRectangle */ 

#ifdef NspGdkRectangle_Private 
static int init_gdkrectangle(NspGdkRectangle *o,NspTypeGdkRectangle *type);
static char *nsp_gdkrectangle_type_as_string(void);
static char *nsp_gdkrectangle_type_short_string(NspObject *v);
static AttrTab gdkrectangle_attrs[];
static NspMethods *gdkrectangle_get_methods(void);
/* static int int_gdkrectangle_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkRectangle_Private */
