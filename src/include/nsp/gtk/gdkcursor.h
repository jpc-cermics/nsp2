/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkCursor
#define NSP_INC_NspGdkCursor

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

/* NspGdkCursor */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkCursor inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkCursor ;
typedef NspTypeGObject NspTypeGdkCursor ;

extern int nsp_type_gdkcursor_id;
extern NspTypeGdkCursor *nsp_type_gdkcursor;

/* type instances for gobject */

NspTypeGdkCursor *new_type_gdkcursor(type_mode mode);

/* instance for NspGdkCursor */

NspGdkCursor *new_gdkcursor();

/*
 * Object methods redefined for gdkcursor 
 */

#define NULLGDKCURSOR (NspGdkCursor*) 0


/* from NspGdkCursorObj.c */

extern NspGdkCursor *nsp_gdkcursor_object (NspObject *O);
extern int IsGdkCursorObj (Stack stack, int i);
extern int IsGdkCursor(NspObject *O);
extern NspGdkCursor *GetGdkCursorCopy (Stack stack, int i);
extern NspGdkCursor *GetGdkCursor (Stack stack, int i);

#endif /* NSP_INC_NspGdkCursor */ 

#ifdef NspGdkCursor_Private 
static int init_gdkcursor(NspGdkCursor *o,NspTypeGdkCursor *type);
static char *nsp_gdkcursor_type_as_string(void);
static char *nsp_gdkcursor_type_short_string(NspObject *v);
static AttrTab gdkcursor_attrs[];
static NspMethods *gdkcursor_get_methods(void);
/* static int int_gdkcursor_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkCursor_Private */
