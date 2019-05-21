/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGCancellable
#define NSP_INC_NspGCancellable

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

/* NspGCancellable */

#include <nsp/gtk/gobject.h>

/*
 * NspGCancellable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGCancellable ;
typedef NspTypeGObject NspTypeGCancellable ;

extern int nsp_type_gcancellable_id;
extern NspTypeGCancellable *nsp_type_gcancellable;

/* type instances for gobject */

NspTypeGCancellable *new_type_gcancellable(type_mode mode);

/* instance for NspGCancellable */

NspGCancellable *new_gcancellable();

/*
 * Object methods redefined for gcancellable 
 */

#define NULLGCANCELLABLE (NspGCancellable*) 0


/* from NspGCancellableObj.c */

extern NspGCancellable *nsp_gcancellable_object (NspObject *O);
extern int IsGCancellableObj (Stack stack, int i);
extern int IsGCancellable(NspObject *O);
extern NspGCancellable *GetGCancellableCopy (Stack stack, int i);
extern NspGCancellable *GetGCancellable (Stack stack, int i);

#endif /* NSP_INC_NspGCancellable */ 

#ifdef NspGCancellable_Private 
static int init_gcancellable(NspGCancellable *o,NspTypeGCancellable *type);
static char *nsp_gcancellable_type_as_string(void);
static char *nsp_gcancellable_type_short_string(NspObject *v);
static AttrTab gcancellable_attrs[];
static NspMethods *gcancellable_get_methods(void);
/* static int int_gcancellable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGCancellable_Private */
