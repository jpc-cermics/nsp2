/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkDrawable
#define NSP_INC_NspGdkDrawable

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

/* NspGdkDrawable */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkDrawable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkDrawable ;
typedef NspTypeGObject NspTypeGdkDrawable ;

extern int nsp_type_gdkdrawable_id;
extern NspTypeGdkDrawable *nsp_type_gdkdrawable;

/* type instances for gobject */

NspTypeGdkDrawable *new_type_gdkdrawable(type_mode mode);

/* instance for NspGdkDrawable */

NspGdkDrawable *new_gdkdrawable();

/*
 * Object methods redefined for gdkdrawable 
 */

#define NULLGDKDRAWABLE (NspGdkDrawable*) 0


/* from NspGdkDrawableObj.c */

extern NspGdkDrawable *nsp_gdkdrawable_object (NspObject *O);
extern int IsGdkDrawableObj (Stack stack, int i);
extern int IsGdkDrawable(NspObject *O);
extern NspGdkDrawable *GetGdkDrawableCopy (Stack stack, int i);
extern NspGdkDrawable *GetGdkDrawable (Stack stack, int i);

#endif /* NSP_INC_NspGdkDrawable */ 

#ifdef NspGdkDrawable_Private 
static int init_gdkdrawable(NspGdkDrawable *o,NspTypeGdkDrawable *type);
static char *nsp_gdkdrawable_type_as_string(void);
static char *nsp_gdkdrawable_type_short_string(NspObject *v);
static AttrTab gdkdrawable_attrs[];
static NspMethods *gdkdrawable_get_methods(void);
/* static int int_gdkdrawable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkDrawable_Private */
