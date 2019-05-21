/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkBitmap
#define NSP_INC_NspGdkBitmap

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

/* NspGdkBitmap */

#include <nsp/gtk/gdkdrawable.h>

/*
 * NspGdkBitmap inherits from GdkDrawable
 * just change some type attributes 
 */

typedef NspGdkDrawable NspGdkBitmap ;
typedef NspTypeGdkDrawable NspTypeGdkBitmap ;

extern int nsp_type_gdkbitmap_id;
extern NspTypeGdkBitmap *nsp_type_gdkbitmap;

/* type instances for gdkdrawable */

NspTypeGdkBitmap *new_type_gdkbitmap(type_mode mode);

/* instance for NspGdkBitmap */

NspGdkBitmap *new_gdkbitmap();

/*
 * Object methods redefined for gdkbitmap 
 */

#define NULLGDKBITMAP (NspGdkBitmap*) 0


/* from NspGdkBitmapObj.c */

extern NspGdkBitmap *nsp_gdkbitmap_object (NspObject *O);
extern int IsGdkBitmapObj (Stack stack, int i);
extern int IsGdkBitmap(NspObject *O);
extern NspGdkBitmap *GetGdkBitmapCopy (Stack stack, int i);
extern NspGdkBitmap *GetGdkBitmap (Stack stack, int i);

#endif /* NSP_INC_NspGdkBitmap */ 

#ifdef NspGdkBitmap_Private 
static int init_gdkbitmap(NspGdkBitmap *o,NspTypeGdkBitmap *type);
static char *nsp_gdkbitmap_type_as_string(void);
static char *nsp_gdkbitmap_type_short_string(NspObject *v);
static AttrTab gdkbitmap_attrs[];
static NspMethods *gdkbitmap_get_methods(void);
/* static int int_gdkbitmap_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkBitmap_Private */
