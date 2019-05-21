/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkPixmap
#define NSP_INC_NspGdkPixmap

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

/* NspGdkPixmap */

#include <nsp/gtk/gdkdrawable.h>

/*
 * NspGdkPixmap inherits from GdkDrawable
 * just change some type attributes 
 */

typedef NspGdkDrawable NspGdkPixmap ;
typedef NspTypeGdkDrawable NspTypeGdkPixmap ;

extern int nsp_type_gdkpixmap_id;
extern NspTypeGdkPixmap *nsp_type_gdkpixmap;

/* type instances for gdkdrawable */

NspTypeGdkPixmap *new_type_gdkpixmap(type_mode mode);

/* instance for NspGdkPixmap */

NspGdkPixmap *new_gdkpixmap();

/*
 * Object methods redefined for gdkpixmap 
 */

#define NULLGDKPIXMAP (NspGdkPixmap*) 0


/* from NspGdkPixmapObj.c */

extern NspGdkPixmap *nsp_gdkpixmap_object (NspObject *O);
extern int IsGdkPixmapObj (Stack stack, int i);
extern int IsGdkPixmap(NspObject *O);
extern NspGdkPixmap *GetGdkPixmapCopy (Stack stack, int i);
extern NspGdkPixmap *GetGdkPixmap (Stack stack, int i);

#endif /* NSP_INC_NspGdkPixmap */ 

#ifdef NspGdkPixmap_Private 
static int init_gdkpixmap(NspGdkPixmap *o,NspTypeGdkPixmap *type);
static char *nsp_gdkpixmap_type_as_string(void);
static char *nsp_gdkpixmap_type_short_string(NspObject *v);
static AttrTab gdkpixmap_attrs[];
static NspMethods *gdkpixmap_get_methods(void);
/* static int int_gdkpixmap_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkPixmap_Private */
