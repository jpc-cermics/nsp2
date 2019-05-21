/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkImage
#define NSP_INC_NspGdkImage

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

/* NspGdkImage */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkImage inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkImage ;
typedef NspTypeGObject NspTypeGdkImage ;

extern int nsp_type_gdkimage_id;
extern NspTypeGdkImage *nsp_type_gdkimage;

/* type instances for gobject */

NspTypeGdkImage *new_type_gdkimage(type_mode mode);

/* instance for NspGdkImage */

NspGdkImage *new_gdkimage();

/*
 * Object methods redefined for gdkimage 
 */

#define NULLGDKIMAGE (NspGdkImage*) 0


/* from NspGdkImageObj.c */

extern NspGdkImage *nsp_gdkimage_object (NspObject *O);
extern int IsGdkImageObj (Stack stack, int i);
extern int IsGdkImage(NspObject *O);
extern NspGdkImage *GetGdkImageCopy (Stack stack, int i);
extern NspGdkImage *GetGdkImage (Stack stack, int i);

#endif /* NSP_INC_NspGdkImage */ 

#ifdef NspGdkImage_Private 
static int init_gdkimage(NspGdkImage *o,NspTypeGdkImage *type);
static char *nsp_gdkimage_type_as_string(void);
static char *nsp_gdkimage_type_short_string(NspObject *v);
static AttrTab gdkimage_attrs[];
static NspMethods *gdkimage_get_methods(void);
/* static int int_gdkimage_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkImage_Private */
