/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkPixbufLoader
#define NSP_INC_NspGdkPixbufLoader

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

/* NspGdkPixbufLoader */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkPixbufLoader inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkPixbufLoader ;
typedef NspTypeGObject NspTypeGdkPixbufLoader ;

extern int nsp_type_gdkpixbufloader_id;
extern NspTypeGdkPixbufLoader *nsp_type_gdkpixbufloader;

/* type instances for gobject */

NspTypeGdkPixbufLoader *new_type_gdkpixbufloader(type_mode mode);

/* instance for NspGdkPixbufLoader */

NspGdkPixbufLoader *new_gdkpixbufloader();

/*
 * Object methods redefined for gdkpixbufloader 
 */

#define NULLGDKPIXBUFLOADER (NspGdkPixbufLoader*) 0


/* from NspGdkPixbufLoaderObj.c */

extern NspGdkPixbufLoader *nsp_gdkpixbufloader_object (NspObject *O);
extern int IsGdkPixbufLoaderObj (Stack stack, int i);
extern int IsGdkPixbufLoader(NspObject *O);
extern NspGdkPixbufLoader *GetGdkPixbufLoaderCopy (Stack stack, int i);
extern NspGdkPixbufLoader *GetGdkPixbufLoader (Stack stack, int i);

#endif /* NSP_INC_NspGdkPixbufLoader */ 

#ifdef NspGdkPixbufLoader_Private 
static int init_gdkpixbufloader(NspGdkPixbufLoader *o,NspTypeGdkPixbufLoader *type);
static char *nsp_gdkpixbufloader_type_as_string(void);
static char *nsp_gdkpixbufloader_type_short_string(NspObject *v);
static AttrTab gdkpixbufloader_attrs[];
static NspMethods *gdkpixbufloader_get_methods(void);
/* static int int_gdkpixbufloader_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkPixbufLoader_Private */
