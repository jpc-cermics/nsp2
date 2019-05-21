/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkColormap
#define NSP_INC_NspGdkColormap

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

/* NspGdkColormap */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkColormap inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkColormap ;
typedef NspTypeGObject NspTypeGdkColormap ;

extern int nsp_type_gdkcolormap_id;
extern NspTypeGdkColormap *nsp_type_gdkcolormap;

/* type instances for gobject */

NspTypeGdkColormap *new_type_gdkcolormap(type_mode mode);

/* instance for NspGdkColormap */

NspGdkColormap *new_gdkcolormap();

/*
 * Object methods redefined for gdkcolormap 
 */

#define NULLGDKCOLORMAP (NspGdkColormap*) 0


/* from NspGdkColormapObj.c */

extern NspGdkColormap *nsp_gdkcolormap_object (NspObject *O);
extern int IsGdkColormapObj (Stack stack, int i);
extern int IsGdkColormap(NspObject *O);
extern NspGdkColormap *GetGdkColormapCopy (Stack stack, int i);
extern NspGdkColormap *GetGdkColormap (Stack stack, int i);

#endif /* NSP_INC_NspGdkColormap */ 

#ifdef NspGdkColormap_Private 
static int init_gdkcolormap(NspGdkColormap *o,NspTypeGdkColormap *type);
static char *nsp_gdkcolormap_type_as_string(void);
static char *nsp_gdkcolormap_type_short_string(NspObject *v);
static AttrTab gdkcolormap_attrs[];
static NspMethods *gdkcolormap_get_methods(void);
/* static int int_gdkcolormap_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkColormap_Private */
