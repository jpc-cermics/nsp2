/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkKeymap
#define NSP_INC_NspGdkKeymap

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

/* NspGdkKeymap */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkKeymap inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkKeymap ;
typedef NspTypeGObject NspTypeGdkKeymap ;

extern int nsp_type_gdkkeymap_id;
extern NspTypeGdkKeymap *nsp_type_gdkkeymap;

/* type instances for gobject */

NspTypeGdkKeymap *new_type_gdkkeymap(type_mode mode);

/* instance for NspGdkKeymap */

NspGdkKeymap *new_gdkkeymap();

/*
 * Object methods redefined for gdkkeymap 
 */

#define NULLGDKKEYMAP (NspGdkKeymap*) 0


/* from NspGdkKeymapObj.c */

extern NspGdkKeymap *nsp_gdkkeymap_object (NspObject *O);
extern int IsGdkKeymapObj (Stack stack, int i);
extern int IsGdkKeymap(NspObject *O);
extern NspGdkKeymap *GetGdkKeymapCopy (Stack stack, int i);
extern NspGdkKeymap *GetGdkKeymap (Stack stack, int i);

#endif /* NSP_INC_NspGdkKeymap */ 

#ifdef NspGdkKeymap_Private 
static int init_gdkkeymap(NspGdkKeymap *o,NspTypeGdkKeymap *type);
static char *nsp_gdkkeymap_type_as_string(void);
static char *nsp_gdkkeymap_type_short_string(NspObject *v);
static AttrTab gdkkeymap_attrs[];
static NspMethods *gdkkeymap_get_methods(void);
/* static int int_gdkkeymap_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkKeymap_Private */
