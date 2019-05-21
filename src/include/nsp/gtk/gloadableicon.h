/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGLoadableIcon
#define NSP_INC_NspGLoadableIcon

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

/* NspGLoadableIcon */

#include <nsp/gtk/gobject.h>

/*
 * NspGLoadableIcon inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGLoadableIcon ;
typedef NspTypeGObject NspTypeGLoadableIcon ;

extern int nsp_type_gloadableicon_id;
extern NspTypeGLoadableIcon *nsp_type_gloadableicon;

/* type instances for gobject */

NspTypeGLoadableIcon *new_type_gloadableicon(type_mode mode);

/* instance for NspGLoadableIcon */

NspGLoadableIcon *new_gloadableicon();

/*
 * Object methods redefined for gloadableicon 
 */

#define NULLGLOADABLEICON (NspGLoadableIcon*) 0


/* from NspGLoadableIconObj.c */

extern NspGLoadableIcon *nsp_gloadableicon_object (NspObject *O);
extern int IsGLoadableIconObj (Stack stack, int i);
extern int IsGLoadableIcon(NspObject *O);
extern NspGLoadableIcon *GetGLoadableIconCopy (Stack stack, int i);
extern NspGLoadableIcon *GetGLoadableIcon (Stack stack, int i);

#endif /* NSP_INC_NspGLoadableIcon */ 

#ifdef NspGLoadableIcon_Private 
static int init_gloadableicon(NspGLoadableIcon *o,NspTypeGLoadableIcon *type);
static char *nsp_gloadableicon_type_as_string(void);
static char *nsp_gloadableicon_type_short_string(NspObject *v);
static AttrTab gloadableicon_attrs[];
static NspMethods *gloadableicon_get_methods(void);
/* static int int_gloadableicon_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGLoadableIcon_Private */
