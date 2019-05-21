/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGIcon
#define NSP_INC_NspGIcon

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

/* NspGIcon */

#include <nsp/gtk/gobject.h>

/*
 * NspGIcon inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGIcon ;
typedef NspTypeGObject NspTypeGIcon ;

extern int nsp_type_gicon_id;
extern NspTypeGIcon *nsp_type_gicon;

/* type instances for gobject */

NspTypeGIcon *new_type_gicon(type_mode mode);

/* instance for NspGIcon */

NspGIcon *new_gicon();

/*
 * Object methods redefined for gicon 
 */

#define NULLGICON (NspGIcon*) 0


/* from NspGIconObj.c */

extern NspGIcon *nsp_gicon_object (NspObject *O);
extern int IsGIconObj (Stack stack, int i);
extern int IsGIcon(NspObject *O);
extern NspGIcon *GetGIconCopy (Stack stack, int i);
extern NspGIcon *GetGIcon (Stack stack, int i);

#endif /* NSP_INC_NspGIcon */ 

#ifdef NspGIcon_Private 
static int init_gicon(NspGIcon *o,NspTypeGIcon *type);
static char *nsp_gicon_type_as_string(void);
static char *nsp_gicon_type_short_string(NspObject *v);
static AttrTab gicon_attrs[];
static NspMethods *gicon_get_methods(void);
/* static int int_gicon_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGIcon_Private */
