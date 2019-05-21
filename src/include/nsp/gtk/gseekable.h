/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGSeekable
#define NSP_INC_NspGSeekable

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

/* NspGSeekable */

#include <nsp/gtk/gobject.h>

/*
 * NspGSeekable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGSeekable ;
typedef NspTypeGObject NspTypeGSeekable ;

extern int nsp_type_gseekable_id;
extern NspTypeGSeekable *nsp_type_gseekable;

/* type instances for gobject */

NspTypeGSeekable *new_type_gseekable(type_mode mode);

/* instance for NspGSeekable */

NspGSeekable *new_gseekable();

/*
 * Object methods redefined for gseekable 
 */

#define NULLGSEEKABLE (NspGSeekable*) 0


/* from NspGSeekableObj.c */

extern NspGSeekable *nsp_gseekable_object (NspObject *O);
extern int IsGSeekableObj (Stack stack, int i);
extern int IsGSeekable(NspObject *O);
extern NspGSeekable *GetGSeekableCopy (Stack stack, int i);
extern NspGSeekable *GetGSeekable (Stack stack, int i);

#endif /* NSP_INC_NspGSeekable */ 

#ifdef NspGSeekable_Private 
static int init_gseekable(NspGSeekable *o,NspTypeGSeekable *type);
static char *nsp_gseekable_type_as_string(void);
static char *nsp_gseekable_type_short_string(NspObject *v);
static AttrTab gseekable_attrs[];
static NspMethods *gseekable_get_methods(void);
/* static int int_gseekable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGSeekable_Private */
