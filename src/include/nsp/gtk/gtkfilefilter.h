/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFileFilter
#define NSP_INC_NspGtkFileFilter

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

/* NspGtkFileFilter */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkFileFilter inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkFileFilter ;
typedef NspTypeGObject NspTypeGtkFileFilter ;

extern int nsp_type_gtkfilefilter_id;
extern NspTypeGtkFileFilter *nsp_type_gtkfilefilter;

/* type instances for gobject */

NspTypeGtkFileFilter *new_type_gtkfilefilter(type_mode mode);

/* instance for NspGtkFileFilter */

NspGtkFileFilter *new_gtkfilefilter();

/*
 * Object methods redefined for gtkfilefilter 
 */

#define NULLGTKFILEFILTER (NspGtkFileFilter*) 0


/* from NspGtkFileFilterObj.c */

extern NspGtkFileFilter *nsp_gtkfilefilter_object (NspObject *O);
extern int IsGtkFileFilterObj (Stack stack, int i);
extern int IsGtkFileFilter(NspObject *O);
extern NspGtkFileFilter *GetGtkFileFilterCopy (Stack stack, int i);
extern NspGtkFileFilter *GetGtkFileFilter (Stack stack, int i);

#endif /* NSP_INC_NspGtkFileFilter */ 

#ifdef NspGtkFileFilter_Private 
static int init_gtkfilefilter(NspGtkFileFilter *o,NspTypeGtkFileFilter *type);
static char *nsp_gtkfilefilter_type_as_string(void);
static char *nsp_gtkfilefilter_type_short_string(NspObject *v);
static AttrTab gtkfilefilter_attrs[];
static NspMethods *gtkfilefilter_get_methods(void);
/* static int int_gtkfilefilter_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFileFilter_Private */
