/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGResource
#define NSP_INC_NspGResource

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

/* NspGResource */

#include <nsp/gtk/gboxed.h>

/*
 * NspGResource inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGResource ;
typedef NspTypeGBoxed NspTypeGResource ;

extern int nsp_type_gresource_id;
extern NspTypeGResource *nsp_type_gresource;

/* type instances for gboxed */

NspTypeGResource *new_type_gresource(type_mode mode);

/* instance for NspGResource */

NspGResource *new_gresource();

/*
 * Object methods redefined for gresource 
 */

#define NULLGRESOURCE (NspGResource*) 0


/* from NspGResourceObj.c */

extern NspGResource *nsp_gresource_object (NspObject *O);
extern int IsGResourceObj (Stack stack, int i);
extern int IsGResource(NspObject *O);
extern NspGResource *GetGResourceCopy (Stack stack, int i);
extern NspGResource *GetGResource (Stack stack, int i);

#endif /* NSP_INC_NspGResource */ 

#ifdef NspGResource_Private 
static int init_gresource(NspGResource *o,NspTypeGResource *type);
static char *nsp_gresource_type_as_string(void);
static char *nsp_gresource_type_short_string(NspObject *v);
static AttrTab gresource_attrs[];
static NspMethods *gresource_get_methods(void);
/* static int int_gresource_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGResource_Private */
