/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGResolver
#define NSP_INC_NspGResolver

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

/* NspGResolver */

#include <nsp/gtk/gobject.h>

/*
 * NspGResolver inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGResolver ;
typedef NspTypeGObject NspTypeGResolver ;

extern int nsp_type_gresolver_id;
extern NspTypeGResolver *nsp_type_gresolver;

/* type instances for gobject */

NspTypeGResolver *new_type_gresolver(type_mode mode);

/* instance for NspGResolver */

NspGResolver *new_gresolver();

/*
 * Object methods redefined for gresolver 
 */

#define NULLGRESOLVER (NspGResolver*) 0


/* from NspGResolverObj.c */

extern NspGResolver *nsp_gresolver_object (NspObject *O);
extern int IsGResolverObj (Stack stack, int i);
extern int IsGResolver(NspObject *O);
extern NspGResolver *GetGResolverCopy (Stack stack, int i);
extern NspGResolver *GetGResolver (Stack stack, int i);

#endif /* NSP_INC_NspGResolver */ 

#ifdef NspGResolver_Private 
static int init_gresolver(NspGResolver *o,NspTypeGResolver *type);
static char *nsp_gresolver_type_as_string(void);
static char *nsp_gresolver_type_short_string(NspObject *v);
static AttrTab gresolver_attrs[];
static NspMethods *gresolver_get_methods(void);
/* static int int_gresolver_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGResolver_Private */
