/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreePath
#define NSP_INC_NspGtkTreePath

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

/* NspGtkTreePath */

#include <nsp/gtk/gboxed.h>

/*
 * NspGtkTreePath inherits from GBoxed
 * just change some type attributes 
 */

typedef NspGBoxed NspGtkTreePath ;
typedef NspTypeGBoxed NspTypeGtkTreePath ;

extern int nsp_type_gtktreepath_id;
extern NspTypeGtkTreePath *nsp_type_gtktreepath;

/* type instances for gboxed */

NspTypeGtkTreePath *new_type_gtktreepath(type_mode mode);

/* instance for NspGtkTreePath */

NspGtkTreePath *new_gtktreepath();

/*
 * Object methods redefined for gtktreepath 
 */

#define NULLGTKTREEPATH (NspGtkTreePath*) 0


/* from NspGtkTreePathObj.c */

extern NspGtkTreePath *nsp_gtktreepath_object (NspObject *O);
extern int IsGtkTreePathObj (Stack stack, int i);
extern int IsGtkTreePath(NspObject *O);
extern NspGtkTreePath *GetGtkTreePathCopy (Stack stack, int i);
extern NspGtkTreePath *GetGtkTreePath (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreePath */ 

#ifdef NspGtkTreePath_Private 
static int init_gtktreepath(NspGtkTreePath *o,NspTypeGtkTreePath *type);
static char *nsp_gtktreepath_type_as_string(void);
static char *nsp_gtktreepath_type_short_string(NspObject *v);
static AttrTab gtktreepath_attrs[];
static NspMethods *gtktreepath_get_methods(void);
/* static int int_gtktreepath_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreePath_Private */
