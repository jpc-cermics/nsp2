/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeStore
#define NSP_INC_NspGtkTreeStore

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

/* NspGtkTreeStore */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTreeStore inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTreeStore ;
typedef NspTypeGObject NspTypeGtkTreeStore ;

extern int nsp_type_gtktreestore_id;
extern NspTypeGtkTreeStore *nsp_type_gtktreestore;

/* type instances for gobject */

NspTypeGtkTreeStore *new_type_gtktreestore(type_mode mode);

/* instance for NspGtkTreeStore */

NspGtkTreeStore *new_gtktreestore();

/*
 * Object methods redefined for gtktreestore 
 */

#define NULLGTKTREESTORE (NspGtkTreeStore*) 0


/* from NspGtkTreeStoreObj.c */

extern NspGtkTreeStore *nsp_gtktreestore_object (NspObject *O);
extern int IsGtkTreeStoreObj (Stack stack, int i);
extern int IsGtkTreeStore(NspObject *O);
extern NspGtkTreeStore *GetGtkTreeStoreCopy (Stack stack, int i);
extern NspGtkTreeStore *GetGtkTreeStore (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeStore */ 

#ifdef NspGtkTreeStore_Private 
static int init_gtktreestore(NspGtkTreeStore *o,NspTypeGtkTreeStore *type);
static char *nsp_gtktreestore_type_as_string(void);
static char *nsp_gtktreestore_type_short_string(NspObject *v);
static AttrTab gtktreestore_attrs[];
static NspMethods *gtktreestore_get_methods(void);
/* static int int_gtktreestore_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeStore_Private */
