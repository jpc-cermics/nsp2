/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkListStore
#define NSP_INC_NspGtkListStore

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

/* NspGtkListStore */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkListStore inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkListStore ;
typedef NspTypeGObject NspTypeGtkListStore ;

extern int nsp_type_gtkliststore_id;
extern NspTypeGtkListStore *nsp_type_gtkliststore;

/* type instances for gobject */

NspTypeGtkListStore *new_type_gtkliststore(type_mode mode);

/* instance for NspGtkListStore */

NspGtkListStore *new_gtkliststore();

/*
 * Object methods redefined for gtkliststore 
 */

#define NULLGTKLISTSTORE (NspGtkListStore*) 0


/* from NspGtkListStoreObj.c */

extern NspGtkListStore *nsp_gtkliststore_object (NspObject *O);
extern int IsGtkListStoreObj (Stack stack, int i);
extern int IsGtkListStore(NspObject *O);
extern NspGtkListStore *GetGtkListStoreCopy (Stack stack, int i);
extern NspGtkListStore *GetGtkListStore (Stack stack, int i);

#endif /* NSP_INC_NspGtkListStore */ 

#ifdef NspGtkListStore_Private 
static int init_gtkliststore(NspGtkListStore *o,NspTypeGtkListStore *type);
static char *nsp_gtkliststore_type_as_string(void);
static char *nsp_gtkliststore_type_short_string(NspObject *v);
static AttrTab gtkliststore_attrs[];
static NspMethods *gtkliststore_get_methods(void);
/* static int int_gtkliststore_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkListStore_Private */
