/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSearchBar
#define NSP_INC_NspGtkSearchBar

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

/* NspGtkSearchBar */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkSearchBar inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkSearchBar ;
typedef NspTypeGtkBin NspTypeGtkSearchBar ;

extern int nsp_type_gtksearchbar_id;
extern NspTypeGtkSearchBar *nsp_type_gtksearchbar;

/* type instances for gtkbin */

NspTypeGtkSearchBar *new_type_gtksearchbar(type_mode mode);

/* instance for NspGtkSearchBar */

NspGtkSearchBar *new_gtksearchbar();

/*
 * Object methods redefined for gtksearchbar 
 */

#define NULLGTKSEARCHBAR (NspGtkSearchBar*) 0


/* from NspGtkSearchBarObj.c */

extern NspGtkSearchBar *nsp_gtksearchbar_object (NspObject *O);
extern int IsGtkSearchBarObj (Stack stack, int i);
extern int IsGtkSearchBar(NspObject *O);
extern NspGtkSearchBar *GetGtkSearchBarCopy (Stack stack, int i);
extern NspGtkSearchBar *GetGtkSearchBar (Stack stack, int i);

#endif /* NSP_INC_NspGtkSearchBar */ 

#ifdef NspGtkSearchBar_Private 
static int init_gtksearchbar(NspGtkSearchBar *o,NspTypeGtkSearchBar *type);
static char *nsp_gtksearchbar_type_as_string(void);
static char *nsp_gtksearchbar_type_short_string(NspObject *v);
static AttrTab gtksearchbar_attrs[];
static NspMethods *gtksearchbar_get_methods(void);
/* static int int_gtksearchbar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSearchBar_Private */
