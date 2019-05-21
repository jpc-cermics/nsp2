/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkActionBar
#define NSP_INC_NspGtkActionBar

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

/* NspGtkActionBar */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkActionBar inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkActionBar ;
typedef NspTypeGtkBin NspTypeGtkActionBar ;

extern int nsp_type_gtkactionbar_id;
extern NspTypeGtkActionBar *nsp_type_gtkactionbar;

/* type instances for gtkbin */

NspTypeGtkActionBar *new_type_gtkactionbar(type_mode mode);

/* instance for NspGtkActionBar */

NspGtkActionBar *new_gtkactionbar();

/*
 * Object methods redefined for gtkactionbar 
 */

#define NULLGTKACTIONBAR (NspGtkActionBar*) 0


/* from NspGtkActionBarObj.c */

extern NspGtkActionBar *nsp_gtkactionbar_object (NspObject *O);
extern int IsGtkActionBarObj (Stack stack, int i);
extern int IsGtkActionBar(NspObject *O);
extern NspGtkActionBar *GetGtkActionBarCopy (Stack stack, int i);
extern NspGtkActionBar *GetGtkActionBar (Stack stack, int i);

#endif /* NSP_INC_NspGtkActionBar */ 

#ifdef NspGtkActionBar_Private 
static int init_gtkactionbar(NspGtkActionBar *o,NspTypeGtkActionBar *type);
static char *nsp_gtkactionbar_type_as_string(void);
static char *nsp_gtkactionbar_type_short_string(NspObject *v);
static AttrTab gtkactionbar_attrs[];
static NspMethods *gtkactionbar_get_methods(void);
/* static int int_gtkactionbar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkActionBar_Private */
