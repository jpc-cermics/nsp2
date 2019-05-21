/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkOrientable
#define NSP_INC_NspGtkOrientable

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

/* NspGtkOrientable */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkOrientable inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkOrientable ;
typedef NspTypeGObject NspTypeGtkOrientable ;

extern int nsp_type_gtkorientable_id;
extern NspTypeGtkOrientable *nsp_type_gtkorientable;

/* type instances for gobject */

NspTypeGtkOrientable *new_type_gtkorientable(type_mode mode);

/* instance for NspGtkOrientable */

NspGtkOrientable *new_gtkorientable();

/*
 * Object methods redefined for gtkorientable 
 */

#define NULLGTKORIENTABLE (NspGtkOrientable*) 0


/* from NspGtkOrientableObj.c */

extern NspGtkOrientable *nsp_gtkorientable_object (NspObject *O);
extern int IsGtkOrientableObj (Stack stack, int i);
extern int IsGtkOrientable(NspObject *O);
extern NspGtkOrientable *GetGtkOrientableCopy (Stack stack, int i);
extern NspGtkOrientable *GetGtkOrientable (Stack stack, int i);

#endif /* NSP_INC_NspGtkOrientable */ 

#ifdef NspGtkOrientable_Private 
static int init_gtkorientable(NspGtkOrientable *o,NspTypeGtkOrientable *type);
static char *nsp_gtkorientable_type_as_string(void);
static char *nsp_gtkorientable_type_short_string(NspObject *v);
static AttrTab gtkorientable_attrs[];
static NspMethods *gtkorientable_get_methods(void);
/* static int int_gtkorientable_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkOrientable_Private */
