/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceGutter
#define NSP_INC_NspGtkSourceGutter

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

/* NspGtkSourceGutter */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceGutter inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceGutter ;
typedef NspTypeGObject NspTypeGtkSourceGutter ;

extern int nsp_type_gtksourcegutter_id;
extern NspTypeGtkSourceGutter *nsp_type_gtksourcegutter;

/* type instances for gobject */

NspTypeGtkSourceGutter *new_type_gtksourcegutter(type_mode mode);

/* instance for NspGtkSourceGutter */

NspGtkSourceGutter *new_gtksourcegutter();

/*
 * Object methods redefined for gtksourcegutter 
 */

#define NULLGTKSOURCEGUTTER (NspGtkSourceGutter*) 0


/* from NspGtkSourceGutterObj.c */

extern NspGtkSourceGutter *nsp_gtksourcegutter_object (NspObject *O);
extern int IsGtkSourceGutterObj (Stack stack, int i);
extern int IsGtkSourceGutter(NspObject *O);
extern NspGtkSourceGutter *GetGtkSourceGutterCopy (Stack stack, int i);
extern NspGtkSourceGutter *GetGtkSourceGutter (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceGutter */ 

#ifdef NspGtkSourceGutter_Private 
static int init_gtksourcegutter(NspGtkSourceGutter *o,NspTypeGtkSourceGutter *type);
static char *nsp_gtksourcegutter_type_as_string(void);
static char *nsp_gtksourcegutter_type_short_string(NspObject *v);
static AttrTab gtksourcegutter_attrs[];
static NspMethods *gtksourcegutter_get_methods(void);
/* static int int_gtksourcegutter_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceGutter_Private */
