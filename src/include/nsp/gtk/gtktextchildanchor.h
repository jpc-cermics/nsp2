/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTextChildAnchor
#define NSP_INC_NspGtkTextChildAnchor

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

/* NspGtkTextChildAnchor */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTextChildAnchor inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTextChildAnchor ;
typedef NspTypeGObject NspTypeGtkTextChildAnchor ;

extern int nsp_type_gtktextchildanchor_id;
extern NspTypeGtkTextChildAnchor *nsp_type_gtktextchildanchor;

/* type instances for gobject */

NspTypeGtkTextChildAnchor *new_type_gtktextchildanchor(type_mode mode);

/* instance for NspGtkTextChildAnchor */

NspGtkTextChildAnchor *new_gtktextchildanchor();

/*
 * Object methods redefined for gtktextchildanchor 
 */

#define NULLGTKTEXTCHILDANCHOR (NspGtkTextChildAnchor*) 0


/* from NspGtkTextChildAnchorObj.c */

extern NspGtkTextChildAnchor *nsp_gtktextchildanchor_object (NspObject *O);
extern int IsGtkTextChildAnchorObj (Stack stack, int i);
extern int IsGtkTextChildAnchor(NspObject *O);
extern NspGtkTextChildAnchor *GetGtkTextChildAnchorCopy (Stack stack, int i);
extern NspGtkTextChildAnchor *GetGtkTextChildAnchor (Stack stack, int i);

#endif /* NSP_INC_NspGtkTextChildAnchor */ 

#ifdef NspGtkTextChildAnchor_Private 
static int init_gtktextchildanchor(NspGtkTextChildAnchor *o,NspTypeGtkTextChildAnchor *type);
static char *nsp_gtktextchildanchor_type_as_string(void);
static char *nsp_gtktextchildanchor_type_short_string(NspObject *v);
static AttrTab gtktextchildanchor_attrs[];
static NspMethods *gtktextchildanchor_get_methods(void);
/* static int int_gtktextchildanchor_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTextChildAnchor_Private */
