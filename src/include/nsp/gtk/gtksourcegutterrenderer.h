/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceGutterRenderer
#define NSP_INC_NspGtkSourceGutterRenderer

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

/* NspGtkSourceGutterRenderer */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkSourceGutterRenderer inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkSourceGutterRenderer ;
typedef NspTypeGObject NspTypeGtkSourceGutterRenderer ;

extern int nsp_type_gtksourcegutterrenderer_id;
extern NspTypeGtkSourceGutterRenderer *nsp_type_gtksourcegutterrenderer;

/* type instances for gobject */

NspTypeGtkSourceGutterRenderer *new_type_gtksourcegutterrenderer(type_mode mode);

/* instance for NspGtkSourceGutterRenderer */

NspGtkSourceGutterRenderer *new_gtksourcegutterrenderer();

/*
 * Object methods redefined for gtksourcegutterrenderer 
 */

#define NULLGTKSOURCEGUTTERRENDERER (NspGtkSourceGutterRenderer*) 0


/* from NspGtkSourceGutterRendererObj.c */

extern NspGtkSourceGutterRenderer *nsp_gtksourcegutterrenderer_object (NspObject *O);
extern int IsGtkSourceGutterRendererObj (Stack stack, int i);
extern int IsGtkSourceGutterRenderer(NspObject *O);
extern NspGtkSourceGutterRenderer *GetGtkSourceGutterRendererCopy (Stack stack, int i);
extern NspGtkSourceGutterRenderer *GetGtkSourceGutterRenderer (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceGutterRenderer */ 

#ifdef NspGtkSourceGutterRenderer_Private 
static int init_gtksourcegutterrenderer(NspGtkSourceGutterRenderer *o,NspTypeGtkSourceGutterRenderer *type);
static char *nsp_gtksourcegutterrenderer_type_as_string(void);
static char *nsp_gtksourcegutterrenderer_type_short_string(NspObject *v);
static AttrTab gtksourcegutterrenderer_attrs[];
static NspMethods *gtksourcegutterrenderer_get_methods(void);
/* static int int_gtksourcegutterrenderer_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceGutterRenderer_Private */
