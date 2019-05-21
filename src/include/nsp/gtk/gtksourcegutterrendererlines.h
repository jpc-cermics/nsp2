/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceGutterRendererLines
#define NSP_INC_NspGtkSourceGutterRendererLines

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

/* NspGtkSourceGutterRendererLines */

#include <nsp/gtk/gtksourcegutterrenderertext.h>

/*
 * NspGtkSourceGutterRendererLines inherits from GtkSourceGutterRendererText
 * just change some type attributes 
 */

typedef NspGtkSourceGutterRendererText NspGtkSourceGutterRendererLines ;
typedef NspTypeGtkSourceGutterRendererText NspTypeGtkSourceGutterRendererLines ;

extern int nsp_type_gtksourcegutterrendererlines_id;
extern NspTypeGtkSourceGutterRendererLines *nsp_type_gtksourcegutterrendererlines;

/* type instances for gtksourcegutterrenderertext */

NspTypeGtkSourceGutterRendererLines *new_type_gtksourcegutterrendererlines(type_mode mode);

/* instance for NspGtkSourceGutterRendererLines */

NspGtkSourceGutterRendererLines *new_gtksourcegutterrendererlines();

/*
 * Object methods redefined for gtksourcegutterrendererlines 
 */

#define NULLGTKSOURCEGUTTERRENDERERLINES (NspGtkSourceGutterRendererLines*) 0


/* from NspGtkSourceGutterRendererLinesObj.c */

extern NspGtkSourceGutterRendererLines *nsp_gtksourcegutterrendererlines_object (NspObject *O);
extern int IsGtkSourceGutterRendererLinesObj (Stack stack, int i);
extern int IsGtkSourceGutterRendererLines(NspObject *O);
extern NspGtkSourceGutterRendererLines *GetGtkSourceGutterRendererLinesCopy (Stack stack, int i);
extern NspGtkSourceGutterRendererLines *GetGtkSourceGutterRendererLines (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceGutterRendererLines */ 

#ifdef NspGtkSourceGutterRendererLines_Private 
static int init_gtksourcegutterrendererlines(NspGtkSourceGutterRendererLines *o,NspTypeGtkSourceGutterRendererLines *type);
static char *nsp_gtksourcegutterrendererlines_type_as_string(void);
static char *nsp_gtksourcegutterrendererlines_type_short_string(NspObject *v);
static AttrTab gtksourcegutterrendererlines_attrs[];
static NspMethods *gtksourcegutterrendererlines_get_methods(void);
/* static int int_gtksourcegutterrendererlines_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceGutterRendererLines_Private */
