/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceGutterRendererMarks
#define NSP_INC_NspGtkSourceGutterRendererMarks

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

/* NspGtkSourceGutterRendererMarks */

#include <nsp/gtk/gtksourcegutterrendererpixbuf.h>

/*
 * NspGtkSourceGutterRendererMarks inherits from GtkSourceGutterRendererPixbuf
 * just change some type attributes 
 */

typedef NspGtkSourceGutterRendererPixbuf NspGtkSourceGutterRendererMarks ;
typedef NspTypeGtkSourceGutterRendererPixbuf NspTypeGtkSourceGutterRendererMarks ;

extern int nsp_type_gtksourcegutterrenderermarks_id;
extern NspTypeGtkSourceGutterRendererMarks *nsp_type_gtksourcegutterrenderermarks;

/* type instances for gtksourcegutterrendererpixbuf */

NspTypeGtkSourceGutterRendererMarks *new_type_gtksourcegutterrenderermarks(type_mode mode);

/* instance for NspGtkSourceGutterRendererMarks */

NspGtkSourceGutterRendererMarks *new_gtksourcegutterrenderermarks();

/*
 * Object methods redefined for gtksourcegutterrenderermarks 
 */

#define NULLGTKSOURCEGUTTERRENDERERMARKS (NspGtkSourceGutterRendererMarks*) 0


/* from NspGtkSourceGutterRendererMarksObj.c */

extern NspGtkSourceGutterRendererMarks *nsp_gtksourcegutterrenderermarks_object (NspObject *O);
extern int IsGtkSourceGutterRendererMarksObj (Stack stack, int i);
extern int IsGtkSourceGutterRendererMarks(NspObject *O);
extern NspGtkSourceGutterRendererMarks *GetGtkSourceGutterRendererMarksCopy (Stack stack, int i);
extern NspGtkSourceGutterRendererMarks *GetGtkSourceGutterRendererMarks (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceGutterRendererMarks */ 

#ifdef NspGtkSourceGutterRendererMarks_Private 
static int init_gtksourcegutterrenderermarks(NspGtkSourceGutterRendererMarks *o,NspTypeGtkSourceGutterRendererMarks *type);
static char *nsp_gtksourcegutterrenderermarks_type_as_string(void);
static char *nsp_gtksourcegutterrenderermarks_type_short_string(NspObject *v);
static AttrTab gtksourcegutterrenderermarks_attrs[];
static NspMethods *gtksourcegutterrenderermarks_get_methods(void);
/* static int int_gtksourcegutterrenderermarks_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceGutterRendererMarks_Private */
