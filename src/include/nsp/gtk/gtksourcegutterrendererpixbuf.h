/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceGutterRendererPixbuf
#define NSP_INC_NspGtkSourceGutterRendererPixbuf

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

/* NspGtkSourceGutterRendererPixbuf */

#include <nsp/gtk/gtksourcegutterrenderer.h>

/*
 * NspGtkSourceGutterRendererPixbuf inherits from GtkSourceGutterRenderer
 * just change some type attributes 
 */

typedef NspGtkSourceGutterRenderer NspGtkSourceGutterRendererPixbuf ;
typedef NspTypeGtkSourceGutterRenderer NspTypeGtkSourceGutterRendererPixbuf ;

extern int nsp_type_gtksourcegutterrendererpixbuf_id;
extern NspTypeGtkSourceGutterRendererPixbuf *nsp_type_gtksourcegutterrendererpixbuf;

/* type instances for gtksourcegutterrenderer */

NspTypeGtkSourceGutterRendererPixbuf *new_type_gtksourcegutterrendererpixbuf(type_mode mode);

/* instance for NspGtkSourceGutterRendererPixbuf */

NspGtkSourceGutterRendererPixbuf *new_gtksourcegutterrendererpixbuf();

/*
 * Object methods redefined for gtksourcegutterrendererpixbuf 
 */

#define NULLGTKSOURCEGUTTERRENDERERPIXBUF (NspGtkSourceGutterRendererPixbuf*) 0


/* from NspGtkSourceGutterRendererPixbufObj.c */

extern NspGtkSourceGutterRendererPixbuf *nsp_gtksourcegutterrendererpixbuf_object (NspObject *O);
extern int IsGtkSourceGutterRendererPixbufObj (Stack stack, int i);
extern int IsGtkSourceGutterRendererPixbuf(NspObject *O);
extern NspGtkSourceGutterRendererPixbuf *GetGtkSourceGutterRendererPixbufCopy (Stack stack, int i);
extern NspGtkSourceGutterRendererPixbuf *GetGtkSourceGutterRendererPixbuf (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceGutterRendererPixbuf */ 

#ifdef NspGtkSourceGutterRendererPixbuf_Private 
static int init_gtksourcegutterrendererpixbuf(NspGtkSourceGutterRendererPixbuf *o,NspTypeGtkSourceGutterRendererPixbuf *type);
static char *nsp_gtksourcegutterrendererpixbuf_type_as_string(void);
static char *nsp_gtksourcegutterrendererpixbuf_type_short_string(NspObject *v);
static AttrTab gtksourcegutterrendererpixbuf_attrs[];
static NspMethods *gtksourcegutterrendererpixbuf_get_methods(void);
/* static int int_gtksourcegutterrendererpixbuf_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceGutterRendererPixbuf_Private */
