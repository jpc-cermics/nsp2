/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceGutterRendererText
#define NSP_INC_NspGtkSourceGutterRendererText

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

/* NspGtkSourceGutterRendererText */

#include <nsp/gtk/gtksourcegutterrenderer.h>

/*
 * NspGtkSourceGutterRendererText inherits from GtkSourceGutterRenderer
 * just change some type attributes 
 */

typedef NspGtkSourceGutterRenderer NspGtkSourceGutterRendererText ;
typedef NspTypeGtkSourceGutterRenderer NspTypeGtkSourceGutterRendererText ;

extern int nsp_type_gtksourcegutterrenderertext_id;
extern NspTypeGtkSourceGutterRendererText *nsp_type_gtksourcegutterrenderertext;

/* type instances for gtksourcegutterrenderer */

NspTypeGtkSourceGutterRendererText *new_type_gtksourcegutterrenderertext(type_mode mode);

/* instance for NspGtkSourceGutterRendererText */

NspGtkSourceGutterRendererText *new_gtksourcegutterrenderertext();

/*
 * Object methods redefined for gtksourcegutterrenderertext 
 */

#define NULLGTKSOURCEGUTTERRENDERERTEXT (NspGtkSourceGutterRendererText*) 0


/* from NspGtkSourceGutterRendererTextObj.c */

extern NspGtkSourceGutterRendererText *nsp_gtksourcegutterrenderertext_object (NspObject *O);
extern int IsGtkSourceGutterRendererTextObj (Stack stack, int i);
extern int IsGtkSourceGutterRendererText(NspObject *O);
extern NspGtkSourceGutterRendererText *GetGtkSourceGutterRendererTextCopy (Stack stack, int i);
extern NspGtkSourceGutterRendererText *GetGtkSourceGutterRendererText (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceGutterRendererText */ 

#ifdef NspGtkSourceGutterRendererText_Private 
static int init_gtksourcegutterrenderertext(NspGtkSourceGutterRendererText *o,NspTypeGtkSourceGutterRendererText *type);
static char *nsp_gtksourcegutterrenderertext_type_as_string(void);
static char *nsp_gtksourcegutterrenderertext_type_short_string(NspObject *v);
static AttrTab gtksourcegutterrenderertext_attrs[];
static NspMethods *gtksourcegutterrenderertext_get_methods(void);
/* static int int_gtksourcegutterrenderertext_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceGutterRendererText_Private */
