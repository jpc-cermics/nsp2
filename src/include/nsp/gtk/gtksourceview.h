/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkSourceView
#define NSP_INC_NspGtkSourceView

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

/* NspGtkSourceView */

#include <nsp/gtk/gtktextview.h>

/*
 * NspGtkSourceView inherits from GtkTextView
 * just change some type attributes 
 */

typedef NspGtkTextView NspGtkSourceView ;
typedef NspTypeGtkTextView NspTypeGtkSourceView ;

extern int nsp_type_gtksourceview_id;
extern NspTypeGtkSourceView *nsp_type_gtksourceview;

/* type instances for gtktextview */

NspTypeGtkSourceView *new_type_gtksourceview(type_mode mode);

/* instance for NspGtkSourceView */

NspGtkSourceView *new_gtksourceview();

/*
 * Object methods redefined for gtksourceview 
 */

#define NULLGTKSOURCEVIEW (NspGtkSourceView*) 0


/* from NspGtkSourceViewObj.c */

extern NspGtkSourceView *nsp_gtksourceview_object (NspObject *O);
extern int IsGtkSourceViewObj (Stack stack, int i);
extern int IsGtkSourceView(NspObject *O);
extern NspGtkSourceView *GetGtkSourceViewCopy (Stack stack, int i);
extern NspGtkSourceView *GetGtkSourceView (Stack stack, int i);

#endif /* NSP_INC_NspGtkSourceView */ 

#ifdef NspGtkSourceView_Private 
static int init_gtksourceview(NspGtkSourceView *o,NspTypeGtkSourceView *type);
static char *nsp_gtksourceview_type_as_string(void);
static char *nsp_gtksourceview_type_short_string(NspObject *v);
static AttrTab gtksourceview_attrs[];
static NspMethods *gtksourceview_get_methods(void);
/* static int int_gtksourceview_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkSourceView_Private */
