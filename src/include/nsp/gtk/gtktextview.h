/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTextView
#define NSP_INC_NspGtkTextView

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

/* NspGtkTextView */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkTextView inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkTextView ;
typedef NspTypeGtkContainer NspTypeGtkTextView ;

extern int nsp_type_gtktextview_id;
extern NspTypeGtkTextView *nsp_type_gtktextview;

/* type instances for gtkcontainer */

NspTypeGtkTextView *new_type_gtktextview(type_mode mode);

/* instance for NspGtkTextView */

NspGtkTextView *new_gtktextview();

/*
 * Object methods redefined for gtktextview 
 */

#define NULLGTKTEXTVIEW (NspGtkTextView*) 0


/* from NspGtkTextViewObj.c */

extern NspGtkTextView *nsp_gtktextview_object (NspObject *O);
extern int IsGtkTextViewObj (Stack stack, int i);
extern int IsGtkTextView(NspObject *O);
extern NspGtkTextView *GetGtkTextViewCopy (Stack stack, int i);
extern NspGtkTextView *GetGtkTextView (Stack stack, int i);

#endif /* NSP_INC_NspGtkTextView */ 

#ifdef NspGtkTextView_Private 
static int init_gtktextview(NspGtkTextView *o,NspTypeGtkTextView *type);
static char *nsp_gtktextview_type_as_string(void);
static char *nsp_gtktextview_type_short_string(NspObject *v);
static AttrTab gtktextview_attrs[];
static NspMethods *gtktextview_get_methods(void);
/* static int int_gtktextview_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTextView_Private */
