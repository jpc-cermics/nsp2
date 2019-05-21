/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkIconView
#define NSP_INC_NspGtkIconView

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

/* NspGtkIconView */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkIconView inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkIconView ;
typedef NspTypeGtkContainer NspTypeGtkIconView ;

extern int nsp_type_gtkiconview_id;
extern NspTypeGtkIconView *nsp_type_gtkiconview;

/* type instances for gtkcontainer */

NspTypeGtkIconView *new_type_gtkiconview(type_mode mode);

/* instance for NspGtkIconView */

NspGtkIconView *new_gtkiconview();

/*
 * Object methods redefined for gtkiconview 
 */

#define NULLGTKICONVIEW (NspGtkIconView*) 0


/* from NspGtkIconViewObj.c */

extern NspGtkIconView *nsp_gtkiconview_object (NspObject *O);
extern int IsGtkIconViewObj (Stack stack, int i);
extern int IsGtkIconView(NspObject *O);
extern NspGtkIconView *GetGtkIconViewCopy (Stack stack, int i);
extern NspGtkIconView *GetGtkIconView (Stack stack, int i);

#endif /* NSP_INC_NspGtkIconView */ 

#ifdef NspGtkIconView_Private 
static int init_gtkiconview(NspGtkIconView *o,NspTypeGtkIconView *type);
static char *nsp_gtkiconview_type_as_string(void);
static char *nsp_gtkiconview_type_short_string(NspObject *v);
static AttrTab gtkiconview_attrs[];
static NspMethods *gtkiconview_get_methods(void);
/* static int int_gtkiconview_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkIconView_Private */
