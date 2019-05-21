/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTreeView
#define NSP_INC_NspGtkTreeView

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

/* NspGtkTreeView */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkTreeView inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkTreeView ;
typedef NspTypeGtkContainer NspTypeGtkTreeView ;

extern int nsp_type_gtktreeview_id;
extern NspTypeGtkTreeView *nsp_type_gtktreeview;

/* type instances for gtkcontainer */

NspTypeGtkTreeView *new_type_gtktreeview(type_mode mode);

/* instance for NspGtkTreeView */

NspGtkTreeView *new_gtktreeview();

/*
 * Object methods redefined for gtktreeview 
 */

#define NULLGTKTREEVIEW (NspGtkTreeView*) 0


/* from NspGtkTreeViewObj.c */

extern NspGtkTreeView *nsp_gtktreeview_object (NspObject *O);
extern int IsGtkTreeViewObj (Stack stack, int i);
extern int IsGtkTreeView(NspObject *O);
extern NspGtkTreeView *GetGtkTreeViewCopy (Stack stack, int i);
extern NspGtkTreeView *GetGtkTreeView (Stack stack, int i);

#endif /* NSP_INC_NspGtkTreeView */ 

#ifdef NspGtkTreeView_Private 
static int init_gtktreeview(NspGtkTreeView *o,NspTypeGtkTreeView *type);
static char *nsp_gtktreeview_type_as_string(void);
static char *nsp_gtktreeview_type_short_string(NspObject *v);
static AttrTab gtktreeview_attrs[];
static NspMethods *gtktreeview_get_methods(void);
/* static int int_gtktreeview_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTreeView_Private */
