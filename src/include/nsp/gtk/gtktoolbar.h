/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkToolbar
#define NSP_INC_NspGtkToolbar

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

/* NspGtkToolbar */

#include <nsp/gtk/gtkcontainer.h>

/*
 * NspGtkToolbar inherits from GtkContainer
 * just change some type attributes 
 */

typedef NspGtkContainer NspGtkToolbar ;
typedef NspTypeGtkContainer NspTypeGtkToolbar ;

extern int nsp_type_gtktoolbar_id;
extern NspTypeGtkToolbar *nsp_type_gtktoolbar;

/* type instances for gtkcontainer */

NspTypeGtkToolbar *new_type_gtktoolbar(type_mode mode);

/* instance for NspGtkToolbar */

NspGtkToolbar *new_gtktoolbar();

/*
 * Object methods redefined for gtktoolbar 
 */

#define NULLGTKTOOLBAR (NspGtkToolbar*) 0


/* from NspGtkToolbarObj.c */

extern NspGtkToolbar *nsp_gtktoolbar_object (NspObject *O);
extern int IsGtkToolbarObj (Stack stack, int i);
extern int IsGtkToolbar(NspObject *O);
extern NspGtkToolbar *GetGtkToolbarCopy (Stack stack, int i);
extern NspGtkToolbar *GetGtkToolbar (Stack stack, int i);

#endif /* NSP_INC_NspGtkToolbar */ 

#ifdef NspGtkToolbar_Private 
static int init_gtktoolbar(NspGtkToolbar *o,NspTypeGtkToolbar *type);
static char *nsp_gtktoolbar_type_as_string(void);
static char *nsp_gtktoolbar_type_short_string(NspObject *v);
static AttrTab gtktoolbar_attrs[];
static NspMethods *gtktoolbar_get_methods(void);
/* static int int_gtktoolbar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkToolbar_Private */
