/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkHScrollbar
#define NSP_INC_NspGtkHScrollbar

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

/* NspGtkHScrollbar */

#include <nsp/gtk/gtkscrollbar.h>

/*
 * NspGtkHScrollbar inherits from GtkScrollbar
 * just change some type attributes 
 */

typedef NspGtkScrollbar NspGtkHScrollbar ;
typedef NspTypeGtkScrollbar NspTypeGtkHScrollbar ;

extern int nsp_type_gtkhscrollbar_id;
extern NspTypeGtkHScrollbar *nsp_type_gtkhscrollbar;

/* type instances for gtkscrollbar */

NspTypeGtkHScrollbar *new_type_gtkhscrollbar(type_mode mode);

/* instance for NspGtkHScrollbar */

NspGtkHScrollbar *new_gtkhscrollbar();

/*
 * Object methods redefined for gtkhscrollbar 
 */

#define NULLGTKHSCROLLBAR (NspGtkHScrollbar*) 0


/* from NspGtkHScrollbarObj.c */

extern NspGtkHScrollbar *nsp_gtkhscrollbar_object (NspObject *O);
extern int IsGtkHScrollbarObj (Stack stack, int i);
extern int IsGtkHScrollbar(NspObject *O);
extern NspGtkHScrollbar *GetGtkHScrollbarCopy (Stack stack, int i);
extern NspGtkHScrollbar *GetGtkHScrollbar (Stack stack, int i);

#endif /* NSP_INC_NspGtkHScrollbar */ 

#ifdef NspGtkHScrollbar_Private 
static int init_gtkhscrollbar(NspGtkHScrollbar *o,NspTypeGtkHScrollbar *type);
static char *nsp_gtkhscrollbar_type_as_string(void);
static char *nsp_gtkhscrollbar_type_short_string(NspObject *v);
static AttrTab gtkhscrollbar_attrs[];
static NspMethods *gtkhscrollbar_get_methods(void);
/* static int int_gtkhscrollbar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkHScrollbar_Private */
