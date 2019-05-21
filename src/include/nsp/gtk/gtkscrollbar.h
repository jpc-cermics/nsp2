/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkScrollbar
#define NSP_INC_NspGtkScrollbar

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

/* NspGtkScrollbar */

#include <nsp/gtk/gtkrange.h>

/*
 * NspGtkScrollbar inherits from GtkRange
 * just change some type attributes 
 */

typedef NspGtkRange NspGtkScrollbar ;
typedef NspTypeGtkRange NspTypeGtkScrollbar ;

extern int nsp_type_gtkscrollbar_id;
extern NspTypeGtkScrollbar *nsp_type_gtkscrollbar;

/* type instances for gtkrange */

NspTypeGtkScrollbar *new_type_gtkscrollbar(type_mode mode);

/* instance for NspGtkScrollbar */

NspGtkScrollbar *new_gtkscrollbar();

/*
 * Object methods redefined for gtkscrollbar 
 */

#define NULLGTKSCROLLBAR (NspGtkScrollbar*) 0


/* from NspGtkScrollbarObj.c */

extern NspGtkScrollbar *nsp_gtkscrollbar_object (NspObject *O);
extern int IsGtkScrollbarObj (Stack stack, int i);
extern int IsGtkScrollbar(NspObject *O);
extern NspGtkScrollbar *GetGtkScrollbarCopy (Stack stack, int i);
extern NspGtkScrollbar *GetGtkScrollbar (Stack stack, int i);

#endif /* NSP_INC_NspGtkScrollbar */ 

#ifdef NspGtkScrollbar_Private 
static int init_gtkscrollbar(NspGtkScrollbar *o,NspTypeGtkScrollbar *type);
static char *nsp_gtkscrollbar_type_as_string(void);
static char *nsp_gtkscrollbar_type_short_string(NspObject *v);
static AttrTab gtkscrollbar_attrs[];
static NspMethods *gtkscrollbar_get_methods(void);
/* static int int_gtkscrollbar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkScrollbar_Private */
