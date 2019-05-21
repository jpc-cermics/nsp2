/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkVScrollbar
#define NSP_INC_NspGtkVScrollbar

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

/* NspGtkVScrollbar */

#include <nsp/gtk/gtkscrollbar.h>

/*
 * NspGtkVScrollbar inherits from GtkScrollbar
 * just change some type attributes 
 */

typedef NspGtkScrollbar NspGtkVScrollbar ;
typedef NspTypeGtkScrollbar NspTypeGtkVScrollbar ;

extern int nsp_type_gtkvscrollbar_id;
extern NspTypeGtkVScrollbar *nsp_type_gtkvscrollbar;

/* type instances for gtkscrollbar */

NspTypeGtkVScrollbar *new_type_gtkvscrollbar(type_mode mode);

/* instance for NspGtkVScrollbar */

NspGtkVScrollbar *new_gtkvscrollbar();

/*
 * Object methods redefined for gtkvscrollbar 
 */

#define NULLGTKVSCROLLBAR (NspGtkVScrollbar*) 0


/* from NspGtkVScrollbarObj.c */

extern NspGtkVScrollbar *nsp_gtkvscrollbar_object (NspObject *O);
extern int IsGtkVScrollbarObj (Stack stack, int i);
extern int IsGtkVScrollbar(NspObject *O);
extern NspGtkVScrollbar *GetGtkVScrollbarCopy (Stack stack, int i);
extern NspGtkVScrollbar *GetGtkVScrollbar (Stack stack, int i);

#endif /* NSP_INC_NspGtkVScrollbar */ 

#ifdef NspGtkVScrollbar_Private 
static int init_gtkvscrollbar(NspGtkVScrollbar *o,NspTypeGtkVScrollbar *type);
static char *nsp_gtkvscrollbar_type_as_string(void);
static char *nsp_gtkvscrollbar_type_short_string(NspObject *v);
static AttrTab gtkvscrollbar_attrs[];
static NspMethods *gtkvscrollbar_get_methods(void);
/* static int int_gtkvscrollbar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkVScrollbar_Private */
