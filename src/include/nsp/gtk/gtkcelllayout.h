/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkCellLayout
#define NSP_INC_NspGtkCellLayout

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

/* NspGtkCellLayout */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkCellLayout inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkCellLayout ;
typedef NspTypeGObject NspTypeGtkCellLayout ;

extern int nsp_type_gtkcelllayout_id;
extern NspTypeGtkCellLayout *nsp_type_gtkcelllayout;

/* type instances for gobject */

NspTypeGtkCellLayout *new_type_gtkcelllayout(type_mode mode);

/* instance for NspGtkCellLayout */

NspGtkCellLayout *new_gtkcelllayout();

/*
 * Object methods redefined for gtkcelllayout 
 */

#define NULLGTKCELLLAYOUT (NspGtkCellLayout*) 0


/* from NspGtkCellLayoutObj.c */

extern NspGtkCellLayout *nsp_gtkcelllayout_object (NspObject *O);
extern int IsGtkCellLayoutObj (Stack stack, int i);
extern int IsGtkCellLayout(NspObject *O);
extern NspGtkCellLayout *GetGtkCellLayoutCopy (Stack stack, int i);
extern NspGtkCellLayout *GetGtkCellLayout (Stack stack, int i);

#endif /* NSP_INC_NspGtkCellLayout */ 

#ifdef NspGtkCellLayout_Private 
static int init_gtkcelllayout(NspGtkCellLayout *o,NspTypeGtkCellLayout *type);
static char *nsp_gtkcelllayout_type_as_string(void);
static char *nsp_gtkcelllayout_type_short_string(NspObject *v);
static AttrTab gtkcelllayout_attrs[];
static NspMethods *gtkcelllayout_get_methods(void);
/* static int int_gtkcelllayout_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkCellLayout_Private */
