/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTooltips
#define NSP_INC_NspGtkTooltips

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

/* NspGtkTooltips */

#include <nsp/gtk/gtkobject.h>

/*
 * NspGtkTooltips inherits from GtkObject
 * just change some type attributes 
 */

typedef NspGtkObject NspGtkTooltips ;
typedef NspTypeGtkObject NspTypeGtkTooltips ;

extern int nsp_type_gtktooltips_id;
extern NspTypeGtkTooltips *nsp_type_gtktooltips;

/* type instances for gtkobject */

NspTypeGtkTooltips *new_type_gtktooltips(type_mode mode);

/* instance for NspGtkTooltips */

NspGtkTooltips *new_gtktooltips();

/*
 * Object methods redefined for gtktooltips 
 */

#define NULLGTKTOOLTIPS (NspGtkTooltips*) 0


/* from NspGtkTooltipsObj.c */

extern NspGtkTooltips *nsp_gtktooltips_object (NspObject *O);
extern int IsGtkTooltipsObj (Stack stack, int i);
extern int IsGtkTooltips(NspObject *O);
extern NspGtkTooltips *GetGtkTooltipsCopy (Stack stack, int i);
extern NspGtkTooltips *GetGtkTooltips (Stack stack, int i);

#endif /* NSP_INC_NspGtkTooltips */ 

#ifdef NspGtkTooltips_Private 
static int init_gtktooltips(NspGtkTooltips *o,NspTypeGtkTooltips *type);
static char *nsp_gtktooltips_type_as_string(void);
static char *nsp_gtktooltips_type_short_string(NspObject *v);
static AttrTab gtktooltips_attrs[];
static NspMethods *gtktooltips_get_methods(void);
/* static int int_gtktooltips_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTooltips_Private */
