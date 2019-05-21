/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkTooltip
#define NSP_INC_NspGtkTooltip

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

/* NspGtkTooltip */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkTooltip inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkTooltip ;
typedef NspTypeGObject NspTypeGtkTooltip ;

extern int nsp_type_gtktooltip_id;
extern NspTypeGtkTooltip *nsp_type_gtktooltip;

/* type instances for gobject */

NspTypeGtkTooltip *new_type_gtktooltip(type_mode mode);

/* instance for NspGtkTooltip */

NspGtkTooltip *new_gtktooltip();

/*
 * Object methods redefined for gtktooltip 
 */

#define NULLGTKTOOLTIP (NspGtkTooltip*) 0


/* from NspGtkTooltipObj.c */

extern NspGtkTooltip *nsp_gtktooltip_object (NspObject *O);
extern int IsGtkTooltipObj (Stack stack, int i);
extern int IsGtkTooltip(NspObject *O);
extern NspGtkTooltip *GetGtkTooltipCopy (Stack stack, int i);
extern NspGtkTooltip *GetGtkTooltip (Stack stack, int i);

#endif /* NSP_INC_NspGtkTooltip */ 

#ifdef NspGtkTooltip_Private 
static int init_gtktooltip(NspGtkTooltip *o,NspTypeGtkTooltip *type);
static char *nsp_gtktooltip_type_as_string(void);
static char *nsp_gtktooltip_type_short_string(NspObject *v);
static AttrTab gtktooltip_attrs[];
static NspMethods *gtktooltip_get_methods(void);
/* static int int_gtktooltip_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkTooltip_Private */
