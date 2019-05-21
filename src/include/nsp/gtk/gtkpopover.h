/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkPopover
#define NSP_INC_NspGtkPopover

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

/* NspGtkPopover */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkPopover inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkPopover ;
typedef NspTypeGtkBin NspTypeGtkPopover ;

extern int nsp_type_gtkpopover_id;
extern NspTypeGtkPopover *nsp_type_gtkpopover;

/* type instances for gtkbin */

NspTypeGtkPopover *new_type_gtkpopover(type_mode mode);

/* instance for NspGtkPopover */

NspGtkPopover *new_gtkpopover();

/*
 * Object methods redefined for gtkpopover 
 */

#define NULLGTKPOPOVER (NspGtkPopover*) 0


/* from NspGtkPopoverObj.c */

extern NspGtkPopover *nsp_gtkpopover_object (NspObject *O);
extern int IsGtkPopoverObj (Stack stack, int i);
extern int IsGtkPopover(NspObject *O);
extern NspGtkPopover *GetGtkPopoverCopy (Stack stack, int i);
extern NspGtkPopover *GetGtkPopover (Stack stack, int i);

#endif /* NSP_INC_NspGtkPopover */ 

#ifdef NspGtkPopover_Private 
static int init_gtkpopover(NspGtkPopover *o,NspTypeGtkPopover *type);
static char *nsp_gtkpopover_type_as_string(void);
static char *nsp_gtkpopover_type_short_string(NspObject *v);
static AttrTab gtkpopover_attrs[];
static NspMethods *gtkpopover_get_methods(void);
/* static int int_gtkpopover_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkPopover_Private */
