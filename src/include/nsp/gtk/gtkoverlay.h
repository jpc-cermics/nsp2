/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkOverlay
#define NSP_INC_NspGtkOverlay

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

/* NspGtkOverlay */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkOverlay inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkOverlay ;
typedef NspTypeGtkBin NspTypeGtkOverlay ;

extern int nsp_type_gtkoverlay_id;
extern NspTypeGtkOverlay *nsp_type_gtkoverlay;

/* type instances for gtkbin */

NspTypeGtkOverlay *new_type_gtkoverlay(type_mode mode);

/* instance for NspGtkOverlay */

NspGtkOverlay *new_gtkoverlay();

/*
 * Object methods redefined for gtkoverlay 
 */

#define NULLGTKOVERLAY (NspGtkOverlay*) 0


/* from NspGtkOverlayObj.c */

extern NspGtkOverlay *nsp_gtkoverlay_object (NspObject *O);
extern int IsGtkOverlayObj (Stack stack, int i);
extern int IsGtkOverlay(NspObject *O);
extern NspGtkOverlay *GetGtkOverlayCopy (Stack stack, int i);
extern NspGtkOverlay *GetGtkOverlay (Stack stack, int i);

#endif /* NSP_INC_NspGtkOverlay */ 

#ifdef NspGtkOverlay_Private 
static int init_gtkoverlay(NspGtkOverlay *o,NspTypeGtkOverlay *type);
static char *nsp_gtkoverlay_type_as_string(void);
static char *nsp_gtkoverlay_type_short_string(NspObject *v);
static AttrTab gtkoverlay_attrs[];
static NspMethods *gtkoverlay_get_methods(void);
/* static int int_gtkoverlay_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkOverlay_Private */
