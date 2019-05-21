/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkViewport
#define NSP_INC_NspGtkViewport

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

/* NspGtkViewport */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkViewport inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkViewport ;
typedef NspTypeGtkBin NspTypeGtkViewport ;

extern int nsp_type_gtkviewport_id;
extern NspTypeGtkViewport *nsp_type_gtkviewport;

/* type instances for gtkbin */

NspTypeGtkViewport *new_type_gtkviewport(type_mode mode);

/* instance for NspGtkViewport */

NspGtkViewport *new_gtkviewport();

/*
 * Object methods redefined for gtkviewport 
 */

#define NULLGTKVIEWPORT (NspGtkViewport*) 0


/* from NspGtkViewportObj.c */

extern NspGtkViewport *nsp_gtkviewport_object (NspObject *O);
extern int IsGtkViewportObj (Stack stack, int i);
extern int IsGtkViewport(NspObject *O);
extern NspGtkViewport *GetGtkViewportCopy (Stack stack, int i);
extern NspGtkViewport *GetGtkViewport (Stack stack, int i);

#endif /* NSP_INC_NspGtkViewport */ 

#ifdef NspGtkViewport_Private 
static int init_gtkviewport(NspGtkViewport *o,NspTypeGtkViewport *type);
static char *nsp_gtkviewport_type_as_string(void);
static char *nsp_gtkviewport_type_short_string(NspObject *v);
static AttrTab gtkviewport_attrs[];
static NspMethods *gtkviewport_get_methods(void);
/* static int int_gtkviewport_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkViewport_Private */
