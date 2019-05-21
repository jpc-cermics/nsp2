/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkPlug
#define NSP_INC_NspGtkPlug

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

/* NspGtkPlug */

#include <nsp/gtk/gtkwindow.h>

/*
 * NspGtkPlug inherits from GtkWindow
 * just change some type attributes 
 */

typedef NspGtkWindow NspGtkPlug ;
typedef NspTypeGtkWindow NspTypeGtkPlug ;

extern int nsp_type_gtkplug_id;
extern NspTypeGtkPlug *nsp_type_gtkplug;

/* type instances for gtkwindow */

NspTypeGtkPlug *new_type_gtkplug(type_mode mode);

/* instance for NspGtkPlug */

NspGtkPlug *new_gtkplug();

/*
 * Object methods redefined for gtkplug 
 */

#define NULLGTKPLUG (NspGtkPlug*) 0


/* from NspGtkPlugObj.c */

extern NspGtkPlug *nsp_gtkplug_object (NspObject *O);
extern int IsGtkPlugObj (Stack stack, int i);
extern int IsGtkPlug(NspObject *O);
extern NspGtkPlug *GetGtkPlugCopy (Stack stack, int i);
extern NspGtkPlug *GetGtkPlug (Stack stack, int i);

#endif /* NSP_INC_NspGtkPlug */ 

#ifdef NspGtkPlug_Private 
static int init_gtkplug(NspGtkPlug *o,NspTypeGtkPlug *type);
static char *nsp_gtkplug_type_as_string(void);
static char *nsp_gtkplug_type_short_string(NspObject *v);
static AttrTab gtkplug_attrs[];
static NspMethods *gtkplug_get_methods(void);
/* static int int_gtkplug_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkPlug_Private */
