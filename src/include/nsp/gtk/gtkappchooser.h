/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkAppChooser
#define NSP_INC_NspGtkAppChooser

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

/* NspGtkAppChooser */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkAppChooser inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkAppChooser ;
typedef NspTypeGObject NspTypeGtkAppChooser ;

extern int nsp_type_gtkappchooser_id;
extern NspTypeGtkAppChooser *nsp_type_gtkappchooser;

/* type instances for gobject */

NspTypeGtkAppChooser *new_type_gtkappchooser(type_mode mode);

/* instance for NspGtkAppChooser */

NspGtkAppChooser *new_gtkappchooser();

/*
 * Object methods redefined for gtkappchooser 
 */

#define NULLGTKAPPCHOOSER (NspGtkAppChooser*) 0


/* from NspGtkAppChooserObj.c */

extern NspGtkAppChooser *nsp_gtkappchooser_object (NspObject *O);
extern int IsGtkAppChooserObj (Stack stack, int i);
extern int IsGtkAppChooser(NspObject *O);
extern NspGtkAppChooser *GetGtkAppChooserCopy (Stack stack, int i);
extern NspGtkAppChooser *GetGtkAppChooser (Stack stack, int i);

#endif /* NSP_INC_NspGtkAppChooser */ 

#ifdef NspGtkAppChooser_Private 
static int init_gtkappchooser(NspGtkAppChooser *o,NspTypeGtkAppChooser *type);
static char *nsp_gtkappchooser_type_as_string(void);
static char *nsp_gtkappchooser_type_short_string(NspObject *v);
static AttrTab gtkappchooser_attrs[];
static NspMethods *gtkappchooser_get_methods(void);
/* static int int_gtkappchooser_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkAppChooser_Private */
