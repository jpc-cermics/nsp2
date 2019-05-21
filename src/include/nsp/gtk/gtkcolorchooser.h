/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkColorChooser
#define NSP_INC_NspGtkColorChooser

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

/* NspGtkColorChooser */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkColorChooser inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkColorChooser ;
typedef NspTypeGObject NspTypeGtkColorChooser ;

extern int nsp_type_gtkcolorchooser_id;
extern NspTypeGtkColorChooser *nsp_type_gtkcolorchooser;

/* type instances for gobject */

NspTypeGtkColorChooser *new_type_gtkcolorchooser(type_mode mode);

/* instance for NspGtkColorChooser */

NspGtkColorChooser *new_gtkcolorchooser();

/*
 * Object methods redefined for gtkcolorchooser 
 */

#define NULLGTKCOLORCHOOSER (NspGtkColorChooser*) 0


/* from NspGtkColorChooserObj.c */

extern NspGtkColorChooser *nsp_gtkcolorchooser_object (NspObject *O);
extern int IsGtkColorChooserObj (Stack stack, int i);
extern int IsGtkColorChooser(NspObject *O);
extern NspGtkColorChooser *GetGtkColorChooserCopy (Stack stack, int i);
extern NspGtkColorChooser *GetGtkColorChooser (Stack stack, int i);

#endif /* NSP_INC_NspGtkColorChooser */ 

#ifdef NspGtkColorChooser_Private 
static int init_gtkcolorchooser(NspGtkColorChooser *o,NspTypeGtkColorChooser *type);
static char *nsp_gtkcolorchooser_type_as_string(void);
static char *nsp_gtkcolorchooser_type_short_string(NspObject *v);
static AttrTab gtkcolorchooser_attrs[];
static NspMethods *gtkcolorchooser_get_methods(void);
/* static int int_gtkcolorchooser_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkColorChooser_Private */
