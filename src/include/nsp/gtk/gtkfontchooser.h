/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFontChooser
#define NSP_INC_NspGtkFontChooser

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

/* NspGtkFontChooser */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkFontChooser inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkFontChooser ;
typedef NspTypeGObject NspTypeGtkFontChooser ;

extern int nsp_type_gtkfontchooser_id;
extern NspTypeGtkFontChooser *nsp_type_gtkfontchooser;

/* type instances for gobject */

NspTypeGtkFontChooser *new_type_gtkfontchooser(type_mode mode);

/* instance for NspGtkFontChooser */

NspGtkFontChooser *new_gtkfontchooser();

/*
 * Object methods redefined for gtkfontchooser 
 */

#define NULLGTKFONTCHOOSER (NspGtkFontChooser*) 0


/* from NspGtkFontChooserObj.c */

extern NspGtkFontChooser *nsp_gtkfontchooser_object (NspObject *O);
extern int IsGtkFontChooserObj (Stack stack, int i);
extern int IsGtkFontChooser(NspObject *O);
extern NspGtkFontChooser *GetGtkFontChooserCopy (Stack stack, int i);
extern NspGtkFontChooser *GetGtkFontChooser (Stack stack, int i);

#endif /* NSP_INC_NspGtkFontChooser */ 

#ifdef NspGtkFontChooser_Private 
static int init_gtkfontchooser(NspGtkFontChooser *o,NspTypeGtkFontChooser *type);
static char *nsp_gtkfontchooser_type_as_string(void);
static char *nsp_gtkfontchooser_type_short_string(NspObject *v);
static AttrTab gtkfontchooser_attrs[];
static NspMethods *gtkfontchooser_get_methods(void);
/* static int int_gtkfontchooser_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFontChooser_Private */
