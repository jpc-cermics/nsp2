/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkFileChooser
#define NSP_INC_NspGtkFileChooser

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

/* NspGtkFileChooser */

#include <nsp/gtk/gobject.h>

/*
 * NspGtkFileChooser inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGtkFileChooser ;
typedef NspTypeGObject NspTypeGtkFileChooser ;

extern int nsp_type_gtkfilechooser_id;
extern NspTypeGtkFileChooser *nsp_type_gtkfilechooser;

/* type instances for gobject */

NspTypeGtkFileChooser *new_type_gtkfilechooser(type_mode mode);

/* instance for NspGtkFileChooser */

NspGtkFileChooser *new_gtkfilechooser();

/*
 * Object methods redefined for gtkfilechooser 
 */

#define NULLGTKFILECHOOSER (NspGtkFileChooser*) 0


/* from NspGtkFileChooserObj.c */

extern NspGtkFileChooser *nsp_gtkfilechooser_object (NspObject *O);
extern int IsGtkFileChooserObj (Stack stack, int i);
extern int IsGtkFileChooser(NspObject *O);
extern NspGtkFileChooser *GetGtkFileChooserCopy (Stack stack, int i);
extern NspGtkFileChooser *GetGtkFileChooser (Stack stack, int i);

#endif /* NSP_INC_NspGtkFileChooser */ 

#ifdef NspGtkFileChooser_Private 
static int init_gtkfilechooser(NspGtkFileChooser *o,NspTypeGtkFileChooser *type);
static char *nsp_gtkfilechooser_type_as_string(void);
static char *nsp_gtkfilechooser_type_short_string(NspObject *v);
static AttrTab gtkfilechooser_attrs[];
static NspMethods *gtkfilechooser_get_methods(void);
/* static int int_gtkfilechooser_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkFileChooser_Private */
