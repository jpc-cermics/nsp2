/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkMenuBar
#define NSP_INC_NspGtkMenuBar

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

/* NspGtkMenuBar */

#include <nsp/gtk/gtkmenushell.h>

/*
 * NspGtkMenuBar inherits from GtkMenuShell
 * just change some type attributes 
 */

typedef NspGtkMenuShell NspGtkMenuBar ;
typedef NspTypeGtkMenuShell NspTypeGtkMenuBar ;

extern int nsp_type_gtkmenubar_id;
extern NspTypeGtkMenuBar *nsp_type_gtkmenubar;

/* type instances for gtkmenushell */

NspTypeGtkMenuBar *new_type_gtkmenubar(type_mode mode);

/* instance for NspGtkMenuBar */

NspGtkMenuBar *new_gtkmenubar();

/*
 * Object methods redefined for gtkmenubar 
 */

#define NULLGTKMENUBAR (NspGtkMenuBar*) 0


/* from NspGtkMenuBarObj.c */

extern NspGtkMenuBar *nsp_gtkmenubar_object (NspObject *O);
extern int IsGtkMenuBarObj (Stack stack, int i);
extern int IsGtkMenuBar(NspObject *O);
extern NspGtkMenuBar *GetGtkMenuBarCopy (Stack stack, int i);
extern NspGtkMenuBar *GetGtkMenuBar (Stack stack, int i);

#endif /* NSP_INC_NspGtkMenuBar */ 

#ifdef NspGtkMenuBar_Private 
static int init_gtkmenubar(NspGtkMenuBar *o,NspTypeGtkMenuBar *type);
static char *nsp_gtkmenubar_type_as_string(void);
static char *nsp_gtkmenubar_type_short_string(NspObject *v);
static AttrTab gtkmenubar_attrs[];
static NspMethods *gtkmenubar_get_methods(void);
/* static int int_gtkmenubar_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkMenuBar_Private */
