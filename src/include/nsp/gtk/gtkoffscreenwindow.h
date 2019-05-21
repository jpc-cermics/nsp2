/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkOffscreenWindow
#define NSP_INC_NspGtkOffscreenWindow

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

/* NspGtkOffscreenWindow */

#include <nsp/gtk/gtkwindow.h>

/*
 * NspGtkOffscreenWindow inherits from GtkWindow
 * just change some type attributes 
 */

typedef NspGtkWindow NspGtkOffscreenWindow ;
typedef NspTypeGtkWindow NspTypeGtkOffscreenWindow ;

extern int nsp_type_gtkoffscreenwindow_id;
extern NspTypeGtkOffscreenWindow *nsp_type_gtkoffscreenwindow;

/* type instances for gtkwindow */

NspTypeGtkOffscreenWindow *new_type_gtkoffscreenwindow(type_mode mode);

/* instance for NspGtkOffscreenWindow */

NspGtkOffscreenWindow *new_gtkoffscreenwindow();

/*
 * Object methods redefined for gtkoffscreenwindow 
 */

#define NULLGTKOFFSCREENWINDOW (NspGtkOffscreenWindow*) 0


/* from NspGtkOffscreenWindowObj.c */

extern NspGtkOffscreenWindow *nsp_gtkoffscreenwindow_object (NspObject *O);
extern int IsGtkOffscreenWindowObj (Stack stack, int i);
extern int IsGtkOffscreenWindow(NspObject *O);
extern NspGtkOffscreenWindow *GetGtkOffscreenWindowCopy (Stack stack, int i);
extern NspGtkOffscreenWindow *GetGtkOffscreenWindow (Stack stack, int i);

#endif /* NSP_INC_NspGtkOffscreenWindow */ 

#ifdef NspGtkOffscreenWindow_Private 
static int init_gtkoffscreenwindow(NspGtkOffscreenWindow *o,NspTypeGtkOffscreenWindow *type);
static char *nsp_gtkoffscreenwindow_type_as_string(void);
static char *nsp_gtkoffscreenwindow_type_short_string(NspObject *v);
static AttrTab gtkoffscreenwindow_attrs[];
static NspMethods *gtkoffscreenwindow_get_methods(void);
/* static int int_gtkoffscreenwindow_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkOffscreenWindow_Private */
