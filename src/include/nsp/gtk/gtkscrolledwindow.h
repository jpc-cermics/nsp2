/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkScrolledWindow
#define NSP_INC_NspGtkScrolledWindow

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

/* NspGtkScrolledWindow */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkScrolledWindow inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkScrolledWindow ;
typedef NspTypeGtkBin NspTypeGtkScrolledWindow ;

extern int nsp_type_gtkscrolledwindow_id;
extern NspTypeGtkScrolledWindow *nsp_type_gtkscrolledwindow;

/* type instances for gtkbin */

NspTypeGtkScrolledWindow *new_type_gtkscrolledwindow(type_mode mode);

/* instance for NspGtkScrolledWindow */

NspGtkScrolledWindow *new_gtkscrolledwindow();

/*
 * Object methods redefined for gtkscrolledwindow 
 */

#define NULLGTKSCROLLEDWINDOW (NspGtkScrolledWindow*) 0


/* from NspGtkScrolledWindowObj.c */

extern NspGtkScrolledWindow *nsp_gtkscrolledwindow_object (NspObject *O);
extern int IsGtkScrolledWindowObj (Stack stack, int i);
extern int IsGtkScrolledWindow(NspObject *O);
extern NspGtkScrolledWindow *GetGtkScrolledWindowCopy (Stack stack, int i);
extern NspGtkScrolledWindow *GetGtkScrolledWindow (Stack stack, int i);

#endif /* NSP_INC_NspGtkScrolledWindow */ 

#ifdef NspGtkScrolledWindow_Private 
static int init_gtkscrolledwindow(NspGtkScrolledWindow *o,NspTypeGtkScrolledWindow *type);
static char *nsp_gtkscrolledwindow_type_as_string(void);
static char *nsp_gtkscrolledwindow_type_short_string(NspObject *v);
static AttrTab gtkscrolledwindow_attrs[];
static NspMethods *gtkscrolledwindow_get_methods(void);
/* static int int_gtkscrolledwindow_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkScrolledWindow_Private */
