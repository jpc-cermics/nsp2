/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkApplicationWindow
#define NSP_INC_NspGtkApplicationWindow

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

/* NspGtkApplicationWindow */

#include <nsp/gtk/gtkwindow.h>

/*
 * NspGtkApplicationWindow inherits from GtkWindow
 * just change some type attributes 
 */

typedef NspGtkWindow NspGtkApplicationWindow ;
typedef NspTypeGtkWindow NspTypeGtkApplicationWindow ;

extern int nsp_type_gtkapplicationwindow_id;
extern NspTypeGtkApplicationWindow *nsp_type_gtkapplicationwindow;

/* type instances for gtkwindow */

NspTypeGtkApplicationWindow *new_type_gtkapplicationwindow(type_mode mode);

/* instance for NspGtkApplicationWindow */

NspGtkApplicationWindow *new_gtkapplicationwindow();

/*
 * Object methods redefined for gtkapplicationwindow 
 */

#define NULLGTKAPPLICATIONWINDOW (NspGtkApplicationWindow*) 0


/* from NspGtkApplicationWindowObj.c */

extern NspGtkApplicationWindow *nsp_gtkapplicationwindow_object (NspObject *O);
extern int IsGtkApplicationWindowObj (Stack stack, int i);
extern int IsGtkApplicationWindow(NspObject *O);
extern NspGtkApplicationWindow *GetGtkApplicationWindowCopy (Stack stack, int i);
extern NspGtkApplicationWindow *GetGtkApplicationWindow (Stack stack, int i);

#endif /* NSP_INC_NspGtkApplicationWindow */ 

#ifdef NspGtkApplicationWindow_Private 
static int init_gtkapplicationwindow(NspGtkApplicationWindow *o,NspTypeGtkApplicationWindow *type);
static char *nsp_gtkapplicationwindow_type_as_string(void);
static char *nsp_gtkapplicationwindow_type_short_string(NspObject *v);
static AttrTab gtkapplicationwindow_attrs[];
static NspMethods *gtkapplicationwindow_get_methods(void);
/* static int int_gtkapplicationwindow_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkApplicationWindow_Private */
