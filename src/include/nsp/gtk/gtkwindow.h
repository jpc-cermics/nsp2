/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGtkWindow
#define NSP_INC_NspGtkWindow

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

/* NspGtkWindow */

#include <nsp/gtk/gtkbin.h>

/*
 * NspGtkWindow inherits from GtkBin
 * just change some type attributes 
 */

typedef NspGtkBin NspGtkWindow ;
typedef NspTypeGtkBin NspTypeGtkWindow ;

extern int nsp_type_gtkwindow_id;
extern NspTypeGtkWindow *nsp_type_gtkwindow;

/* type instances for gtkbin */

NspTypeGtkWindow *new_type_gtkwindow(type_mode mode);

/* instance for NspGtkWindow */

NspGtkWindow *new_gtkwindow();

/*
 * Object methods redefined for gtkwindow 
 */

#define NULLGTKWINDOW (NspGtkWindow*) 0


/* from NspGtkWindowObj.c */

extern NspGtkWindow *nsp_gtkwindow_object (NspObject *O);
extern int IsGtkWindowObj (Stack stack, int i);
extern int IsGtkWindow(NspObject *O);
extern NspGtkWindow *GetGtkWindowCopy (Stack stack, int i);
extern NspGtkWindow *GetGtkWindow (Stack stack, int i);

#endif /* NSP_INC_NspGtkWindow */ 

#ifdef NspGtkWindow_Private 
static int init_gtkwindow(NspGtkWindow *o,NspTypeGtkWindow *type);
static char *nsp_gtkwindow_type_as_string(void);
static char *nsp_gtkwindow_type_short_string(NspObject *v);
static AttrTab gtkwindow_attrs[];
static NspMethods *gtkwindow_get_methods(void);
/* static int int_gtkwindow_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGtkWindow_Private */
