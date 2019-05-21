/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkWindow
#define NSP_INC_NspGdkWindow

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

/* NspGdkWindow */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkWindow inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkWindow ;
typedef NspTypeGObject NspTypeGdkWindow ;

extern int nsp_type_gdkwindow_id;
extern NspTypeGdkWindow *nsp_type_gdkwindow;

/* type instances for gobject */

NspTypeGdkWindow *new_type_gdkwindow(type_mode mode);

/* instance for NspGdkWindow */

NspGdkWindow *new_gdkwindow();

/*
 * Object methods redefined for gdkwindow 
 */

#define NULLGDKWINDOW (NspGdkWindow*) 0


/* from NspGdkWindowObj.c */

extern NspGdkWindow *nsp_gdkwindow_object (NspObject *O);
extern int IsGdkWindowObj (Stack stack, int i);
extern int IsGdkWindow(NspObject *O);
extern NspGdkWindow *GetGdkWindowCopy (Stack stack, int i);
extern NspGdkWindow *GetGdkWindow (Stack stack, int i);

#endif /* NSP_INC_NspGdkWindow */ 

#ifdef NspGdkWindow_Private 
static int init_gdkwindow(NspGdkWindow *o,NspTypeGdkWindow *type);
static char *nsp_gdkwindow_type_as_string(void);
static char *nsp_gdkwindow_type_short_string(NspObject *v);
static AttrTab gdkwindow_attrs[];
static NspMethods *gdkwindow_get_methods(void);
/* static int int_gdkwindow_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkWindow_Private */
