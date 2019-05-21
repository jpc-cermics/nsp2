/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkDisplay
#define NSP_INC_NspGdkDisplay

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

/* NspGdkDisplay */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkDisplay inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkDisplay ;
typedef NspTypeGObject NspTypeGdkDisplay ;

extern int nsp_type_gdkdisplay_id;
extern NspTypeGdkDisplay *nsp_type_gdkdisplay;

/* type instances for gobject */

NspTypeGdkDisplay *new_type_gdkdisplay(type_mode mode);

/* instance for NspGdkDisplay */

NspGdkDisplay *new_gdkdisplay();

/*
 * Object methods redefined for gdkdisplay 
 */

#define NULLGDKDISPLAY (NspGdkDisplay*) 0


/* from NspGdkDisplayObj.c */

extern NspGdkDisplay *nsp_gdkdisplay_object (NspObject *O);
extern int IsGdkDisplayObj (Stack stack, int i);
extern int IsGdkDisplay(NspObject *O);
extern NspGdkDisplay *GetGdkDisplayCopy (Stack stack, int i);
extern NspGdkDisplay *GetGdkDisplay (Stack stack, int i);

#endif /* NSP_INC_NspGdkDisplay */ 

#ifdef NspGdkDisplay_Private 
static int init_gdkdisplay(NspGdkDisplay *o,NspTypeGdkDisplay *type);
static char *nsp_gdkdisplay_type_as_string(void);
static char *nsp_gdkdisplay_type_short_string(NspObject *v);
static AttrTab gdkdisplay_attrs[];
static NspMethods *gdkdisplay_get_methods(void);
/* static int int_gdkdisplay_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkDisplay_Private */
