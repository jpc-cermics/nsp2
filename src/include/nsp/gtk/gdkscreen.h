/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkScreen
#define NSP_INC_NspGdkScreen

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

/* NspGdkScreen */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkScreen inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkScreen ;
typedef NspTypeGObject NspTypeGdkScreen ;

extern int nsp_type_gdkscreen_id;
extern NspTypeGdkScreen *nsp_type_gdkscreen;

/* type instances for gobject */

NspTypeGdkScreen *new_type_gdkscreen(type_mode mode);

/* instance for NspGdkScreen */

NspGdkScreen *new_gdkscreen();

/*
 * Object methods redefined for gdkscreen 
 */

#define NULLGDKSCREEN (NspGdkScreen*) 0


/* from NspGdkScreenObj.c */

extern NspGdkScreen *nsp_gdkscreen_object (NspObject *O);
extern int IsGdkScreenObj (Stack stack, int i);
extern int IsGdkScreen(NspObject *O);
extern NspGdkScreen *GetGdkScreenCopy (Stack stack, int i);
extern NspGdkScreen *GetGdkScreen (Stack stack, int i);

#endif /* NSP_INC_NspGdkScreen */ 

#ifdef NspGdkScreen_Private 
static int init_gdkscreen(NspGdkScreen *o,NspTypeGdkScreen *type);
static char *nsp_gdkscreen_type_as_string(void);
static char *nsp_gdkscreen_type_short_string(NspObject *v);
static AttrTab gdkscreen_attrs[];
static NspMethods *gdkscreen_get_methods(void);
/* static int int_gdkscreen_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkScreen_Private */
