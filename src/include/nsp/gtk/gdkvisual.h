/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkVisual
#define NSP_INC_NspGdkVisual

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

/* NspGdkVisual */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkVisual inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkVisual ;
typedef NspTypeGObject NspTypeGdkVisual ;

extern int nsp_type_gdkvisual_id;
extern NspTypeGdkVisual *nsp_type_gdkvisual;

/* type instances for gobject */

NspTypeGdkVisual *new_type_gdkvisual(type_mode mode);

/* instance for NspGdkVisual */

NspGdkVisual *new_gdkvisual();

/*
 * Object methods redefined for gdkvisual 
 */

#define NULLGDKVISUAL (NspGdkVisual*) 0


/* from NspGdkVisualObj.c */

extern NspGdkVisual *nsp_gdkvisual_object (NspObject *O);
extern int IsGdkVisualObj (Stack stack, int i);
extern int IsGdkVisual(NspObject *O);
extern NspGdkVisual *GetGdkVisualCopy (Stack stack, int i);
extern NspGdkVisual *GetGdkVisual (Stack stack, int i);

#endif /* NSP_INC_NspGdkVisual */ 

#ifdef NspGdkVisual_Private 
static int init_gdkvisual(NspGdkVisual *o,NspTypeGdkVisual *type);
static char *nsp_gdkvisual_type_as_string(void);
static char *nsp_gdkvisual_type_short_string(NspObject *v);
static AttrTab gdkvisual_attrs[];
static NspMethods *gdkvisual_get_methods(void);
/* static int int_gdkvisual_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkVisual_Private */
