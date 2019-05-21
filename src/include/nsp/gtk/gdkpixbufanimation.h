/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkPixbufAnimation
#define NSP_INC_NspGdkPixbufAnimation

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

/* NspGdkPixbufAnimation */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkPixbufAnimation inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkPixbufAnimation ;
typedef NspTypeGObject NspTypeGdkPixbufAnimation ;

extern int nsp_type_gdkpixbufanimation_id;
extern NspTypeGdkPixbufAnimation *nsp_type_gdkpixbufanimation;

/* type instances for gobject */

NspTypeGdkPixbufAnimation *new_type_gdkpixbufanimation(type_mode mode);

/* instance for NspGdkPixbufAnimation */

NspGdkPixbufAnimation *new_gdkpixbufanimation();

/*
 * Object methods redefined for gdkpixbufanimation 
 */

#define NULLGDKPIXBUFANIMATION (NspGdkPixbufAnimation*) 0


/* from NspGdkPixbufAnimationObj.c */

extern NspGdkPixbufAnimation *nsp_gdkpixbufanimation_object (NspObject *O);
extern int IsGdkPixbufAnimationObj (Stack stack, int i);
extern int IsGdkPixbufAnimation(NspObject *O);
extern NspGdkPixbufAnimation *GetGdkPixbufAnimationCopy (Stack stack, int i);
extern NspGdkPixbufAnimation *GetGdkPixbufAnimation (Stack stack, int i);

#endif /* NSP_INC_NspGdkPixbufAnimation */ 

#ifdef NspGdkPixbufAnimation_Private 
static int init_gdkpixbufanimation(NspGdkPixbufAnimation *o,NspTypeGdkPixbufAnimation *type);
static char *nsp_gdkpixbufanimation_type_as_string(void);
static char *nsp_gdkpixbufanimation_type_short_string(NspObject *v);
static AttrTab gdkpixbufanimation_attrs[];
static NspMethods *gdkpixbufanimation_get_methods(void);
/* static int int_gdkpixbufanimation_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkPixbufAnimation_Private */
