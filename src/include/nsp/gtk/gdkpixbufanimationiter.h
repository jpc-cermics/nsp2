/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkPixbufAnimationIter
#define NSP_INC_NspGdkPixbufAnimationIter

/*
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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

/* NspGdkPixbufAnimationIter */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkPixbufAnimationIter inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkPixbufAnimationIter ;
typedef NspTypeGObject NspTypeGdkPixbufAnimationIter ;

extern int nsp_type_gdkpixbufanimationiter_id;
extern NspTypeGdkPixbufAnimationIter *nsp_type_gdkpixbufanimationiter;

/* type instances for gobject */

NspTypeGdkPixbufAnimationIter *new_type_gdkpixbufanimationiter(type_mode mode);

/* instance for NspGdkPixbufAnimationIter */

NspGdkPixbufAnimationIter *new_gdkpixbufanimationiter();

/*
 * Object methods redefined for gdkpixbufanimationiter 
 */

#define NULLGDKPIXBUFANIMATIONITER (NspGdkPixbufAnimationIter*) 0


/* from NspGdkPixbufAnimationIterObj.c */

extern NspGdkPixbufAnimationIter *nsp_gdkpixbufanimationiter_object (NspObject *O);
extern int IsGdkPixbufAnimationIterObj (Stack stack, int i);
extern int IsGdkPixbufAnimationIter(NspObject *O);
extern NspGdkPixbufAnimationIter *GetGdkPixbufAnimationIterCopy (Stack stack, int i);
extern NspGdkPixbufAnimationIter *GetGdkPixbufAnimationIter (Stack stack, int i);

#endif /* NSP_INC_NspGdkPixbufAnimationIter */ 

#ifdef NspGdkPixbufAnimationIter_Private 
static int init_gdkpixbufanimationiter(NspGdkPixbufAnimationIter *o,NspTypeGdkPixbufAnimationIter *type);
static char *nsp_gdkpixbufanimationiter_type_as_string(void);
static char *nsp_gdkpixbufanimationiter_type_short_string(NspObject *v);
static AttrTab gdkpixbufanimationiter_attrs[];
static NspMethods *gdkpixbufanimationiter_get_methods(void);
/* static int int_gdkpixbufanimationiter_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkPixbufAnimationIter_Private */
