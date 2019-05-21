/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkPixbuf
#define NSP_INC_NspGdkPixbuf

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

/* NspGdkPixbuf */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkPixbuf inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkPixbuf ;
typedef NspTypeGObject NspTypeGdkPixbuf ;

extern int nsp_type_gdkpixbuf_id;
extern NspTypeGdkPixbuf *nsp_type_gdkpixbuf;

/* type instances for gobject */

NspTypeGdkPixbuf *new_type_gdkpixbuf(type_mode mode);

/* instance for NspGdkPixbuf */

NspGdkPixbuf *new_gdkpixbuf();

/*
 * Object methods redefined for gdkpixbuf 
 */

#define NULLGDKPIXBUF (NspGdkPixbuf*) 0


/* from NspGdkPixbufObj.c */

extern NspGdkPixbuf *nsp_gdkpixbuf_object (NspObject *O);
extern int IsGdkPixbufObj (Stack stack, int i);
extern int IsGdkPixbuf(NspObject *O);
extern NspGdkPixbuf *GetGdkPixbufCopy (Stack stack, int i);
extern NspGdkPixbuf *GetGdkPixbuf (Stack stack, int i);

#endif /* NSP_INC_NspGdkPixbuf */ 

#ifdef NspGdkPixbuf_Private 
static int init_gdkpixbuf(NspGdkPixbuf *o,NspTypeGdkPixbuf *type);
static char *nsp_gdkpixbuf_type_as_string(void);
static char *nsp_gdkpixbuf_type_short_string(NspObject *v);
static AttrTab gdkpixbuf_attrs[];
static NspMethods *gdkpixbuf_get_methods(void);
/* static int int_gdkpixbuf_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkPixbuf_Private */
