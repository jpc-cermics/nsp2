/* -*- Mode: C -*- */
#ifndef NSP_INC_NspGdkWindowImpl
#define NSP_INC_NspGdkWindowImpl

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

/* NspGdkWindowImpl */

#include <nsp/gtk/gobject.h>

/*
 * NspGdkWindowImpl inherits from GObject
 * just change some type attributes 
 */

typedef NspGObject NspGdkWindowImpl ;
typedef NspTypeGObject NspTypeGdkWindowImpl ;

extern int nsp_type_gdkwindowimpl_id;
extern NspTypeGdkWindowImpl *nsp_type_gdkwindowimpl;

/* type instances for gobject */

NspTypeGdkWindowImpl *new_type_gdkwindowimpl(type_mode mode);

/* instance for NspGdkWindowImpl */

NspGdkWindowImpl *new_gdkwindowimpl();

/*
 * Object methods redefined for gdkwindowimpl 
 */

#define NULLGDKWINDOWIMPL (NspGdkWindowImpl*) 0


/* from NspGdkWindowImplObj.c */

extern NspGdkWindowImpl *nsp_gdkwindowimpl_object (NspObject *O);
extern int IsGdkWindowImplObj (Stack stack, int i);
extern int IsGdkWindowImpl(NspObject *O);
extern NspGdkWindowImpl *GetGdkWindowImplCopy (Stack stack, int i);
extern NspGdkWindowImpl *GetGdkWindowImpl (Stack stack, int i);

#endif /* NSP_INC_NspGdkWindowImpl */ 

#ifdef NspGdkWindowImpl_Private 
static int init_gdkwindowimpl(NspGdkWindowImpl *o,NspTypeGdkWindowImpl *type);
static char *nsp_gdkwindowimpl_type_as_string(void);
static char *nsp_gdkwindowimpl_type_short_string(NspObject *v);
static AttrTab gdkwindowimpl_attrs[];
static NspMethods *gdkwindowimpl_get_methods(void);
/* static int int_gdkwindowimpl_create(Stack stack, int rhs, int opt, int lhs);*/ 
#endif /* NspGdkWindowImpl_Private */
